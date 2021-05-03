#! /usr/bin/python3

import functools
import json
import multiprocess
import subprocess
import sys

N = 8
return_key = '*retval'

def curry(f):
  def g(x):
    def h(y):
      return f(x, y)
    return h
  return g

default_state = {
  'neg': lambda x: -x,
  'not': lambda b: not b,
  'and': curry(lambda b, c: b and c),
  'or': curry(lambda b, c: b or c),
  'add': curry(lambda x, y: x + y),
  'sub': curry(lambda x, y: x - y),
  'mul': curry(lambda x, y: x * y),
  'div': curry(lambda x, y: x // y),
  'mod': curry(lambda x, y: x % y),
  'lt': curry(lambda x, y: x < y),
  'le': curry(lambda x, y: x <= y),
  'gt': curry(lambda x, y: x > y),
  'ge': curry(lambda x, y: x >= y),
  'eq': curry(lambda x, y: x == y),
  'neq': curry(lambda x, y: x != y),
  'scan': lambda x: int(input()),
  'print': print,
  'alloc': lambda n: [None for i in range(n)],
}

def chunked(indices, n):
  size = len(indices) // n
  extra = len(indices) % n
  chunks = []
  cur = 0
  while len(chunks) < n:
    end = cur + size
    if extra > 0:
      end += 1
      extra -= 1
    chunks.append(indices[cur : end])
    cur = end
  return chunks

def eval_expr(e, state):
  if e['kind'] == 'unit':
    return None
  elif e['kind'] == 'int' or e['kind'] == 'bool':
    return e['value']
  elif e['kind'] == 'sub':
    e1 = eval_expr(e['e1'], state)
    e2 = eval_expr(e['e2'], state)
    try:
      return e1[e2]
    except:
      raise Exception('array index out of bounds: %d' % e2) from None
  elif e['kind'] == 'var':
    return state[e['name']]
  elif e['kind'] == 'apply':
    e1 = eval_expr(e['e1'], state)
    e2 = eval_expr(e['e2'], state)
    if callable(e1):
      return e1(e2)
    elif e1['kind'] == 'lambda':
      state[e1['arg']] = e2
      state[return_key] = None
      eval_stmt(e1['body'], state)
      return state[return_key]
  else:
    return e

def eval_stmt(s, state):
  if s['kind'] == 'assign':
    e = eval_expr(s['expr'], state)
    a = s['lhs']
    indices = []
    while a['kind'] == 'sub':
      indices.append(a['index'])
      a = a['base']
    if a['name'] == '_':
      pass
    elif len(indices) == 0:
      state[a['name']] = e
    else:
      array = state[a['name']]
      for i in range(len(indices) - 1, -1, -1):
        index = eval_expr(indices[i], state)
        try:
          if i > 0:
            array = array[index]
          else:
            array[index] = e
        except:
          raise Exception('array index out of bounds: %d' % index) from None
  elif s['kind'] == 'if':
    eval_stmt(s['true'] if eval_expr(s['guard'], state) else s['false'], state)
  elif s['kind'] == 'while':
    while eval_expr(s['guard'], state):
      eval_stmt(s['body'], state)
  elif s['kind'] == 'for':
    start = eval_expr(s['start'], state)
    end = eval_expr(s['end'], state)
    step = eval_expr(s['step'], state)
    for i in range(start, end, step):
      state[s['name']] = i
      eval_stmt(s['body'], state)
  elif s['kind'] == 'cfor':
    def func(indices):
      for i in indices:
        state[s['name']] = i
        eval_stmt(s['body'], state)
      return state
    start = eval_expr(s['start'], state)
    end = eval_expr(s['end'], state)
    step = eval_expr(s['step'], state)
    indices = range(start, end, step)
    with multiprocess.Pool(N) as p:
      for p_state in p.imap_unordered(func, chunked(indices, N)):
        for acc_f in s['acc_fs']:
          acc = acc_f[0]
          f = acc_f[1]
          state[acc] = state[f](state[acc])(p_state[acc])
  elif s['kind'] == 'seq':
    for s1 in s['body']:
      eval_stmt(s1, state)
  elif s['kind'] == 'return':
    state[return_key] = eval_expr(s['expr'], state)

if __name__ == '__main__':
  eval_stmt(json.loads(sys.stdin.read()), default_state)
