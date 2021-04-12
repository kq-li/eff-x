#! /usr/bin/python3

import functools
import json
import multiprocess
import subprocess
import sys

N = 4
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
}

manager = multiprocess.Manager()

def eval_expr(e, state):
  if e['kind'] == 'unit':
    return None
  elif e['kind'] == 'int' or e['kind'] == 'bool':
    return e['value']
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
    if s['var'] != '_':
      state[s['var']] = e
  elif s['kind'] == 'if':
    eval_stmt(s['true'] if eval_expr(s['guard'], state) else s['false'], state)
  elif s['kind'] == 'while':
    while eval_expr(s['guard'], state):
      eval_stmt(s['body'], state)
  elif s['kind'] == 'for':
    for i in range(s['start'], s['end'] + 1):
      state[s['name']] = i
      eval_stmt(s['body'], state)
  elif s['kind'] == 'cfor':
    shared = manager.dict(state)
    lock = manager.Lock()
    def func(i):
      with lock:
        shared[s['name']] = i
        eval_stmt(s['body'], shared)
    with multiprocess.Pool(N) as p:
      for i in p.map(func, range(s['start'], s['end'] + 1)):
        pass
    state.update(shared)
  elif s['kind'] == 'seq':
    for s1 in s['body']:
      eval_stmt(s1, state)
  elif s['kind'] == 'return':
    state[return_key] = eval_expr(s['expr'], state)

if __name__ == '__main__':
  eval_stmt(json.loads(sys.stdin.read()), default_state)
