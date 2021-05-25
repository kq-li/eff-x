import multiprocess

N = 8

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

def f(indices):
  x = 0
  for i in indices:
    x += i
  return x

with multiprocess.Pool(N) as p:
  chunks = chunked(range(5000000), N)
  x = 0
  for xi in p.map(f, chunks):
    x += xi
  print(x)
