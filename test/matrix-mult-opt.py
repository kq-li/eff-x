import multiprocess

N = 200

a = [[0 for i in range(N)] for j in range(N)]
b = [[0 for i in range(N)] for j in range(N)]
c = [[0 for i in range(N)] for j in range(N)]

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

def init(indices):
  for i in indices:
    for j in range(N):
      a[i][j] = i if i == j else 0
      b[i][j] = (N * i) + j
  return a, b

def mult(indices):
  for i in indices:
    for j in range(N):
      for k in range(N):
        c[i][j] += a[i][k] * b[k][j]
  return c

chunks = chunked(range(N), 8)
with multiprocess.Pool(N) as p:
  results = p.map(init, chunks)
  for k in range(len(results)):
    a_k, b_k = results[k]
    for i in chunks[k]:
      for j in range(N):
        a[i][j] = a_k[i][j]
        b[i][j] = b_k[i][j]

with multiprocess.Pool(N) as p:
  cs = p.map(mult, chunks)
  for k in range(len(cs)):
    for i in chunks[k]:
      for j in range(N):
        c[i][j] = cs[k][i][j]

          
