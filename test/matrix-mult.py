N = 200

a = [[0 for i in range(N)] for j in range(N)]
b = [[0 for i in range(N)] for j in range(N)]
c = [[0 for i in range(N)] for j in range(N)]

for i in range(N):
  for j in range(N):
    a[i][j] = i if i == j else 0
    b[i][j] = (N * i) + j

for i in range(N):
  for j in range(N):
    for k in range(N):
      c[i][j] += a[i][k] * b[k][j]
