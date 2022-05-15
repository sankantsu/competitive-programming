n = int(input())

res = -1
max_score = -1

dic = {}
for i in range(n):
    s,t = input().split()
    t = int(t)
    if s in dic:
        continue
    dic[s] = t
    if t > max_score:
        res = i+1
        max_score = t

print(res)
