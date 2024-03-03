from functools import cache


@cache
def f(n):
    if (n == 1):
        return 0
    return n + f(n//2) + f(n - n//2)


n = int(input())
print(f(n))
