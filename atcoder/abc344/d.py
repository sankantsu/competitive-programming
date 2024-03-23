import sys
import random
import itertools
import functools

inf = 100000000


def solve(t, n, s):
    m = len(t)
    dp = [inf] * (m + 1)
    dp[0] = 0

    for k in range(n):
        # print(dp)
        dp2 = [inf] * (m + 1)
        for ss in s[k]:
            k = len(ss)
            cost = 1 if k > 0 else 0
            for i in range(m+1):
                if i + k > m:
                    continue
                tt = t[i:i + k]
                if ss == tt:
                    dp2[i + k] = min(dp2[i + k], dp[i] + cost)
        dp = dp2
    # print(dp)
    if dp[m] == inf:
        return -1
    else:
        return dp[m]


def solve_jury(t, n, s):
    def calc_cost(s):
        return int(bool(s))
    ans = inf
    for prod in itertools.product(*s):
        cs = map(calc_cost, prod)
        cost = functools.reduce(lambda x, y: x + y, cs)
        ss = functools.reduce(lambda x, y: x + y, prod)
        if (t == ss):
            ans = min(ans, cost)
    if (ans == inf):
        return -1
    else:
        return ans


def gen_random():
    def random_alph():
        return chr(ord('a') + random.randrange(26))
    m = 3
    n = 5
    n_cand = 3
    t = ""
    for i in range(m):
        t += random_alph()
    s = []
    for k in range(n):
        ent = [""]
        for _ in range(n_cand):
            len_sub = random.randint(1, 3)
            sub = ""
            for _ in range(len_sub):
                sub = sub + random_alph()
            ent.append(sub)
        s.append(ent)
    return t, n, s


def test():
    n_test = 10000
    for i in range(n_test):
        if (i % 1000 == 0):
            print(f"Tested {i} samples...")
        t, n, s = gen_random()
        ans_jury = solve_jury(t, n, s)
        ans = solve(t, n, s)
        if (ans_jury != ans):
            print("expected:", ans_jury, file=sys.stderr)
            print("actual:", ans, file=sys.stderr)
            print(t)
            print(n)
            for ent in s:
                line = " ".join(ent)
                line = str(len(ent) - 1) + line
                print(line)
            sys.exit(1)
    print("All test passed!!")
    sys.exit(0)


if __name__ == "__main__":
    # test()

    t = input().strip()
    n = int(input())
    s = []

    for k in range(n):
        s.append(input().split())
        s[k][0] = ""

    ans = solve(t, n, s)
    print(ans)
