// F - Surrounded Nodes
// https://atcoder.jp/contests/abc149/tasks/abc149_f

#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_n = 200000;
const long mod = 1000000007;

int n;
vector<int> g[max_n+10];

long mod_pow(long a, long b) {
    long res = 1;
    long p = a;
    while (b > 0) {
        if (b&1) {
            res = (res*p)%mod;
        }
        p = (p*p)%mod;
        b >>= 1;
    }
    return res;
}

long inv(long a) {
    return mod_pow(a,mod-2);
}

int dp[max_n+10];
long prob[max_n+10];

// 部分木のサイズ
int size(int v, int par) {
    if (dp[v] != 0) {
        return dp[v];
    }
    dp[v] = 1;
    for (auto u : g[v]) {
        if (u == par) continue;
        dp[v] += size(u,v);
    }
    return dp[v];
}

void calc_prob(int v, int par) {
    long sum = 0;
    vector<long> vs;
    for (auto u : g[v]) {
        if (u == par) continue;
        calc_prob(u,v);
        long s = size(u,v);
        sum += s;
        vs.push_back(s);
    }
    if (par != -1) vs.push_back(n-sum-1); // parent
    if (vs.size() == 1) {
        prob[v] = 0;
    }
    else {
        long prod = 1;
        long sum = 1;
        for (auto s : vs) {
            long p1 = mod_pow(inv(2),s);
            long p2 = mod_pow(2,s);
            prod = (prod*p1)%mod;
            sum += ((1-p1+mod)*p2)%mod;
        }
        long q = (prod*sum)%mod;
        long p = (1-q+mod)%mod;
        p = (inv(2)*p)%mod;
        prob[v] = p;
    }
}

int main() {
    cin >> n;
    rep(i,n-1) {
        int a,b;
        cin >> a >> b; a--; b--;
        g[a].push_back(b);
        g[b].push_back(a);
    }

    calc_prob(0,-1);
    long res = 0;
    rep(i,n) {
        res = (res+prob[i])%mod;
    }
    cout << res << endl;
}
