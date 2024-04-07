#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

vector<vector<long>> g;

long dfs(long u, long p, const vector<long>& c, vector<long>& sum) {
    long s = c[u];
    for (auto v : g[u]) {
        if (v == p) continue;
        s += dfs(v, u, c, sum);
    }
    sum[u] = s;
    return s;
}

long f(long u, long p, long depth, const vector<long>& c) {
    long acc = depth * c[u];
    for (auto v : g[u]) {
        if (v == p) continue;
        acc += f(v, u, depth+1, c);
    }
    return acc;
}

void f2(long u, long p, const vector<long>& sum, vector<long>& score) {
    for (auto v : g[u]) {
        if (v == p) continue;
        score[v] = score[u] - sum[v] + (sum[0] - sum[v]);
        f2(v, u, sum, score);
    }
}

int main() {
    long n;
    cin >> n;

    g.resize(n);
    rep(i,n-1) {
        long a, b;
        cin >> a >> b;
        a--; b--;
        g[a].push_back(b);
        g[b].push_back(a);
    }

    vector<long> c(n);
    rep(i,n) cin >> c[i];

    vector<long> sum(n);
    dfs(0, -1, c, sum);
    vector<long> score(n);
    score[0] = f(0, -1, 0, c);
    f2(0, -1, sum, score);

    long ans = 1L<<62;
    rep(i,n) {
        cerr << "i, score[i]: " << i << " " << score[i] << endl;
        ans = min(ans, score[i]);
    }
    cout << ans << endl;
}
