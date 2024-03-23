#include <iostream>
#include <vector>
#include <algorithm>
#include <random>
#include <set>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long n, m;
long a[200000];

using Edge = pair<long, long>;
set<Edge> edges;
vector<long> g[200000];

bool used[200000];

long dfs(long u, long score) {
    if (u == n -1) {
        return score;
    }
    long max_score = 0;
    for (auto v : g[u]) {
        if (used[v]) continue;
        used[v] = true;
        long ns = (a[u] == a[v]) ? score : score + 1;
        long sc = dfs(v, ns);
        max_score = max(max_score, sc);
        used[v] = false;
    }
    return max_score;
}

long solve_jury() {
    rep(i,n) used[i] = false;
    used[0] = true;
    long score = dfs(0, 1);
    return score;
}

long solve() {
    vector<long> dp(n);
    dp[0] = 1;

    using State = pair<long, size_t>;  // current value, node id
    priority_queue<State> q;
    q.emplace(-a[0], 0);
    while (!q.empty()) {
        auto [_, u] = q.top(); q.pop();
        /* cerr << "u, dp[u]: " << u << " " << dp[u] << endl; */
        for (auto v : g[u]) {
            long score = (a[u] == a[v]) ? dp[u] : dp[u] + 1;
            if (score > dp[v]) {
                dp[v] = score;
                q.emplace(-a[v], v);
            }
        }
    }
    return dp[n-1];
}

void gen_random() {
    static mt19937 mt;
    n = 5;
    m = 6;
    rep(i,n) {
        a[i] = 1 + mt()%10;
    }

    rep(i,n) g[i].clear();
    long k = 0;

    edges.clear();
    while (k < m) {
        long u = mt()%n;
        long v = mt()%n;
        if (u == v) continue;
        if (a[u] > a[v]) swap(u,v);
        if (edges.contains(make_pair(u,v))) continue;
        edges.emplace(u,v);
        g[u].push_back(v);
        if (a[u] == a[v]) g[v].push_back(u);
        k++;
    }
}

void test() {
    long n_case = 1000;
    rep(i,n_case) {
        gen_random();
        long ans1 = solve_jury();
        long ans2 = solve();
        if (ans1 != ans2) {
            cerr << "Wrong answer!" << endl;
            cerr << "expected: " << ans1 << endl;
            cerr << "actual: " << ans2 << endl;
            // print testcase
            cout << n << " " << m << endl;
            rep(i,n) cout << a[i] << " "; cout << endl;
            for (auto [u, v] : edges) {
                cout << u+1 << " " << v+1 << endl;
            }
            exit(1);
        }
    }
    cerr << "All test passed!" << endl;
    exit(0);
}

int main() {
    /* test(); */

    cin >> n >> m;

    rep(i,n) cin >> a[i];

    rep(i,m) {
        long u, v;
        cin >> u >> v;
        u--; v--;
        if (a[u] > a[v]) swap(u,v);
        g[u].push_back(v);
        if (a[u] == a[v]) g[v].push_back(u);
    }

    long ans = solve();
    cout << ans << endl;
}
