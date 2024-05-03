#include <iostream>
#include <vector>
#include <queue>
#include <set>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, m;
    cin >> n >> m;

    vector<vector<long>> g(n);
    rep(i,m) {
        long a, b;
        cin >> a >> b;
        a--; b--;
        g[a].push_back(b);
        g[b].push_back(a);
    }

    long ans = -m;
    vector<bool> visited(n);
    rep(i,n) {
        if (visited[i]) continue;
        long s = 0;
        using P = pair<int, int>;
        queue<long> q;
        q.push(i);
        visited[i] = true;
        while (!q.empty()) {
            long u = q.front();
            q.pop();
            s++;
            for (auto v : g[u]) {
                if (visited[v]) continue;
                q.push(v);
                visited[v] = true;
            }
        }
        long k = s*(s - 1)/2;
        ans += k;
    }
    cout << ans << endl;
}
