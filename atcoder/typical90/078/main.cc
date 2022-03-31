#include <iostream>
#include <vector>

using namespace std;

int n, m;
vector<int> g[100000];

int main() {
    cin >> n >> m;
    for (int i = 0; i < m; i++) {
        int u,v;
        cin >> u >> v; u--; v--;
        g[u].push_back(v);
        g[v].push_back(u);
    }
    int ans = 0;
    for (int i = 0; i < n; i++) {
        int cnt = 0;
        for (auto adj : g[i]) {
            if (adj < i) {
                cnt++;
            }
        }
        if (cnt == 1) {
            ans++;
        }
    }
    cout << ans << endl;
}
