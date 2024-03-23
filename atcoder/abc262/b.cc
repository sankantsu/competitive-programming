#include <iostream>
#include <vector>
#include <set>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, m;
    cin >> n >> m;

    vector<set<long>> g(n);
    rep(i,m) {
        long u, v;
        cin >> u >> v;
        u--; v--;
        g[u].insert(v);
        g[v].insert(u);
    }

    long ans = 0;
    rep(a,n) for (long b = a+1; b < n; b++) for (long c = b+1; c < n; c++) {
        if (g[a].contains(b) && g[b].contains(c) && g[c].contains(a)) {
            ans++;
        }
    }
    cout << ans << endl;
}
