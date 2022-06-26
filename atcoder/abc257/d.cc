#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
long x[300];
long y[300];
long p[300];

bool check(long c) {
    vector<int> g[300];
    rep(i,n) {
        rep(j,n) {
            if (i == j) continue;
            long d = abs(x[i]-x[j]) + abs(y[i]-y[j]);
            if (c*p[i] >= d) {
                g[i].push_back(j);
            }
        }
    }
    auto is_connected = [&] {
        int res = 0;
        rep(start,n) {
            vector<bool> used(n,false);
            queue<int> q;
            int cnt = 0;
            q.push(start);
            while(!q.empty()) {
                int v = q.front(); q.pop();
                if (used[v]) continue;
                used[v] = true;
                cnt++;
                for (auto u : g[v]) {
                    q.push(u);
                }
            }
            res = max(res,cnt);
        }
        if (res == n) {
            return true;
        }
        else {
            return false;
        }
    };
    return is_connected();
}

int main() {
    cin >> n;
    rep(i,n) cin >> x[i] >> y[i] >> p[i];

    long ng = 0;
    long ok = 5L*(1L<<30);
    while (ok - ng > 1) {
        long c = (ng+ok)/2;
        if (check(c)) {
            ok = c;
        }
        else {
            ng = c;
        }
    }
    cout << ok << endl;
}
