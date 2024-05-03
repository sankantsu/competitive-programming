#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>
#include <set>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, m, l;
    cin >> n >> m >> l;

    vector<long> a(n), b(m);
    rep(i,n) cin >> a[i];
    rep(i,m) cin >> b[i];

    vector<size_t> idx(m);
    rep(i,m) idx[i] = i;
    sort(idx.begin(), idx.end(), [&](size_t i, size_t j) { return b[i] > b[j]; });

    using P = pair<long, long>;  // main, sub
    set<P> ng;
    rep(i,l) {
        long c, d;
        cin >> c >> d;
        c--; d--;
        ng.emplace(c, d);
    }

    vector<long> sub(n, 0);  // (i, sub[i]) pair is in queue
    using S = pair<long, long>;  // cost, main
    priority_queue<S> q;
    rep(i,n) q.emplace(a[i] + b[idx[sub[i]]], i);

    long ans;
    while (!q.empty()) {
        auto [cost, i] = q.top(); q.pop();
        /* cerr << "i,j,cost: " << i << " " << idx[sub[i]] << " " << cost << endl; */
        auto p = make_pair(i, idx[sub[i]]);
        if (!ng.count(p)) {
            ans = cost;
            break;
        }
        sub[i]++;
        q.emplace(a[i] + b[idx[sub[i]]], i);
    }
    cout << ans << endl;
}
