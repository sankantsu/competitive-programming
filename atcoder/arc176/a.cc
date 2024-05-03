#include <iostream>
#include <vector>
#include <set>
#include <queue>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n, m;
    cin >> n >> m;

    vector<int> a(m), b(m);
    rep(i,m) {
        cin >> a[i] >> b[i];
        a[i]--; b[i]--;
    }

    using P = pair<int, int>;
    set<P> s;
    rep(i,m) {
        s.emplace(a[i], b[i]);
    }

    vector<int> rows(n), cols(n);
    rep(i,m) {
        rows[a[i]]++;
        cols[b[i]]++;
    }

    using E = pair<int, size_t>;
    priority_queue<E, vector<E>, greater<E>> queue;
    rep(j,n) {
        if (cols[j] < m) queue.emplace(cols[j], j);
    }

    vector<int> idx(n);
    rep(i,n) idx[i] = i;
    sort(idx.begin(), idx.end(), [&](int i, int j){
        return rows[i] > rows[j];
    });

    set<P> ans;
    for (auto p : s) ans.insert(p);
    for (auto i : idx) {
        set<E> tmp;
        while (rows[i] < m) {
            if (queue.empty()) {
                assert(false);
                break;
            }
            auto [cnt, j] = queue.top();
            /* cerr << "i,j,cnt: " << i << " " << j << " " << cnt << endl; */
            queue.pop();
            if (ans.count(make_pair(i, j))) {
                tmp.emplace(cnt, j);
                continue;
            }
            ans.emplace(i, j);
            rows[i]++;
            if (cnt+1 < m) queue.emplace(cnt+1, j);
        }
        for (auto [cnt, j] : tmp) queue.emplace(cnt, j);
    }

    // debug
    /* vector<vector<int>> board(n, vector<int>(n)); */
    /* for (auto [i,j] : ans) board[i][j] = 1; */
    /* rep(i,n) { */
    /*     rep(j,n) cerr << board[i][j] << " "; */
    /*     cerr << endl; */
    /* } */

    cout << ans.size() << endl;
    for (auto [i,j] : ans) {
        cout << i+1 << " " << j+1 << endl;
    }
}
