#include <iostream>
#include <set>
#include <map>
#include <atcoder/fenwicktree.hpp>

using atcoder::fenwick_tree;

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    set<long> s;
    rep(i,n) s.insert(a[i]);

    map<long, int> comp;
    {
        int i = 0;
        for (auto x : s) {
            comp[x] = i++;
        }
    }

    fenwick_tree<int> fw_cnt(n);
    fenwick_tree<long> fw_sum(n);

    long ans = 0;
    rep(i,n) {
        long x = a[i];
        int idx = comp[x];

        int cnt = fw_cnt.sum(0, idx);
        long sum = fw_sum.sum(0, idx);
        ans += x*cnt - sum;

        fw_cnt.add(idx, 1);
        fw_sum.add(idx, x);
    }
    cout << ans << endl;
}
