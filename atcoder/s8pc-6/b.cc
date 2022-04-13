// AtCoder Market
// https://atcoder.jp/contests/s8pc-6/tasks/s8pc_6_b
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
long a[50];
long b[50];

int main() {
    cin >> n;
    rep(i,n) {
        cin >> a[i] >> b[i];
        a[i]--; b[i]--;
    }
    long res = 1L<<60;
    rep(i,n) {
        rep(j,n) {
            long ent = a[i];
            long ext = b[j];
            long dist = 0;
            rep(k,n) {
                dist += b[k]-a[k];
                dist += abs(ent-a[k]);
                dist += abs(b[k]-ext);
            }
            /* cout << "ent,ext,dist: " << ent << " " << ext << " " << dist << endl; */
            res = min(res,dist);
        }
    }
    cout << res << endl;
}
