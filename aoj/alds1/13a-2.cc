// 8 Queens Problem (reduce code size)
// https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=ALDS1_13_A&lang=ja
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int n = 8;

int k;
int r[n];
int c[n];

int main() {
    cin >> k;
    rep(i,k) cin >> r[i] >> c[i];
    vector<int> v(n);
    rep(i,n) v[i] = i;
    do {
        rep(i,k) if (v[r[i]] != c[i]) goto NEXT;
        rep(i,n) {
            for (int j = i+1; j < n; j++) {
                if (abs(i-j) == abs(v[i]-v[j])) {
                    goto NEXT;
                }
            }
        }
        break;
NEXT:
        ;
    } while (next_permutation(v.begin(),v.end()));

    rep(i,n) {
        rep(j,n) {
            if (j == v[i]) cout << 'Q';
            else cout << '.';
        }
        cout << endl;
    }
}
