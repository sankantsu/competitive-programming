// 8 Queens Problem
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

int x[n];
int y[n];

bool attack(int i, int j) {
    bool res = false;
    if (x[i] == x[j]) res = true;
    if (y[i] == y[j]) res = true;
    if (abs(x[i]-x[j]) == abs(y[i]-y[j])) res = true;
    return res;
}

int main() {
    cin >> k;
    rep(i,k) cin >> r[i] >> c[i];

    vector<int> v(n);
    rep(i,n) v[i] = i;
    do {
        rep(i,k) {
            if (v[r[i]] != c[i]) {
                goto NEXT;
            }
        }
        rep(i,n) {
            x[i] = i;
            y[i] = v[i];
        }
        rep(i,n) {
            for (int j = i+1; j < n; j++) {
                if (attack(i,j)) {
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
            if (j == y[i]) cout << 'Q';
            else cout << '.';
        }
        cout << endl;
    }
}
