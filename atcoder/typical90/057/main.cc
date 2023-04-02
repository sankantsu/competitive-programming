#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const long mod = 998244353;

int n,m;
int A[300][300];
int S[300];

long mod_pow(int a, int b) {
    if (b <= 0) return 1;
    return (a*mod_pow(a,b-1))%mod;
}

void gauss() {
    int col = 0;
    auto swap_row = [](int i, int j) {
        vector<int> tmp(m);
        rep(k,m) tmp[k] = A[i][k];
        rep(k,m) A[i][k] = A[j][k];
        rep(k,m) A[j][k] = tmp[k];
    };
    auto set_row = [&](int i) {
        while (col < m) {
            int i2 = -1;
            for (int j = i; j < n; j++) {
                if (A[j][col] == 1) {
                    i2 = j;
                    break;
                }
            }
            if (i2 == -1) {
                col++;
                continue;
            }
            else {
                swap_row(i,i2);
                return true;
            }
        }
        return false;
    };
    for (int i = 0; i < n; i++) {
        if(!set_row(i)) break;
        for (int j = i+1; j < n; j++) {
            if (A[j][col] == 1) {
                rep(k,m) A[j][k] = A[j][k]^A[i][k];
            }
        }
    }
}

int main() {
    cin >> n >> m;
    rep(i,n) {
        int t;
        cin >> t;
        rep(k,t) {
            int j;
            cin >> j; j--;
            A[i][j] = 1;
        }
    }
    rep(i,m) cin >> S[i];

    gauss();

    vector<int> sw(m,0);
    auto op = [&sw](int i) {
        rep(j,m) {
            sw[j] = sw[j]^A[i][j];
        }
    };
    int blank_lines = 0;
    {
        int col = 0;
        rep(i,n) {
            while (col < m && A[i][col] == 0) {
                col++;
            }
            if (col == m) {
                blank_lines = n-i;
                break;
            }
            if (sw[col] != S[col]) {
                op(i);
            }
        }
    }
    rep(j,m) {
        if (sw[j] != S[j]) {
            cout << 0 << endl;
            return 0;
        }
    }
    cout << mod_pow(2,blank_lines) << endl;
}
