#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long g[10][10];
long h[10][10];

int main() {
    long n, m;
    cin >> n >> m;
    
    rep(i,m) {
        long a, b;
        cin >> a >> b;
        a--; b--;
        g[a][b] = 1;
        g[b][a] = 1;
    }
    rep(i,m) {
        long c, d;
        cin >> c >> d;
        c--; d--;
        h[c][d] = 1;
        h[d][c] = 1;
    }

    vector<long> p(n);
    rep(i,n) {
        p[i] = i;
    }

    bool ans = false;
    do {
        bool check = true;
        rep(i,n) rep(j,n) {
            if (g[i][j] != h[p[i]][p[j]]) {
                check = false;
                break;
            }
        }
        ans = ans || check;
    } while (std::next_permutation(p.begin(), p.end()));

    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
