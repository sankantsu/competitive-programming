#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
int a[200];

int p[4];

int main() {
    cin >> n;
    rep(i,n) cin >> a[i];

    int res = 0;
    rep(i,n) {
        int next[4];
        rep(j,4) next[j] = 0;
        p[0] = 1;
        rep(j,4) {
            if (p[j]) {
                int nj = j+a[i];
                if (nj >= 4) {
                    res++;
                }
                else {
                    next[nj] = 1;
                }
            }
        }
        rep(j,4) p[j] = next[j];
    }
    cout << res << endl;
}
