#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,q;
int a[200000];
int l[200000];
int r[200000];
int x[200000];

int main() {
    cin >> n;
    rep(i,n) cin >> a[i];
    cin >> q;
    rep(i,q) {
        cin >> l[i] >> r[i] >> x[i];
        l[i]--; r[i]--;
    }

    int bsize = sqrt(n);
    int bnum = n/bsize+1;
    vector<vector<int>> bucket(bnum,vector<int>(n+1,0)); // i番目のバケツ内の値jの数
    rep(i,bnum) {
        rep(j,bsize) {
            int k = bsize*i + j;
            if (k >= n) break;
            bucket[i][a[k]]++;
        }
    }
    /* cout << "bsize,bnum: " << bsize << " " << bnum << endl; */
    /* rep(i,bnum) { */
    /*     rep(j,n+1) { */
    /*         cout << "i,j,bucket[i][j]: " << i << " " << j << " " << bucket[i][j] << endl; */
    /*     } */
    /* } */

    rep(i,q) {
        int res = 0;
        int lb = l[i]/bsize;
        int ub = r[i]/bsize;
        /* cout << "lb,ub: " << lb << " " << ub << endl; */
        for (int j = lb+1; j <= ub-1; j++) {
            res += bucket[j][x[i]];
        }
        if (lb != ub) {
            for (int k = l[i]; k < bsize*(lb+1); k++) {
                if (a[k] == x[i]) res++;
            }
            for (int k = r[i]; k >= bsize*ub; k--) {
                if (a[k] == x[i]) res++;
            }
        }
        else {
            for (int k = l[i]; k <= r[i]; k++) {
                if (a[k] == x[i]) res++;
            }
        }
        cout << res << endl;
    }
}
