#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long h,w,K,v;
long a[200][200];

long sum[200][200];

void calc_sum() {
    rep(i,h) {
        rep(j,w) {
            sum[i+1][j+1] = a[i][j];
        }
    }
    rep(i,h) {
        rep(j,w+1) {
            sum[i+1][j] = sum[i][j] + sum[i+1][j];
        }
    }
    rep(i,h+1) {
        rep(j,w) {
            sum[i][j+1] = sum[i][j] + sum[i][j+1];
        }
    }
    /* cout << "a:" << endl; */
    /* rep(i,h) { */
    /*     rep(j,w) { */
    /*         cout << a[i][j] << " "; */
    /*     } */
    /*     cout << endl; */
    /* } */
    /* cout << "sum:" << endl; */
    /* rep(i,h+1) { */
    /*     rep(j,w+1) { */
    /*         cout << sum[i][j] << " "; */
    /*     } */
    /*     cout << endl; */
    /* } */
}

// sum (i,j) to (k-1,l-1) (i < k, j < l)
long area_sum(int i, int j, int k, int l) {
    long s1 = sum[k][l];
    long s2 = sum[i][l];
    long s3 = sum[k][j];
    long s4 = sum[i][j];
    return s1-s2-s3+s4;
}

int main() {
    cin >> h >> w >> K >> v;
    rep(i,h) {
        rep(j,w) {
            cin >> a[i][j];
        }
    }
    calc_sum();
    long res = 0;
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            for (int k = i+1; k <= h; k++) {
                for (int l = j+1; l <= w; l++) {
                    long area = (k-i)*(l-j);
                    long area_cost = area_sum(i,j,k,l);
                    long build_cost = K*area;
                    long cost = area_cost + build_cost;
                    /* cout << "i,j,k,l: " << i << " " << j << " " << k << " " << l << endl; */
                    /* cout << "area,build: " << area_cost << " " << build_cost << endl; */
                    if (cost <= v) {
                        res = max(res,area);
                    }
                }
            }
        }
    }
    cout << res << endl;
}
