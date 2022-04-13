#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int max_n = 500;

int n,m,Q;
int l[200000];
int r[200000];
int p[100000];
int q[100000];

/* int cnt[600][600]; */
int sum[600][600];

void make_sum() {
    rep(i,m) {
        sum[l[i]][r[i]]++;
    }
    rep(i,max_n+1) {
        rep(j,max_n+1) {
            sum[i+1][j] = sum[i][j]+sum[i+1][j];
        }
    }
    rep(i,max_n+1) {
        rep(j,max_n+1) {
            sum[i][j+1] = sum[i][j]+sum[i][j+1];
        }
    }
    /* cout << "cnt:" << endl; */
    /* for (int i = 1; i <= n; i++) { */
    /*     for (int j = 1; j <= n; j++) { */
    /*         cout << cnt[i][j] << " "; */
    /*     } */
    /*     cout << endl; */
    /* } */
    /* cout << "sum:" << endl; */
    /* for (int i = 1; i <= n; i++) { */
    /*     for (int j = 1; j <= n; j++) { */
    /*         cout << sum[i][j] << " "; */
    /*     } */
    /*     cout << endl; */
    /* } */
}

int calc_sum(int p, int q) {
    int s1 = sum[q][q];
    int s2 = sum[p-1][q];
    int s3 = sum[q][p-1];
    int s4 = sum[p-1][p-1];
    return s1-s2-s3+s4;
}

int main() {
    cin >> n >> m >> Q;
    rep(i,m) {
        cin >> l[i] >> r[i];
    }
    rep(i,Q) {
        cin >> p[i] >> q[i];
    }
    make_sum();
    rep(i,Q) {
        cout << calc_sum(p[i],q[i]) << endl;
    }
}
