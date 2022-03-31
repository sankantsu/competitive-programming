#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

template <typename T>
struct segtree {
    segtree(int n_) {
        n = 1;
        while (n < n_) {
            n *= 2;
        }
        data.resize(2*n-1);
        init();
    };
    void init() {
        for (auto &x : data) x = -1;
    }
    T query(int a, int b) {
        return query(a,b,0,0,n);
    }
    void update(int k, T x) {
        k += n-1;
        data[k] = x;
        while (k > 0) {
            k = (k-1)/2;
            data[k] = max(data[2*k+1],data[2*k+2]);
        }
    }
    private:
    T query(int a, int b, int k, int l, int r) {
        if (r <= a || b <= l) {
            return -1;
        }
        else if (a <= l && r <= b) {
            return data[k];
        }
        else {
            int m = (l+r)/2;
            T x1 = query(a,b,2*k+1,l,m);
            T x2 = query(a,b,2*k+2,m,r);
            return max(x1,x2);
        }
    }
    int n;
    vector<T> data;
};

int w, n;
int l[501];
int r[501];
int v[501];

long dp[501][10001];

int main() {
    cin >> w >> n;
    for (int i = 1; i <= n; i++) {
        cin >> l[i] >> r[i] >> v[i];
    }
    for (int i = 0; i <= n; i++) {
        for (int j = 0; j <= w; j++) {
            dp[i][j] = -1;
        }
    }
    segtree<long> seg(w+1);
    dp[0][0] = 0;
    seg.update(0,0);
    for (int i = 1; i <= n; i++) {
        for (int j = 0; j <= w; j++) {
            int cl = max(0,j-r[i]);
            int cr = max(0,j-l[i]+1);
            long x = (cr == 0) ? -1 : seg.query(cl,cr);
            if (x != -1) {
                dp[i][j] = max(dp[i-1][j],x+v[i]);
            }
            else {
                dp[i][j] = dp[i-1][j];
            }
        }
        seg.init();
        for (int j = 0; j <= w; j++) {
            seg.update(j,dp[i][j]);
        }
    }
    cout << dp[n][w] << endl;
}
