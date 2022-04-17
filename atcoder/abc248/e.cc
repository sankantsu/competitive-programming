#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,K;
long x[300];
long y[300];

using pos = pair<long,long>;

long outer_product(pos p, pos q) {
    auto [x0,y0] = p;
    auto [x1,y1] = q;
    return x0*y1 - x1*y0;
}

int count_points(int i, int j) {
    int cnt = 0;
    rep(k,n) {
        pos p = make_pair(x[k]-x[i],y[k]-y[i]);
        pos q = make_pair(x[k]-x[j],y[k]-y[j]);
        long res = outer_product(p,q);
        if (res == 0) {
            cnt++;
        }
    }
    return cnt;
}

int main() {
    cin >> n >> K;
    rep(i,n) cin >> x[i] >> y[i];
    if (K == 1) {
        cout << "Infinity" << endl;
        return 0;
    }
    vector<int> res(n+1);
    rep(i,n) {
        for (int j = i+1; j < n; j++) {
            int cnt = count_points(i,j);
            if (cnt >= K) {
                res[cnt]++;
                /* cout << "i,j: " << i << " " << j << endl; */
            }
        }
    }
    long ans = 0;
    for (int i = 2; i <= n; i++) {
        /* cout << "res " << i << " " << res[i] << endl; */
        int comb = i*(i-1)/2;
        ans += res[i]/comb;
    }
    cout << ans << endl;
}
