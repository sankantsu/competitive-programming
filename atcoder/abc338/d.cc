#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n, m;
    cin >> n >> m;

    vector<long> x(m);
    rep(i,m) {
        cin >> x[i];
        x[i]--;
    }

    long len = 0;
    vector<long> imos(n);
    rep(k,m-1) {
        long i = x[k];
        long j = x[k+1];
        if (j < i) {
            swap(i,j);
        }
        long d1 = abs(j - i);
        long d2 = abs(n - d1);
        len += min(d1, d2);
        long cost = max(d1, d2) - min(d1, d2);
        if (d1 < d2) {
            imos[i] += cost;
            imos[j] -= cost;
        }
        if (d2 < d1) {
            imos[0] += cost;
            imos[i] -= cost;
            imos[j] += cost;
        }
    }

    vector<long> sum(n+1);
    rep(i,n) {
        sum[i+1] = sum[i] + imos[i];
    }

    long min_penalty = 1L<<60;
    rep(i,n) {
        min_penalty = min(min_penalty, sum[i+1]);
    }
    cout << len + min_penalty << endl;
}
