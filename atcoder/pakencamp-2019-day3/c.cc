#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
long a[100][100];

long calc_score(int i, int j) {
    long sum = 0;
    rep(k,n) {
        long score = max(a[k][i],a[k][j]);
        sum += score;
    }
    return sum;
}

int main() {
    cin >> n >> m;
    rep(i,n) {
        rep(j,m) {
            cin >> a[i][j];
        }
    }
    long res = -1;
    rep(i,m) {
        rep(j,m) {
            res = max(res,calc_score(i,j));
        }
    }
    cout << res << endl;
}
