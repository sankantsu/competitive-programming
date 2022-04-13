#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

constexpr int inf = 300001;

int n;
int a[300000];

int dp[300000];
int p[300000];
int q[300000];

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) cin >> a[i];

    fill(dp,dp+n,inf);
    for (int j = 0; j < n; j++) {
        auto k = distance(dp,lower_bound(dp,dp+n,a[j]));
        dp[k] = a[j];
        p[j] = k+1;
    }
    fill(dp,dp+n,inf);
    for (int j = n-1; j >= 0; j--) {
        auto k = distance(dp,lower_bound(dp,dp+n,a[j]));
        dp[k] = a[j];
        q[j] = k+1;
    }
    int mx = -1;
    for (int i = 0; i < n; i++) {
        mx = max(mx,p[i]+q[i]-1);
    }
    cout << mx << endl;
}
