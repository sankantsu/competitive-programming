#include <iostream>

using namespace std;

int n, p, k;
int g[41][41];

int dp[41][41];

void init(int x) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (g[i][j] == -1) dp[i][j] = x;
            else dp[i][j] = g[i][j];
        }
    }
}

void warshall_floyd() {
    for (int k = 0; k < n; k++) {
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                dp[i][j] = min(dp[i][j],dp[i][k]+dp[k][j]);
            }
        }
    }
}

int count_pair(int x) {
    init(x);
    warshall_floyd();
    int cnt = 0;
    for (int i = 0; i < n; i++) {
        for (int j = i+1; j < n; j++) {
            if (dp[i][j] <= p) {
                cnt++;
            }
        }
    }
    return cnt;
}

void solve() {
    if (count_pair(p+1) == k) {
        cout << "Infinity" << endl;
        return;
    }
    // calc lower bound
    int low = 0; int high = p+1;
    while (high - low > 1) {
        int c = (low+high)/2;
        if (count_pair(c) <= k) high = c;
        else low = c;
    }
    int lb = high;
    /* cout << "lb: " << lb << endl; */
    // calc upper bound
    low = 0; high = p+1;
    while (high - low > 1) {
        int c = (low+high)/2;
        if (count_pair(c) < k) high = c;
        else low = c;
    }
    int ub = high;
    /* cout << "ub: " << ub << endl; */
    cout << (ub - lb) << endl;
}

int main() {
    cin >> n >> p >> k;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cin >> g[i][j];
        }
    }
    solve();
}
