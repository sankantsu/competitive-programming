#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n;
long a[3001];
long sum[3001];

int main() {
    cin >> n;
    for (int i = 1; i <= n; i++) cin >> a[i];
    for (int i = 1; i <= n; i++) {
        sum[i] = sum[i-1] + a[i];
    }
    for (int k = 1; k <= n; k++) {
        long res = -1;
        for (int i = 1; i+k-1 <= n; i++) {
            res = max(res, sum[i+k-1]-sum[i-1]);
        }
        cout << res << endl;
    }
}
