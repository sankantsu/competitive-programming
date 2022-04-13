#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n;
long a[3001];
long sum[3001]; // sum[i] = a[0]+...+a[i-1];

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) cin >> a[i];
    for (int i = 0; i < n; i++) {
        sum[i+1] = sum[i] + a[i];
    }
    for (int k = 1; k <= n; k++) {
        long res = -1;
        for (int i = 0; i+k <= n; i++) {
            res = max(res,sum[i+k]-sum[i]);
        }
        cout << res << endl;
    }
}
