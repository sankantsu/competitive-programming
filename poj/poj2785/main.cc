#include <iostream>
#include <algorithm>

using namespace std;

#define MAX_N 4000

int n;
int a[MAX_N];
int b[MAX_N];
int c[MAX_N];
int d[MAX_N];

int sum[MAX_N*MAX_N];

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) {
        cin >> a[i];
        cin >> b[i];
        cin >> c[i];
        cin >> d[i];
    }

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            sum[n*i+j] = c[i] + d[j];
        }
    }
    sort(sum,sum+n*n);

    int cnt = 0;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            int k = a[i] + b[j];
            int *lb = lower_bound(sum,sum+n*n,-k);
            int *ub = upper_bound(sum,sum+n*n,-k);
            cnt += static_cast<int>(ub-lb);
        }
    }
    cout << cnt << endl;
}
