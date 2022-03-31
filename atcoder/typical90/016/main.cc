#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n;

int main() {
    cin >> n;

    vector<long> a(3);
    for (int i = 0; i < 3; i++) cin >> a[i];
    sort(a.begin(),a.end(),greater<long>{});

    long ans = 10000;
    for (int i = 0; i < 10000; i++) {
        for (int j = 0; j < 10000 - i; j++) {
            long k = a[0]*i + a[1]*j;
            if (k > n) break;
            if ((n-k)%a[2] == 0) {
                long num = i + j + (n-k)/a[2];
                if (num < ans) {
                    ans = num;
                }
            }
        }
    }
    cout << ans << endl;
}
