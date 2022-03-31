#include <iostream>
#include <algorithm>
#include <vector>
#include <cmath>

using namespace std;

constexpr int max_n = pow(10,5);

int n;

int main() {
    cin >> n;
    vector<int> a(n);
    vector<int> b(n);

    for (int i = 0; i < n; i++) cin >> a[i];
    for (int i = 0; i < n; i++) cin >> b[i];

    sort(a.begin(),a.end());
    sort(b.begin(),b.end());

    long ans = 0;
    for (int i = 0; i < n; i++) {
        ans += abs(a[i] - b[i]);
    }

    cout << ans << endl;
}
