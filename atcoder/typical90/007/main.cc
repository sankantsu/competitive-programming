#include <iostream>
#include <algorithm>
#include <vector>
#include <cmath>

using namespace std;

constexpr int inf = 2*pow(10,9)+1;

constexpr int max_n = 3*pow(10,5);
constexpr int max_q = max_n;

int n,q;

int main() {
    cin >> n;
    vector<int> a(n);
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }
    sort(a.begin(),a.end());

    cin >> q;
    for (int i = 0; i < q; i++) {
        int b;
        cin >> b;
        auto it = lower_bound(a.begin(),a.end(),b);
        int d1 = inf, d2 = inf;
        if (it != a.begin()) d1 = b - *(prev(it));
        if (it != a.end()) d2 = *it - b;
        cout << min(d1,d2) << endl;
    }
}
