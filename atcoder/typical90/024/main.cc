#include <iostream>
#include <cmath>

using namespace std;

constexpr int max_n = 1000;

int n,k;
int a[max_n];
int b[max_n];

int main() {
    cin >> n >> k;
    for (int i = 0; i < n; i++) cin >> a[i];
    for (int i = 0; i < n; i++) cin >> b[i];

    long sum = 0;
    for (int i = 0; i < n; i++) {
        sum += abs(a[i] - b[i]);
    }
    if (sum <= k && (sum - k)%2 == 0) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
