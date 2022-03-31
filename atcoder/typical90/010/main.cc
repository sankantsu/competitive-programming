#include <iostream>
#include <cmath>

using namespace std;

constexpr int max_n = pow(10,5);
constexpr int max_q = pow(10,5);

int n,q;
int c[max_n+1];
int p[max_n+1];

int sum1[max_n+1];
int sum2[max_n+1];

int main() {
    cin >> n;
    for (int i = 1; i <= n; i++) {
        cin >> c[i] >> p[i];
    }

    for (int i = 1; i <= n; i++) {
        sum1[i] = sum1[i-1];
        sum2[i] = sum2[i-1];
        if (c[i] == 1) {
            sum1[i] += p[i];
        }
        else {
            sum2[i] += p[i];
        }
    }

    cin >> q;
    for (int i = 0; i < q; i++) {
        int l,r;
        cin >> l >> r;
        cout << (sum1[r] - sum1[l-1]) << " " << (sum2[r] - sum2[l-1]) << endl;
    }
}
