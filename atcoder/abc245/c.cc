#include <iostream>
#include <cmath>

using namespace std;

constexpr int max_n = 200000;

long n,k;
long a[max_n];
long b[max_n];

bool ok[max_n][2];

int main() {
    cin >> n >> k;
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }
    for (int i = 0; i < n; i++) {
        cin >> b[i];
    }

    ok[0][0] = true;
    ok[0][1] = true;
    for (int i = 1; i < n; i++) {
        for (int j = 0; j <= 1; j++) {
            long *p;
            if (j == 0) {
                p = a;
            }
            else {
                p = b;
            }
            if (ok[i-1][j]) {
                if (abs(a[i] - p[i-1]) <= k) {
                    ok[i][0] = true;
                }
                if (abs(b[i] - p[i-1]) <= k) {
                    ok[i][1] = true;
                }
            }
        }
    }
    /* cout << "ok:" << endl; */
    /* for (int i = 0; i < n; i++) { */
    /*     cout << ok[i][0] << " " << ok[i][1] << endl; */
    /* } */
    if (ok[n-1][0] || ok[n-1][1]) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
