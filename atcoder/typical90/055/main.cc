#include <iostream>

using namespace std;

constexpr int max_n = 100;

long n,p,q;
long a[max_n];

int main() {
    cin >> n >> p >> q;
    for (int i = 0; i < n; i++) cin >> a[i];

    long cnt = 0;
    for (int i = 0; i < n; i++) {
        for (int j = i+1; j < n; j++) {
            for (int k = j+1; k < n; k++) {
                for (int l = k+1; l < n; l++) {
                    for (int m = l+1; m < n; m++) {
                        long prod = 1;
                        prod = (prod * a[i])%p;
                        prod = (prod * a[j])%p;
                        prod = (prod * a[k])%p;
                        prod = (prod * a[l])%p;
                        prod = (prod * a[m])%p;
                        if (prod == q) cnt++;
                    }
                }
            }
        }
    }

    cout << cnt << endl;
}
