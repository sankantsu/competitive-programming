#include <iostream>

using namespace std;

int n;
long a[18];

int main() {
    cin >> n;
    for (int i = 0; i < (1<<n); i++) cin >> a[i];

    for (int k = 1; k < (1<<n); k++) {
        long res = -1;
        for (int i = 0; i < (1<<n); i++) {
            for (int j = i+1; j < (1<<n); j++) {
                if ((i|j) <= k) {
                    /* cout << i << " " << j << " " << a[i] + a[j] << endl; */
                    res = max(res,a[i]+a[j]);
                }
            }
        }
        cout << res << endl;
    }
}
