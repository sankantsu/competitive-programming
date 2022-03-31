#include <iostream>

using namespace std;

int n;
long a[100000];

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }

    long sum = 0;
    for (int i = 0; i < n; i++) {
        sum += a[i];
    }

    bool ans = false;
    int p = 0;
    int q = 0;
    long s = 0;
    while (true) {
        // cout << "p,q: " << p << " " << q << endl;
        while(10*s < sum) {
            s += a[q];
            q = (q+1)%n;
        }
        if (10*s == sum) {
            ans = true;
            goto END;
        }
        while (10*s > sum) {
            // cout << "p: " << p << endl;
            s -= a[p];
            p = (p+1)%n;
            if (p == 0) {
                ans = false;
                goto END;
            }
        }
    }

END:
    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
