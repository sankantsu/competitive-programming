#include <iostream>
// #include <contest/util.h>

using namespace std;

int n;
long a[46];
long b[46];
long c[46];

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) {
        long x;
        cin >> x;
        a[x%46]++;
    }
    for (int i = 0; i < n; i++) {
        long x;
        cin >> x;
        b[x%46]++;
    }
    for (int i = 0; i < n; i++) {
        long x;
        cin >> x;
        c[x%46]++;
    }

    /* cout << "a: "; */
    /* for (int i = 0; i < 46; i++) { */
    /*     cout << a[i] << " "; */
    /* } */
    /* cout << endl; */

    long sum = 0;
    for (int i = 0; i < 46; i++) {
        for (int j = 0; j < 46; j++) {
            for (int k = 0; k < 46; k++) {
                if ((i + j + k) % 46 != 0) continue;
                // print_vals("i,j,k,a[i]*b[j]*c[k]",i,j,k,a[i]*b[j]*c[k]);
                sum += a[i]*b[j]*c[k];
            }
        }
    }
    cout << sum << endl;
}
