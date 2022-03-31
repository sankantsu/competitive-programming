#include <iostream>

// #include <contest/util.h>

using namespace std;

constexpr int max_n = 100;

int n,m;
long a[max_n+1];
long b[max_n+1];
long c[2*max_n+1];

int main() {
    cin >> n >> m;
    for (int i = 0; i <= n; i++) {
        cin >> a[i];
    }
    for (int i = 0; i <= m+n; i++) {
        cin >> c[i];
    }

    for (int j = m; j >= 0; j--) {
        b[j] = c[j+n]/a[n];
        for (int i = n; i >= 0; i--) {
            // print_vals("i,j,a[i],b[j]",i,j,a[i],b[j]);
            c[j+i] -= a[i]*b[j];
        }

        /* cout << "c:" << endl; */
        /* for (int i = m+n; i >= 0; i--) { */
        /*     cout << c[i] << " "; */
        /* } */
        /* cout << endl; */
    }

    for (int j = 0; j <= m; j++) {
        cout << b[j] << " ";
    }
    cout << endl;
}
