#include <iostream>
#include <algorithm>

using namespace std;

int n, m;
int a[1000000];

int main () {
    cin >> n >> m;
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }

    for (int k = 0; k < m; k++) {
        // copy(a,a+n,buf);
        for (int i = 1; i < n; i++) {
            a[i] ^= a[i-1];
        }
        cout << a[n-1] << " ";
        /* for (int i = 0; i < n; i++) { */
        /*     cout << a[i] << " "; */
        /* } */
        /* cout << endl; */
    }
    cout << endl;
}
