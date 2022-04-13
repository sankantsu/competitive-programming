#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n;
int a[100000];
int b[100000];
int c[100000];

long sum[100001]; // b[i]以降のパーツを使って作れるb[i],c[j]の組の総和

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) cin >> a[i];
    for (int i = 0; i < n; i++) cin >> b[i];
    for (int i = 0; i < n; i++) cin >> c[i];
    sort(a,a+n);
    sort(b,b+n);
    sort(c,c+n);

    /* cout << endl; */
    /* for (int i = 0; i < n; i++) cout << a[i] << " "; cout << endl; */
    /* for (int i = 0; i < n; i++) cout << b[i] << " "; cout << endl; */
    /* for (int i = 0; i < n; i++) cout << c[i] << " "; cout << endl; */
    /* cout << endl; */

    sum[n] = 0;
    for (int i = n-1; i >= 0; i--) {
        auto it = upper_bound(c,c+n,b[i]);
        int cnt = distance(it,c+n);
        sum[i] = sum[i+1] + cnt;
    }

    /* for (int i = 0; i < n; i++) cout << sum[i] << " "; cout << endl; */

    long res = 0;
    for (int i = 0; i < n; i++) {
        int k = distance(b,upper_bound(b,b+n,a[i]));
        res += sum[k];
    }
    cout << res << endl;
}
