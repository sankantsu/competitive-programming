#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n;
int a[100000];
int b[100000];
int c[100000];

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) cin >> a[i];
    for (int i = 0; i < n; i++) cin >> b[i];
    for (int i = 0; i < n; i++) cin >> c[i];
    sort(a,a+n);
    sort(b,b+n);
    sort(c,c+n);

    long res = 0;
    for (int i = 0; i < n; i++) {
        long k = distance(a,lower_bound(a,a+n,b[i]));
        long l = distance(upper_bound(c,c+n,b[i]),c+n);
        res += k*l;
    }
    cout << res << endl;
}
