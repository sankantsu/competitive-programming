#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

long n,k,x;
long a[200000];

int main() {
    cin >> n >> k >> x;
    for (int i = 0; i < n; i++) cin >> a[i];
    sort(a,a+n,greater<long>{});
    long i = 0;
    while (k > 0 && i < n) {
        while (k > 0 && a[i] >= x) {
            k--;
            a[i] -= x;
        }
        i++;
    }
    if (k > 0) {
        sort(a,a+n,greater<long>{});
        fill(a,a+min(k,n),0);
    }
    long sum = 0;
    for (int i = 0; i < n; i++) {
        sum += a[i];
    }
    cout << sum << endl;
}
