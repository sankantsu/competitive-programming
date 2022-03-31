#include <iostream>
#include <cmath>

using namespace std;

int n, q;
long x[100000];
long y[100000];
int query[100000];

long proc(int i) {
    long res = 0;
    for (int j = 0; j < n; j++) {
        res = max(res,abs(x[i]-x[j])+abs(y[i]-y[j]));
    }
    return res;
}

int main() {
    cin >> n >> q;
    for (int i = 0; i < n; i++) {
        cin >> x[i] >> y[i];
    }
    for (int i = 0; i < q; i++) {
        cin >> query[i];
        query[i]--;
    }
    for (int i = 0; i < q; i++) {
        long ans = proc(query[i]);
        cout << ans << endl;
    }
}
