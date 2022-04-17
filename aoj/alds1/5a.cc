// bit search O(N 2^N + QN) AC

#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

using namespace std;

int n, q;
int a[21];
int m[201];
bool ans[201];

set<long> sum;

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) cin >> a[i];
    cin >> q;
    for (int i = 0; i < q; i++) cin >> m[i];

    for (int j = 0; j < (1<<n); j++) {
        long res = 0;
        for (int k = 0; k < n; k++) {
            if ((j>>k)&1) {
                res += a[k];
            }
        }
        sum.insert(res);
    }
    for (int i = 0; i < q; i++) {
        int mi = m[i];
        if (sum.find(mi) != sum.end()) {
            ans[i] = true;
        }
    }
    for (int i = 0; i < q; i++) {
        if (ans[i]) {
            cout << "yes" << endl;
        }
        else {
            cout << "no" << endl;
        }
    }
}
