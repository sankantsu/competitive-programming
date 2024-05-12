#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n, k;
    cin >> n >> k;

    vector<int> a(n);
    rep(i,n) cin >> a[i];

    int cnt = 0;
    int i = 0;
    while (i < n) {
        int cap = k;
        while (i < n && a[i] <= cap) {
            cap -= a[i];
            i++;
        }
        cnt++;
    }
    cout << cnt << endl;
}
