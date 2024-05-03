#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n;
    cin >> n;

    vector<int> a(n-1);
    rep(i,n-1) cin >> a[i];

    int sum = 0;
    rep(i,n-1) sum += a[i];

    int ans = -sum;
    cout << ans << endl;
}
