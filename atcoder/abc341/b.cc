#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    int n;
    cin >> n;

    vector<long> a(n);
    vector<long> s(n-1);
    vector<long> t(n-1);
    rep(i,n) {
        cin >> a[i];
    }
    rep(i,n-1) {
        cin >> s[i] >> t[i];
    }

    rep(i,n-1) {
        long m = a[i]/s[i];
        a[i+1] += m*t[i];
    }
    cout << a[n-1] << endl;
}
