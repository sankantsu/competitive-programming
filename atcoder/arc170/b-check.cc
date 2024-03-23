#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <random>
#include <fstream>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long solve(long n, const vector<long>& a) {
    long ans = 0;
    for (long i = 0; i < n; i++) for (long j = i + 2; j < n; j++) {
        for (long k = i; k <= j; k++) for (long l = k + 1; l <= j; l++) for (long m = l + 1; m <= j; m++) {
            if (a[l] - a[k] == a[m] - a[l]) {
                cerr << "i,j: " << i << " " << j << endl;
                cerr << "k,l,m: " << k << " " << l << " " << m << endl;
                ans++;
                goto next;
            }
        }
next:
        ;
    }
    return ans;
}

int main() {
    long n;
    cin >> n;
    
    vector<long> a(n);
    rep(i,n) cin >> a[i];

    long ans = solve(n,a);
    cout << ans << endl;
}
