#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <set>

using namespace std;

long n;

long fun(long a, long b) {
    return a*a*a + a*a*b + a*b*b + b*b*b;
}

set<long> s;

void solve() {
    for (long i = 0; i < 5000; i++) {
        for (long j = i; j < 5000; j++) {
            long res = fun(i,j);
            s.insert(res);
        }
    }
}

int main() {
    cin >> n;
    solve();
    cout << *(s.lower_bound(n)) << endl;
}
