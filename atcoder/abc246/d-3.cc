#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

using namespace std;

long n;

long fun(long a, long b) {
    return a*a*a + a*a*b + a*b*b + b*b*b;
}

set<long> s;

int main() {
    cin >> n;
    if (n == 0) {
        cout << n << endl;
        return 0;
    }
    for (long i = 0; i <= 1000000; i++) {
        long lb = 0;
        long ub = 1000001;
        while (ub - lb > 1) {
            long j = (lb+ub)/2;
            if (fun(i,j) < n) lb = j;
            else ub = j;
        }
        s.insert(fun(i,ub));
    }
    cout << *(s.lower_bound(n)) << endl;
}
