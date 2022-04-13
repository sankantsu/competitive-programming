#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

using namespace std;

long n;
set<long> s;

long pow(long a,long b) {
    if (b <= 0) return 1;
    return a*pow(a,b-1);
}

int main() {
    cin >> n;
    for (long i = 2; i*i <= n; i++) {
        long j = 2;
        while (true) {
            long k = pow(i,j);
            if (k > n) break;
            s.insert(k);
            j++;
        }
    }
    cout << (n - s.size()) << endl;
}
