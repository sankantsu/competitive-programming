#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

long m,n;

long pow_mod(long a, long b, long mod) {
    long p = a;
    long ans = 1;
    while (b > 0) {
        if (b&1) {
            ans = (ans*p)%mod;
        }
        b >>= 1;
        p = (p*p)%mod;
    }
    return ans;
}

int main() {
    const int mod = 1000000007;
    cin >> m >> n;
    cout << pow_mod(m,n,mod) << endl;
}
