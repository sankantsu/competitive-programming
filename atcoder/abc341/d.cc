#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long gcd(long a, long b) {
    if (b == 0) return a;
    return gcd(b, a%b);
}

long lcm(long a, long b) {
    long g = gcd(a,b);
    return a*b/g;
}

int main() {
    long n, m, k;
    cin >> n >> m >> k;
    if (n > m) {
        long tmp = n;
        n = m;
        m = tmp;
    }

    long max = 2*m*k;
    long lc = lcm(n,m);

    auto count = [n,m,lc](long x) {
        return x/n + x/m - 2*(x/lc);
    };

    long lb = 0;
    long ub = max;
    long ans = (lb + ub)/2;
    while (true) {
        if (ub - lb == 1) break;
        long cnt = count(ans);
        if (cnt >= k) {
            ub = ans;
            ans = (lb + ans)/2;
        }
        else {
            lb = ans;
            ans = (ans + ub)/2;
        }
    }
    cout << ub << endl;
}
