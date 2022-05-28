#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long n,a,b;

long gcd(long a, long b) {
    if (b == 0) return a;
    return gcd(b,a%b);
}

long lcm(long a, long b) {
    long g = gcd(a,b);
    return a*b/g;
}

long sum(long n) {
    return n*(n+1)/2;
}

int main() {
    cin >> n >> a >> b;
    long c = lcm(a,b);

    long na = n/a;
    long nb = n/b;
    long nc = n/c;

    long s = sum(n);
    s -= sum(na)*a;
    s -= sum(nb)*b;
    s += sum(nc)*c;

    cout << s << endl;
}
