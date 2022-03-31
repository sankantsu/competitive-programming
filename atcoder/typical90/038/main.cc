#include <iostream>

using namespace std;
using ll = long long;

constexpr ll pow(ll a, ll b) {
    return (b > 0) ? a*pow(a,b-1) : 1;
}

constexpr ll large_num = pow(10,18);

ll a,b;

ll gcd(ll a, ll b) {
    if (b == 0) return a;
    return gcd(b,a%b);
}

int main () {
    cin >> a >> b;
    ll g = gcd(a,b);
    ll c = a/g;
    if (large_num/c < b) {
        cout << "Large" << endl;
    }
    else {
        cout << b*c << endl;
    }
}
