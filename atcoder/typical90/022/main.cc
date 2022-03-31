#include <iostream>

using namespace std;

long gcd(long a, long b) {
    // cout << "gcd: " << a << " " << b << endl;
    if (b == 0) {
        return a;
    }
    return gcd(b,a%b);
}

long a,b,c;

int main() {
    cin >> a >> b >> c;
    long g;
    g = gcd(a,b);
    // cout << g << endl;
    g = gcd(g,c);
     // cout << g << endl;
    long k = a/g + b/g + c/g - 3;
    cout << k << endl;
}
