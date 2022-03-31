#include <iostream>

using namespace std;

long pow(long a, long b) {
    long res = 1;
    for ( ;b > 0; b--) {
        res *= a;
    }
    return res;
}

long a,b,c;

int main() {
    cin >> a >> b >> c;
    long d = pow(c,b);
    long res = d - a;
    if (res > 0) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
