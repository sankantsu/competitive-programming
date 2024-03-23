#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

long floor(long x, long m) {
    long r = (x % m + m) % m;
    return (x - r)/m;
}

long ceil(long x, long m) {
    return floor(x + (m - 1), m);
}

int main() {
    long x;
    cin >> x;

    cout << ceil(x, 10) << endl;
}
