#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    int a, b;
    cin >> a >> b;

    int c = a + b;
    if (c == 0) {
        cout << 1 << endl;
    }
    else {
        cout << c - 1 << endl;
    }
}
