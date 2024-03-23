#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long b, g;
    cin >> b >> g;

    if (b > g) {
        cout << "Bat" << endl;
    }
    else {
        cout << "Glove" << endl;
    }
}
