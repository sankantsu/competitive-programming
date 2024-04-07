#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    rep(i,n) {
        if (i%3 == 2) {
            cout << "x";
        }
        else {
            cout << "o";
        }
    }
    cout << endl;
}
