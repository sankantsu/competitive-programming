#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    if (n <= 125) {
        cout << 4 << endl;
    }
    else if (n <= 211) {
        cout << 6 << endl;
    }
    else {
        cout << 8 << endl;
    }
}
