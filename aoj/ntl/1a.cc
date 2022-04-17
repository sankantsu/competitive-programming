#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

long n;

int main() {
    cin >> n;
    cout << n << ":";
    for (long p = 2; p*p <= n; p++) {
        while (n%p == 0) {
            cout << " " << p;
            n /= p;
        }
    }
    if (n != 1) {
        cout << " " << n;
    }
    cout << endl;
}
