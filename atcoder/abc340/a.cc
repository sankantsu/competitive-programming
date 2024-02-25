#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long a, b, d;
    cin >> a >> b >> d;
    long x = a;
    while (true) {
        cout << x << " ";
        x = x + d;
        if (x > b) break;
    }
    cout << endl;
}
