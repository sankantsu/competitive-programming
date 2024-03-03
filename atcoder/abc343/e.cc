#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long v1, v2, v3;
    cin >> v1 >> v2 >> v3;

    long total = v1 + 2*v2 + 3*v3;
    if (total != 3*7*7*7) {
        cout << "No" << endl;
        return 0;
    }
}
