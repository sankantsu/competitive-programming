#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    long ans = 0;
    while (n%2 == 0) {
        ans++;
        n /= 2;
    }
    cout << ans << endl;
}
