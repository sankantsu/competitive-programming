#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long a,b,k;

int main() {
    cin >> a >> b >> k;
    int cnt = 0;
    while (a < b) {
        a *= k;
        cnt++;
    }
    cout << cnt << endl;
}
