#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
int c[10];

int main() {
    cin >> n;
    for (int j = 1; j <= 9; j++) cin >> c[j];

    int min_c = n+1;
    for (int j = 1; j <= 9; j++) min_c = min(min_c,c[j]);

    int digit = n/min_c;

    int rest = n - digit*min_c;
    for (int j = 1; j <= 9; j++) c[j] -= min_c;
    rep(i,digit) {
        for (int j = 9; j >= 1; j--) {
            if (rest >= c[j]) {
                cout << j;
                rest -= c[j];
                break;
            }
        }
    }
    cout << endl;
}
