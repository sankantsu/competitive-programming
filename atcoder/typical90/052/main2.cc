#include <iostream>
#include <atcoder/modint>

using namespace std;
using mint = atcoder::modint1000000007;

constexpr int max_n = 100;
int n;

int main() {
    cin >> n;

    mint ans = 1;
    for (int i = 1; i <= n; i++) {
        mint sum = 0;
        for (int k = 0; k < 6; k++) {
            int tmp;
            cin >> tmp;
            sum += tmp;
        }
        ans *= sum;
    }

    cout << ans.val() << endl;
}
