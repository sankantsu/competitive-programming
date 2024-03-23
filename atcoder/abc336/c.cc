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
    n--;

    vector<int> ans;
    vector<int> digits = {0, 2, 4, 6, 8};
    if (n == 0) {
        ans.push_back(0);
    }
    else {
        while (n) {
            long k = n%5;
            ans.push_back(digits[k]);
            n /= 5;
        }
    }
    reverse(ans.begin(), ans.end());

    rep(i, ans.size()) {
        cout << ans[i];
    }
    cout << endl;
}
