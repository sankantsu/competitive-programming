#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

// S = X_1 + ... + X_N とする
//
// (i) K が 0 以下のとき
// Y_0 = 0 だから、Y のすべての要素が K 以上でないとダメ
// (i-i) S < K のとき、必ず Y_N < K になるからダメ
// (i-ii) S >= K のとき X のうち正のものを先に配置するようにすれば OK?
//
// (ii) K が 0 より大きいとき
// (ii-i) S < K のとき、X のうち負のものを先に置けば Forall i. Y_i < K になり OK?
// (ii-ii) S >= K のとき、X のうち負のものを先に置けば Y ははじめ K 未満で、途中から単調増になるから OK?

int main() {
    long n, k;
    cin >> n >> k;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    sort(a.begin(), a.end());
    if (k <= 0) {
        reverse(a.begin(), a.end());
    }

    long s = 0;
    rep(i,n) s += a[i];

    if (k <= 0 && s < k) {
        cout << "No" << endl;
    } else {
        cout << "Yes" << endl;
        rep(i,n) {
            cout << a[i] << " ";
        }
        cout << endl;
    }
}
