#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <atcoder/modint.hpp>

using mint = atcoder::modint998244353;

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

// (考察は 1-index)
// 1 <= B <= M
// X_B == B であれば、A_l = A_r = B なる l < r を選んだとき
// 必ず A_l (= A_r) = X_B となるので気にする必要がない
//
// また、X_B != B であっても、A_i = B なる i の出現回数が 1 回以下であれば常に問題ない
//
// X_B != B であるとき、A_l = A_r かつ r - l = 1 だと間に X_B が入れないからダメ
//
// e.g. M=3, N=4, X=(2, 1, 2) (testcase 1)
// 不適なものも含めて A の候補は M^N = 81 通り
// 同じ数字が連続するものは上の考察からダメなので、それ以外を列挙してみる。(M * (M-1)^(N-1) = 24 通り)
// 1212 -> o
// 1213 -> o
// 1231 -> o
// 1232 -> x
// 1312 -> x
// 1313 -> x
// 1321 -> o
// 1323 -> o
// 2121 -> o
// 2123 -> o
// 2131 -> x
// 2132 -> o
// 2312 -> o
// 2313 -> x
// 2321 -> x
// 2323 -> x
// 3121 -> o
// 3123 -> o
// 3131 -> x
// 3132 -> x
// 3212 -> o
// 3213 -> o
// 3231 -> o
// 3232 -> x
//
// 左の並び L = A_1,...,A_i が決まっているとき、A_i+1 に置けるやつと置けないやつがある。
// L のうち最も右にある B の位置を P_B とするとき、
// L のうち P_B より右の列 A_{P_B + 1},...,A_i に X_B (!= B) が含まれていない場合は置けない
//
// L に対して次に B が置けるかどうかのフラグの集合 S (M bit) を管理しながら dp すれば良さそう?
// dp[i][S]: 長さ i の列 A_1,...,A_i のうち次に置ける数字の集合が S であるものの個数
// とする。
// 
// 初期化
// dp[0][(1<<M)-1] = 1  // 長さ 0 の列なら次に何を置いても良い
//
// 遷移
// B \in S であるような B を i + 1 番目に置くとき、
// - X_B != B である場合、B は X_B を置くまで置けなくなる。
// - ある B' に対して X_B' = B であるとき、B' を i + 2 番目以降に置くことができるようになる。
//
// よって、X_B' = B なる B' の集合を T(B) とするとき次の遷移をすれば良い
// - X_B == B のとき、dp[i+1][S + T(B)] += dp[i][S]
// - X_B != B のとき、dp[i+1][S + T(B) - B] += dp[i][S]
//
// 計算量
// N*2^M の状態について M 通りの遷移を計算すれば良いから O(N*M*2^M) ~ 10^8

using namespace std;

int main() {
    int m, n;
    cin >> m >> n;

    vector<int> x(m);
    rep(i,m) {
        cin >> x[i];
        x[i]--;
    }

    // t[B]: X_i == B なる i の集合
    vector<int> t(m);
    rep(i,m) {
        t[x[i]] |= 1<<i;
    }
    // debug
    /*rep(i,m) {*/
    /*    cerr << "i,t[i]: " << i << " " << hex << t[i] << endl;*/
    /*}*/

    vector<vector<mint>> dp(n+1, vector<mint>(1<<m));
    dp[0][(1<<m) - 1] = 1;

    rep(i,n) {
        rep(s,1<<m) {
            rep(b,m) {
                if (!(s & (1<<b))) {
                    continue;
                }
                int ns = s | t[b];
                if (x[b] != b) {
                    ns &= ~(1<<b);
                }
                dp[i+1][ns] += dp[i][s];
            }
        }
    }
    // debug
    /*rep(i,n+1) {*/
    /*    rep(s,1<<m) {*/
    /*        cerr*/
    /*            << "i,s,dp: " << dec << i << " "*/
    /*            << hex << s << " "*/
    /*            << dec << dp[i][s].val() << endl;*/
    /*    }*/
    /*    cerr << endl;*/
    /*}*/

    mint ans = 0;
    rep(s, 1<<m) {
        ans += dp[n][s];
    }
    cout << ans.val() << endl;
}
