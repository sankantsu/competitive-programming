#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using std::cin, std::cout, std::cerr, std::endl;

int n;
int P[300000];
int I[300000];

int inv_I[300000];

int left[300000];
int right[300000];

// l1,r1: 行きがけ順で考えた部分木の範囲
// l2,r2: 通りがけ順で考えた部分木の範囲
bool rec(int l1, int r1, int l2, int r2) {
    int root = P[l1];
    int k = inv_I[root];
    if (k < l2 || r2 < k) {
        return false;
    }
    bool b = true;
    if (l2 <= k-1) {
        left[root] = P[l1+1];
        b = b && rec(l1+1,l1-l2+k,l2,k-1); // left subtree
    }
    if (k+1 <= r2) {
        right[root] = P[l1-l2+k+1];
        b = b && rec(l1-l2+k+1,r1,k+1,r2); // right subtree
    }
    return b;
}

int main() {
    cin >> n;
    rep(i,n) cin >> P[i];
    rep(i,n) cin >> I[i];
    rep(i,n) inv_I[I[i]] = i;

    if (P[0] == 1 && rec(0,n-1,0,n-1)) {
        rep(i,n) {
            cout << left[i+1] << " " << right[i+1] << endl;
        }
    }
    else {
        cout << -1 << endl;
    }
}
