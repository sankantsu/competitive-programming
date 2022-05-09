#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_n = 200000;

int n,q;
int x[max_n+1];

int val[max_n+1];
int rank_[max_n+1];

int main() {
    cin >> n >> q;
    rep(i,q) cin >> x[i];

    rep(i,n) val[i] = i+1;
    rep(i,n) rank_[i+1] = i;
    rep(i,q) {
        int r = rank_[x[i]];
        int s = (r == n-1) ? r-1 : r+1;
        int a = val[r];
        int b = val[s];
        val[r] = b;
        val[s] = a;
        rank_[a] = s;
        rank_[b] = r;
    }
    rep(i,n) {
        cout << val[i];
        cout << ((i == n-1) ? "\n" : " ");
    }
}
