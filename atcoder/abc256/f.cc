#include <iostream>
#include <vector>
#include <algorithm>
#include <atcoder/modint>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

using mint = atcoder::modint998244353;

struct BIT {
    BIT(long n_) {
        n = 1;
        while (n < n_) {
            n *= 2;
        }
        data.resize(n+1);
    }
    mint sum(int k) {
        mint res = 0;
        while (k > 0) {
            res += data[k];
            k -= k&-k;
        }
        return res;
    }
    void add(int k, mint x) {
        while (k <= n) {
            data[k] += x;
            k += k&-k;
        }
    }
    private:
    int n;
    vector<mint> data;
};

int n,Q;
mint a[210000];

int main() {
    cin >> n >> Q;
    for (int i = 1; i <= n; i++) {
        int a_;
        cin >> a_;
        a[i] = a_;
    }

    BIT bit0(n);
    BIT bit1(n);
    BIT bit2(n);
    for (int i = 1; i <= n; i++) {
        mint x{i};
        bit0.add(i,a[i]);
        bit1.add(i,x*a[i]);
        bit2.add(i,x*x*a[i]);
    }

    rep(q,Q) {
        int t;
        cin >> t;
        if (t == 1) {
            int i;
            int v;
            cin >> i >> v;
            mint x{i};
            mint val = v-a[i];
            bit0.add(i,val);
            bit1.add(i,x*val);
            bit2.add(i,x*x*val);
            a[i] = v;
        }
        else {
            int i;
            cin >> i;
            auto calc = [&](int i) {
                mint x{i};
                mint t0 = (x*x + 3*x + 2)*bit0.sum(i);
                mint t1 = -(2*x+3)*bit1.sum(i);
                mint t2 = bit2.sum(i);
                return (t0+t1+t2)/2;
            };
            cout << calc(i).val() << endl;
        }
    }
}
