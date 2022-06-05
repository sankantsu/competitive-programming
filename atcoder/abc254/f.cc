#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int a[200000];
int b[200000];

int diff_a[200000];
int diff_b[200000];

int gcd(int a, int b) {
    if (b == 0) return a;
    return gcd(b,a%b);
}

struct stree {
    stree(int n_) {
        n = 1;
        while (n < n_) {
            n *= 2;
        }
        data.resize(2*n-1);
    }
    void update(int k, int a) {
        k += n-1;
        data[k] = a;
        while (k > 0) {
            k = (k-1)/2;
            data[k] = gcd(data[2*k+1],data[2*k+2]);
        }
    }
    int query(int a, int b) {
        return query_rec(a,b,0,0,n);
    }
    private:
    int query_rec(int a, int b, int k, int l, int r) {
        int res;
        if (r <= a || b <= l) res = 0;
        else if (a <= l && r <= b) res = data[k];
        else {
            int vl = query_rec(a,b,2*k+1,l,(l+r)/2);
            int vr = query_rec(a,b,2*k+2,(l+r)/2,r);
            res = gcd(vl,vr);
        }
        return res;
    }
    int n;
    vector<int> data;
};

int main() {
    int n,q;
    cin >> n >> q;

    rep(i,n) cin >> a[i];
    rep(i,n) cin >> b[i];

    rep(i,n-1) {
        diff_a[i] = a[i+1]-a[i];
        diff_b[i] = b[i+1]-b[i];
    }

    stree sa(n-1);
    stree sb(n-1);
    rep(i,n-1) {
        sa.update(i,abs(diff_a[i]));
        sb.update(i,abs(diff_b[i]));
    }

    rep(i,q) {
        int h1,h2,w1,w2;
        cin >> h1 >> h2 >> w1 >> w2;
        h1--; h2--; w1--; w2--;
        int ga = (h2-h1 == 0) ? 0 : sa.query(h1,h2);
        int gb = (w2-w1 == 0) ? 0 : sb.query(w1,w2);
        int x = a[h1] + b[w1];
        cout << gcd(gcd(ga,gb),x) << endl;
    }
}
