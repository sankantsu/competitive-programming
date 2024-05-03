#include <iostream>
#include <string>
#include <random>
#include <atcoder/segtree.hpp>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

static mt19937 mt;

const int nmod = 5;
long mod[nmod];
long base[nmod];

bool is_prime(long n) {
    bool res = true;
    for (long i = 2; i*i <= n; i++) {
        if (n%i == 0) {
            res = false;
            break;
        }
    }
    return res;
}

void init_mod() {
    long p = 998244353;
    int i = 0;
    while (i < nmod) {
        if (is_prime(p)) {
            mod[i++] = p;
        }
        p += 2;
    }
}

void init_base() {
    rep(i,nmod) {
        base[i] = mt() % mod[i];
    }
}

void init() {
    init_mod();
    init_base();
}

struct S {
    long h1;
    long h2;  // reverse string
    long pw;

    static S op(S lhs, S rhs, long mod) {
        S res;
        res.h1 = (lhs.h1*rhs.pw + rhs.h1)%mod;
        res.h2 = (lhs.h2 + rhs.h2*lhs.pw)%mod;
        res.pw = (lhs.pw*rhs.pw)%mod;
        return res;
    }

    static S e() {
        S res;
        res.h1 = 0;
        res.h2 = 0;
        res.pw = 1;
        return res;
    }

    static S from_char(char c, long base) {
        S res;
        res.h1 = c;
        res.h2 = c;
        res.pw = base;
        return res;
    }
};

struct T {
    T() : vs(nmod) {}
    vector<S> vs;
    
    static T op(T lhs, T rhs) {
        T res;
        rep(i,nmod) {
            res.vs[i] = S::op(lhs.vs[i], rhs.vs[i], mod[i]);
        }
        return res;
    }

    static T e() {
        T res;
        rep(i,nmod) {
            res.vs[i] = S::e();
        }
        return res;
    }

    static T from_char(char c) {
        T res;
        rep(i,nmod) {
            res.vs[i] = S::from_char(c, base[i]);
        }
        return res;
    }
    
    bool is_palindrome() const {
        bool res = true;
        rep(i,vs.size()) {
            res = res && (vs[i].h1 == vs[i].h2);
        }
        return res;
    }
};

using Seg = atcoder::segtree<T, T::op, T::e>;

int main() {
    init();

    int n, q;
    cin >> n >> q;

    string s;
    cin >> s;

    vector<T> ss(n);
    rep(i,n) ss[i] = T::from_char(s[i]);
    Seg seg(ss);

    rep(_,q) {
        int t;
        cin >> t;
        if (t == 1) {
            int x;
            char c;
            cin >> x >> c;
            x--;
            seg.set(x, T::from_char(c));
        }
        else if (t == 2) {
            int l, r;
            cin >> l >> r;
            l--;
            T prod = seg.prod(l, r);
            if (prod.is_palindrome()) {
                cout << "Yes" << endl;
            }
            else {
                cout << "No" << endl;
            }
        }
    }
}
