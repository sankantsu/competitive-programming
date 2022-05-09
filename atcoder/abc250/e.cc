#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

/* #include <contest/debug> */

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const long max_n = 200000;

long n,q;
long a[max_n+10];
long b[max_n+10];
long x[max_n+10];
long y[max_n+10];

int main() {
    cin >> n;
    rep(i,n) cin >> a[i];
    rep(i,n) cin >> b[i];
    cin >> q;
    rep(i,q) {
        cin >> x[i] >> y[i];
        x[i]--;
        y[i]--;
    }

    set<long> sa,sb;
    vector<long> va,vb; // 新しい数字が初めて登場するindex
    rep(i,n) {
        if (sa.find(a[i]) == sa.end()) {
            sa.insert(a[i]);
            va.push_back(i);
        }
        if (sb.find(b[i]) == sb.end()) {
            sb.insert(b[i]);
            vb.push_back(i);
        }
    }
    /* cerr << "va: "; */
    /* rep(i,va.size()) cerr << va[i] << " "; */
    /* cerr << endl; */
    /* cerr << "vb: "; */
    /* rep(i,vb.size()) cerr << vb[i] << " "; */
    /* cerr << endl; */
    long m = min(va.size(),vb.size());
    set<long> diff_a; // aに含まれていてbには含まれていない
    set<long> diff_b; // bに含まれていてaには含まれていない
    vector<bool> coin(m,false);
    rep(k,m) {
        int i = va[k];
        int j = vb[k];
        /* print("k,i,j,a[i],b[j]",k,i,j,a[i],b[j]); */
        if (diff_b.find(a[i]) == diff_b.end()) diff_a.insert(a[i]);
        if (diff_a.find(b[j]) == diff_a.end()) diff_b.insert(b[j]);
        if (diff_a.find(b[j]) != diff_a.end()) {
            diff_a.erase(b[j]);
        }
        if (diff_b.find(a[i]) != diff_b.end()) {
            diff_b.erase(a[i]);
        }
        if (diff_a.empty() && diff_b.empty()) {
            coin[k] = true;
        }
        /* cout << "diff_a: "; */
        /* for (auto x : diff_a) cout << x << " "; cout << endl; */
        /* cout << "diff_b: "; */
        /* for (auto x : diff_b) cout << x << " "; cout << endl; */
    }
    /* rep(i,m) cerr << coin[i] << endl; */
    rep(i,q) {
        bool ans = false;
        auto it1 = prev(upper_bound(va.begin(),va.end(),x[i]));
        auto it2 = prev(upper_bound(vb.begin(),vb.end(),y[i]));
        int d1 = distance(va.begin(),it1);
        int d2 = distance(vb.begin(),it2);
        /* cerr << "x,y: " << x[i] << " " << y[i] << endl; */
        /* cerr << "d1,d2: " << d1 << " " << d2 << endl; */
        if (d1 != d2) ans = false;
        else {
            if (coin[d1]) ans = true;
            else ans = false;
        }
        if (ans) {
            cout << "Yes" << endl;
        }
        else {
            cout << "No" << endl;
        }
    }
}
