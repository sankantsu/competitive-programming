#include <iostream>
#include <string>
#include <map>
#include <set>
#include <random>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

long solve(const string& s) {
    size_t n = s.size();

    map<char, size_t> cnt;
    rep(i,n) {
        cnt[s[i]]++;
    }

    long x = 0;
    long y = 0;
    for (auto [c, k] : cnt) {
        if (k >= 2) y = 1;
        x += k*(n - k);
    }
    long ans = x/2 + y;

    return ans;
}

long solve_jury(const string& s) {
    string t = s;
    size_t n = s.size();

    set<string> st;
    rep(i,n) rep(j,n) {
        if (i == j) continue;
        swap(t[i], t[j]);
        st.insert(t);
        swap(t[i], t[j]);  // restore
    }
    return st.size();
}

string gen_test() {
    static mt19937 mt;

    long n = 10;
    string s;
    rep(i,n) {
        long k = mt() % 26;
        s.push_back('a' + k);
    }
    return s;
}

void test() {
    long iter = 0;
    while (iter++ < 10000) {
        string s = gen_test();
        long ans_exp = solve_jury(s);
        long ans_act = solve(s);
        if (ans_exp != ans_act) {
            cerr << "Wrong answer!" << endl;
            cerr << "expected: " << ans_exp << endl;
            cerr << "actual: " << ans_act << endl;
            cout << s << endl;
            exit(1);
        }
    }
    cerr << "Passed all tests!" << endl;
    exit(0);
}

int main() {
    /* test(); */

    string s;
    cin >> s;

    long ans = solve(s);
    cout << ans << endl;
}
