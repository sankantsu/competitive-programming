#include <iostream>
#include <vector>
#include <map>
#include <cassert>
#include <random>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

auto solve(long n, long m, long k, const vector<long>& c) {
    vector<long> cnt(n);
    rep(i,n) cnt[c[i]]++;

    long r = 0;
    long ans = 0;
    vector<long> ansv;
    vector<long> mp(n);
    long n_free = m;
    rep(i,n) {
        /* cerr << "-------------" << endl; */
        /* cerr << "i: " << i << endl; */
        while (r < i + n) {
            long cl = c[r];
            if (mp[cl]%k != 0) {
                mp[cl]++;
                r++;
            }
            else if (n_free == 0) {
                /* cerr << "cannot take!" << endl; */
                ansv.push_back(ans);
                break;
            }
            else {  // new box
                /* cerr << "add " << cl << endl; */
                n_free--;
                ans += (mp[cl] == k*(cnt[cl]/k)) ? cnt[cl]%k : k;
                mp[cl]++;
                r++;
            }
            if (r == i + n) {
                /* cerr << "reach end" << endl; */
                ansv.push_back(ans);
                break;
            }
        }
        {
            long cl = c[i];
            mp[cl]--;
            if (mp[cl]%k == 0) {
                /* cerr << "remove " << cl << endl; */
                n_free++;
                ans -= (mp[cl] == k*(cnt[cl]/k)) ? cnt[cl]%k : k;
            }
        }
    }
    return ansv;
}

auto solve_jury(long n, long m, long k, const vector<long>& c) {
    vector<long> ansv;
    rep(i,n) {
        long n_free = m;
        long ans = 0;
        vector<size_t> mp(n);
        rep(j,n) {
            long cl = c[i + j];
           if (mp[cl]%k != 0) {
                mp[cl]++;
                ans++;
            }
            else if (n_free > 0) {
                n_free--;
                mp[cl]++;
                ans++;
            }
        }
        ansv.push_back(ans);
    }
    return ansv;
}

auto gen_random() {
    static mt19937 mt;
    long n = 10;
    long m = 1 + mt() % (n - 1);
    long k = 1 + mt() % (n - 1);
    vector<long> c(2*n);
    rep(i,n) {
        c[i] = mt() % n;
    }
    rep(i,n) c[n+i] = c[i];  // extend
    return make_tuple(n, m, k, c);
}

void test() {
    long n_case = 100000;
    rep(_, n_case) {
        auto [n, m, k, c] = gen_random();
        auto ans_exp = solve_jury(n, m, k, c);
        auto ans_act = solve(n, m, k, c);
        assert(ans_act.size() == n && ans_exp.size() == n);
        bool check = true;
        rep(i,n) {
            if (ans_act[i] != ans_exp[i]) check = false;
        }
        if (!check) {
            cerr << "Wrong answer!" << endl;
            cerr << "expected: "; rep(i,n) cerr << ans_exp[i] << " "; cerr << endl;
            cerr << "actual: "; rep(i,n) cerr << ans_act[i] << " "; cerr << endl;
            cout << n << " " << m << " " << k << endl;
            rep(i,n) cout << c[i]+1 << " "; cout << endl;
            exit(1);
        }
    }
    cerr << "All test passed!" << endl;
    exit(0);
}

int main() {
    /* test(); */

    long n, m, k;
    cin >> n >> m >> k;

    vector<long> c(2*n);
    rep(i,n) {
        cin >> c[i];
        c[i]--;
    }
    rep(i,n) c[n+i] = c[i];  // extend

    auto ans = solve(n, m, k, c);
    assert(ans.size() == n);
    rep(i,n) {
        cout << ans[i] << endl;
    }
}
