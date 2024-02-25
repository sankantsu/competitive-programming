#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n,q;
    cin >> n >> q;

    string s;
    cin >> s;

    set<long> same;
    rep(i,n-1) {
        if (s[i] == s[i+1]) {
            same.insert(i);
        }
    }

    /* std::cerr << "same: "; */
    /* for (auto x : same) { */
    /*     std::cerr << x << " "; */
    /* } */
    /* std::cerr << std::endl; */
    
    rep(i,q) {
        long type, l, r;
        cin >> type >> l >> r;
        l--; r--;
        if (type == 1) {
            // reverse if contains or not
            if (l > 0) {
                auto it = same.find(l-1);
                if (it != same.end()) {
                    same.erase(it);
                }
                else {
                    same.insert(l-1);
                }
            }
            if (r < n-1) {
                auto it = same.find(r);
                if (it != same.end()) {
                    same.erase(it);
                }
                else {
                    same.insert(r);
                }
            }
            /* std::cerr << "Q1: " << l << " " << r << std::endl; */
            /* std::cerr << "same: "; */
            /* for (auto x : same) { */
            /*     std::cerr << x << " "; */
            /* } */
            /* std::cerr << std::endl; */
        }
        else {
            auto it1 = same.lower_bound(l);
            /* std::cerr << "Q2: " << l << " " << r << std::endl; */
            if (it1 == same.end() || r <= *it1) {
                cout << "Yes" << endl;
            }
            else {
                cout << "No" << endl;
            }
        }
    }
}
