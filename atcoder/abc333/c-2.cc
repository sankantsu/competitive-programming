#include <iostream>
#include <vector>
#include <set>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

constexpr long make_repunit(int n) {
    long res = 0;
    while (n-- > 0) {
        res *= 10;
        res += 1;
    }
    return res;
}

int main() {
    int n;
    cin >> n;

    constexpr int max_digit = 12;
    vector<long> rs;
    rep(i, max_digit) {
        rs.push_back(make_repunit(i+1));
    }

    set<long> s;
    for (auto x : rs) for (auto y : rs) for (auto z : rs) {
        s.insert(x + y + z);
    }

    vector<long> vs(s.begin(), s.end());
    cout << vs[n-1] << endl;
}
