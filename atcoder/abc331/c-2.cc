#include <iostream>
#include <vector>
#include <map>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n;
    cin >> n;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    map<long, vector<int>> il;
    rep(i,n) il[-a[i]].push_back(i);

    long s = 0;
    vector<long> b(n);
    for (auto& [x, l] : il) {
        for (auto i : l) b[i] = s;
        s += (-x) * l.size();
    }
    rep(i,n) cout << b[i] << " "; cout << endl;
}
