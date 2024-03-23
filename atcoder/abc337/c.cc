#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<int> a(n);
    rep(i,n) {
        cin >> a[i];
        if (a[i] > 0) a[i]--;
    }

    set<int> s(a.begin(), a.end());
    int last = -1;
    rep(i,n) {
        if (!s.contains(i)) {
            last = i;
            break;
        }
    }

    vector<int> v;
    v.push_back(last);
    while (true) {
        int cur = v[v.size() - 1];
        int prev = a[cur];
        if (prev == -1) {
            break;
        }
        v.push_back(prev);
    }
    reverse(v.begin(), v.end());

    for (auto x : v) {
        cout << x + 1 << " ";
    }
    cout << endl;
}
