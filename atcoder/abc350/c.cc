#include <iostream>
#include <vector>
#include <map>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<int> a(n);
    rep(i,n) {
        cin >> a[i];
        a[i]--;
    }

    map<int, int> pos;
    rep(i,n) {
        pos[a[i]] = i;
    }
    /* cerr << "pos: "; rep(i,n) cerr << pos[i] << " "; cerr << endl; */

    using P = pair<int, int>;
    vector<P> ans;
    rep(i,n) {
        int p = pos[i];
        if (i == p) continue;
        ans.emplace_back(i, p);

        int x = a[i];
        swap(a[i], a[p]);
        pos[x] = p;
    }

    /* cerr << "a: "; rep(i,n) cerr << a[i] << " "; cerr << endl; */

    cout << ans.size() << endl;
    for (auto [i,j] : ans) {
        cout << i + 1 << " " << j + 1 << endl;
    }
}
