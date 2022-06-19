#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

struct interval {
    int l;
    int r;
};

int n;

int main() {
    cin >> n;
    vector<interval> v;
    rep(i,n) {
        int l,r;
        cin >> l >> r;
        v.push_back(interval{l,r});
    }

    sort(v.begin(),v.end(),[](interval lhs, interval rhs) { return lhs.l < rhs.l; });

    int x = -1;
    int y = -1;
    for (auto [l,r] : v) {
        /* cerr << "l,r: " << l << " " << r << endl; */
        if (x == -1) {
            x = l;
            y = r;
        }
        else if (l <= y) { // merge interval
            y = max(y,r);
        }
        else {
            cout << x << " " << y << endl;
            x = l;
            y = r;
        }
    }
    cout << x << " " << y << endl;
}
