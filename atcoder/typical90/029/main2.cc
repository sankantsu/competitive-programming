#include <iostream>
#include <vector>
#include <map>
#include <algorithm>

using namespace std;

map<int,int> height; // x coordinate -> height

int query(int l, int r) {
    auto lb = height.upper_bound(l); lb--;
    auto ub = height.lower_bound(r);
    int h = 0;
    for (auto it = lb; it != ub; it++) {
        h = max(h,it->second);
    }
    int h_new = h+1;
    int h_last = prev(height.upper_bound(r))->second;
    auto [it1,b1] = height.insert_or_assign(l,h_new);
    auto [it2,b2] = height.insert_or_assign(r,h_last);
    height.erase(next(it1),it2);
    return h_new;
}

int main() {
    int w,n;
    cin >> w >> n;
    vector<int> l(n);
    vector<int> r(n);
    for (int i = 0; i < n; i++) {
        int l_,r_;
        cin >> l_ >> r_; l_--;
        l[i] = l_;
        r[i] = r_;
    }
    height.emplace(0,0);
    for (int i = 0; i < n; i++) {
        int h = query(l[i],r[i]);
        cout << h << endl;
    }
}
