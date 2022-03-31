#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int height[18000];

template <typename Vec>
void compress(Vec &l, Vec &r) {
    int n = l.size();
    Vec v;
    for (int i = 0; i < n; i++) {
        v.push_back(l[i]);
        v.push_back(r[i]);
    }
    sort(v.begin(),v.end());
    auto it = unique(v.begin(),v.end());
    v.erase(it,v.end());
    for (int i = 0; i < n; i++) {
        l[i] = distance(v.begin(),lower_bound(v.begin(),v.end(),l[i]));
        r[i] = distance(v.begin(),lower_bound(v.begin(),v.end(),r[i]));
    }
}

int query(int l, int r) {
    int h = *(max_element(height+l,height+r));
    h++;
    fill(height+l,height+r,h);
    return h;
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
    compress(l,r);
    for (int i = 0; i < n; i++) {
        int h = query(l[i],r[i]);
        cout << h << endl;
    }
}
