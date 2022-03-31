#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int height[9000];

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
    for (int i = 0; i < n; i++) {
        int h = query(l[i],r[i]);
        cout << h << endl;
    }
}
