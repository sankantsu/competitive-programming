#include <iostream>
#include <algorithm>

using namespace std;

int w, n;

int w_;
int min_height[1<<21];
int max_height[1<<21];

void init(int w) {
    w_ = 1;
    while (w_ < w) {
        w_ <<= 1;
    }
}

void update(int a, int b, int h, int k, int l, int r) {
    if (r <= a || b <= l) {
        /* cout << "l,r = " << l << " " << r << endl; */
        return;
    }
    else if (a <= l && r <= b) {
        /* cout << "l,r = " << l << " " << r << endl; */
        /* cout << "min = max = " << h << endl; */
        min_height[k] = h;
        max_height[k] = h;
        return;
    }
    else {
        /* cout << "l,r = " << l << " " << r << endl; */
        /* cout << "max = " << h << endl; */
        if (max_height[k] < h) {
            max_height[k] = h;
        }
        update(a,b,h,2*k+1,l,(l+r)/2);
        update(a,b,h,2*k+2,(l+r)/2,r);
    }
}

int query(int a, int b, int k, int l, int r) {
    if (r <= a || b <= l) {
        return 0;
    }
    else if (a <= l && r <= b) {
        /* cout << "l,r: " << l << " " << r << endl; */
        /* cout << "h: " << max_height[k] << endl; */
        return max_height[k];
    }
    else {
        int h = min_height[k];
        if (h == max_height[k]) {
            return h;
        }
        int h1 = query(a,b,2*k+1,l,(l+r)/2);
        int h2 = query(a,b,2*k+2,(l+r)/2,r);
        /* cout << "l,r: " << l << " " << r << endl; */
        /* cout << "h,h1,h2: " << h << " " << h1 << " " << h2 << endl; */
        h = max({h,h1,h2});
        return h;
    }
}

int main() {
    cin >> w >> n;

    init(w);
    for (int i = 0; i < n; i++) {
        // debug
        /* for (int x = 0; x < w; x++) { */
        /*     cout << query(x,x+1,0,0,w_) << " "; */
        /* } */
        /* cout << endl; */

        int l,r;
        cin >> l >> r; l--;
        /* cout << "query " << l << " " << r << endl; */
        int h = 1 + query(l,r,0,0,w_);

        update(l,r,h,0,0,w_);
        cout << h << endl;
    }
}
