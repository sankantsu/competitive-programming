#include <iostream>
#include <vector>
#include <cmath>
#include <algorithm>

using namespace std;

int n, q;
long x[100000];
long y[100000];
long x2[100000];
long y2[100000];
int query[100000];

// rotate 45 degree and enlarge sqrt(2) times
void rotate() {
    for (int i = 0; i < n; i++) {
        x2[i] = x[i] - y[i];
        y2[i] = x[i] + y[i];
    }
}

auto candidates() {
    int i1 = distance(x2,max_element(x2,x2+n));
    int i2 = distance(x2,min_element(x2,x2+n));
    int i3 = distance(y2,max_element(y2,y2+n));
    int i4 = distance(y2,min_element(y2,y2+n));
    return vector<int>{i1,i2,i3,i4};
}

int main() {
    cin >> n >> q;
    for (int i = 0; i < n; i++) {
        cin >> x[i] >> y[i];
    }
    for (int i = 0; i < q; i++) {
        cin >> query[i];
        query[i]--;
    }
    rotate();
    auto cand = candidates();
    for (int k = 0; k < q; k++) {
        int i = query[k];
        long d = 0;
        for (auto j : cand) {
            d = max(d,abs(x[i]-x[j])+abs(y[i]-y[j]));
        }
        cout << d << endl;
    }
}
