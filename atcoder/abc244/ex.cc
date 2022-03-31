#include <iostream>
#include <vector>
#include <utility>
#include <cmath>

using namespace std;

constexpr int inf = pow(10,9);

int main() {
    int q;
    cin >> q;

    vector<pair<int,int>> v;
    for (int i = 0; i < q; i++) {
        int x,y,a,b;
        cin >> x >> y >> a >> b;
        v.push_back(make_pair(x,y));
        int max = -inf;
        for (auto p : v) {
            int r = a*p.first + b*p.second;
            if (r > max) {
                max = r;
            }
        }
        cout << max << endl;
    }
}
