#include <iostream>
#include <deque>
#include <utility>

using namespace std;

int n,q;

int main() {
    cin >> n >> q;
    deque<int> d(n);
    for (int i = 0; i < n; i++) {
        cin >> d[i];
    }
    for (int i = 0; i < q; i++) {
        int t,x,y;
        cin >> t >> x >> y;
        x--; y--;
        if (t == 1) {
            swap(d[x],d[y]);
        }
        else if (t == 2) {
            int a;
            a = d.back(); d.pop_back();
            d.push_front(a);
        }
        else if (t == 3) {
            cout << d[x] << endl;
        }
        else {
            throw;
        }
    }
}
