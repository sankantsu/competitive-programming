#include <iostream>
#include <vector>
#include <set>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, q;
    cin >> n >> q;

    vector<long> a(n);

    set<long> st;
    long s = 0;
    rep(i, q) {
        long x;
        cin >> x; x--;

        if (st.count(x)) {
            a[x] += s;
            st.erase(x);
        }
        else {
            a[x] -= s;
            st.insert(x);
        }
        s += st.size();
    }
    for (auto x : st) {
        a[x] += s;
    }

    rep(i,n) {
        cout << a[i] << " ";
    }
    cout << endl;
}
