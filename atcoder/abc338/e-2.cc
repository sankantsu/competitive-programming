#include <iostream>
#include <vector>
#include <algorithm>
#include <stack>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    using P = pair<long, long>;
    vector<P> vs(2*n);
    rep(id, n) {
        long a, b;
        cin >> a >> b;
        a--; b--;
        if (a > b) {
            swap(a, b);
        }
        vs[a] = {0, id};  // left
        vs[b] = {1, id};  // right
    }

    bool check = false;
    stack<long> st;
    rep(i, 2*n) {
        auto [right, id] = vs[i];
        if (!right) {
            st.push(id);
        }
        else {
            assert(!st.empty());
            if (st.top() != id) {
                check = true;
                break;
            }
            st.pop();
        }
    }
    if (check) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
