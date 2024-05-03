#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <stack>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    stack<long> st;
    rep(i,n) {
        long x = a[i];
        while (!st.empty() && x == st.top()) {
            st.pop();
            x++;
        }
        st.push(x);
    }
    cout << st.size() << endl;
}
