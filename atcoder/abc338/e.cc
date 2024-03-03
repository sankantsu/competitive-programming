#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <stack>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    using P = pair<long, long>;
    vector<P> vs;
    rep(i,n) {
        long a, b;
        cin >> a >> b;
        a--; b--;
        if (a > b) {
            swap(a, b);
        }
        vs.emplace_back(a, b);
    }
    sort(vs.begin(), vs.end());

    bool check = false;
    stack<long> st;
    for (auto [i, j] : vs) {
        if (st.empty()) {
            st.push(j);
        }
        else if (i > st.top()) {
            while (!st.empty() && i > st.top()) {
                st.pop();
            }
            if (!st.empty() && st.top() < j) {
                check = true;
                break;
            }
            st.push(j);
        }
        else if (st.top() < j) {
            check = true;
            break;
        }
        else if (j < st.top()) {
            st.push(j);
        }
    }
    if (check) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
