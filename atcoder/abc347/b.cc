#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <set>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    string s;
    cin >> s;

    set<string> st;
    for (int i = 0; i < s.size(); i++) {
        for (int j = i + 1; j <= s.size(); j++) {
            st.insert(s.substr(i,j-i));
        }
    }
    /* for (auto t : st) cerr << t << endl; */
    cout << st.size() << endl;
}
