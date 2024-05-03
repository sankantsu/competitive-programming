#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <stack>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

char flip_case(char c) {
    if (std::isupper(c)) 
        return std::tolower(c); 
    else
        return std::toupper(c); 
}

int main() {
    string s;
    cin >> s;

    long n = s.size();
    vector<long> pair(n, -1);

    {
        stack<long> left;
        rep(i,n) {
            if (s[i] == '(') {
                left.push(i);
            }
            else if (s[i] == ')') {
                long l = left.top();
                left.pop();
                pair[l] = i;
                pair[i] = l;
            }
        }
    }
    /* cerr << "pair: "; rep(i,n) cerr << pair[i] << " "; cerr << endl; */

    string ans;
    long p = 0;
    stack<long> st;
    while(p < n) {
        long depth = st.size();
        long d = (depth%2 == 0) ? 1 : -1;
        char c = s[p];
        /* cerr << "p,c: " << p << " " << c << endl; */
        if (c == '(' || c == ')') {
            long q = pair[p];
            if (!st.empty() && st.top() == q) {
                // close paren
                st.pop();
            }
            else {
                // open paren
                st.push(p);
            }
            p = q - d;
        }
        else {
            c = (depth%2 == 0) ? c : flip_case(c);
            ans.push_back(c);
            p += d;
        }
    }
    cout << ans << endl;
}
