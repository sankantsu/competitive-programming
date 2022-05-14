#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const long max_n = 100000;

long n;
string s[max_n+10];
long t[max_n+10];

int main() {
    cin >> n;
    rep(i,n) cin >> s[i] >> t[i];

    long res = -1;
    long score = -1;
    set<string> ss;
    rep(i,n) {
        if (ss.find(s[i]) == ss.end()) {
            ss.insert(s[i]);
            if (score < t[i]) {
                res = i;
                score = t[i];
            }
        }
    }
    cout << res+1 << endl;
}
