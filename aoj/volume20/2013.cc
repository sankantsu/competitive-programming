// Osaki
// https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=2013
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int daysec = 60*60*24;

int hms2int(const string& t) {
    vector<int> v(t.size());
    rep(i,t.size()) v[i] = t[i] - '0';
    int h = 10*v[0]+v[1];
    int m = 10*v[3]+v[4];
    int s = 10*v[6]+v[7];
    return 60*60*h + 60*m + s;
}

bool solve() {
    int n;
    cin >> n;
    if (n == 0) return false;
    vector<string> s(n);
    vector<string> t(n);
    rep(i,n) cin >> s[i] >> t[i];
    vector<int> cnt(daysec+1);
    rep(i,n) {
        cnt[hms2int(s[i])]++;
        cnt[hms2int(t[i])]--;
    }
    int res = cnt[0];
    rep(i,cnt.size()-1) {
        cnt[i+1] = cnt[i]+cnt[i+1];
        res = max(res,cnt[i+1]);
    }
    cout << res << endl;
    return true;
}

int main() {
    while(solve()) {}
    return 0;
}
