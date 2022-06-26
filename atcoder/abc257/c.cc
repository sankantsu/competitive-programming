#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
string s;
int w[300000];

int main() {
    cin >> n;
    cin >> s;
    rep(i,n) cin >> w[i];

    vector<int> w_ad;
    vector<int> w_ch;

    rep(i,n) {
        if (s[i] == '1') {
            w_ad.push_back(w[i]);
        }
        else {
            w_ch.push_back(w[i]);
        }
    }
    sort(w_ad.begin(),w_ad.end());
    sort(w_ch.begin(),w_ch.end());

    w[n] = 1<<30;
    int res = -1;
    rep(i,n+1) {
        int x = w[i];
        int ad = distance(lower_bound(w_ad.begin(),w_ad.end(),x),w_ad.end());
        int ch = distance(w_ch.begin(),lower_bound(w_ch.begin(),w_ch.end(),x));
        /* cout << w[i] << " " << ad << " " << ch << endl; */
        res = max(res,ad+ch);
    }
    cout << res << endl;
}
