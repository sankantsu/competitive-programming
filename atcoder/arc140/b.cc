#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
string s;

auto run_length() {
    string cs;
    vector<int> run_length;
    int cnt = 1;
    char last = s[0];
    for (int i = 1; i < n; i++) {
        char cur = s[i];
        if (cur == last) {
            cnt++;
        }
        else {
            cs.push_back(last);
            run_length.push_back(cnt);
            cnt = 1;
        }
        last = cur;
    }
    cs.push_back(last);
    run_length.push_back(cnt);
    return make_pair(cs,run_length);
}

auto count() {
    multiset<int> sc;
    auto [cs,rl] = run_length();
    for (int i = 0; i < (int)cs.size()-2; i++) {
        if (cs[i] == 'A' && cs[i+1] == 'R' && cs[i+2] == 'C') {
            if (rl[i+1] == 1) {
                sc.insert(min(rl[i],rl[i+2]));
            }
        }
    }
    return sc;
}

int main() {
    cin >> n;
    cin >> s;

    auto cnt = count();

    int i = 1;
    int res = 0;
    while(!cnt.empty()) {
        if (i%2 == 1) {
            auto it = prev(cnt.end());
            int c = *it;
            cnt.erase(it);
            if (c > 1) cnt.insert(c-1);
            res++;
        }
        else {
            auto it = cnt.begin();
            cnt.erase(it);
            res++;
        }
        i++;
    }
    cout << res << endl;
}
