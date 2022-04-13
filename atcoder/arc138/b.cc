#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>
#include <unordered_map>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
int a[300000];

struct state {
    state(int c, int l, int f)
        : cur(c), last(l), flip(f) {}
    int cur;
    int last;
    int flip;
    bool operator==(const state& rhs) const {
        return cur == rhs.cur && last == rhs.last && flip == rhs.flip;
    }
};

struct hasher {
    size_t operator()(const state& s) const {
        auto h = hash<int>();
        return h(s.cur) ^ h(s.last) ^ h(s.flip);
    }
};

int main() {
    cin >> n;
    rep(i,n) {
        cin >> a[i];
    }
    bool ans = false;
    queue<state> q;
    unordered_map<state,bool,hasher> mp;
    q.emplace(0,n-1,0);
    while (!q.empty()) {
        auto s = q.front(); q.pop();
        if (mp.find(s) != mp.end()) {
            continue;
        }
        mp[s] = true;
        auto [cur,last,flip] = s;
        if (a[cur]^flip) {
            continue;
        }
        if (cur == last) {
            ans = true;
            break;
        }
        if (a[last]^flip) {
            q.emplace(cur+1,last,flip^1);
        }
        else {
            q.emplace(cur+1,last,flip^1);
            q.emplace(cur,last-1,flip);
        }
    }
    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
