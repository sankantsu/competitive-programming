#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

constexpr int max_n = 100000;

int n;
vector<int> g[max_n];

int reached[max_n];
vector<int> s1;
vector<int> s2;

void make_list(int cur, int c) {
    // cout << "make_list " << cur << " " << c << endl;
    if (reached[cur]) return;
    reached[cur] = 1;
    if (c) {
        s2.push_back(cur);
    }
    else {
        s1.push_back(cur);
    }
    for (auto adj : g[cur]) {
        make_list(adj,1-c);
    }
}

int main() {
    cin >> n;
    for (int i = 0; i < n-1; i++) {
        int u,v;
        cin >> u >> v;
        u--; v--;
        g[u].push_back(v);
        g[v].push_back(u);
    }

    make_list(0,1);

    auto print_vec = [](auto v) {
        for (int i = 0; i < n/2; i++) {
            cout << (v[i]+1) << " ";
        }
        cout << endl;
    };

    if (s1.size() > n/2) {
        sort(s1.begin(),s1.end());
        print_vec(s1);
    }
    else {
        sort(s2.begin(),s2.end());
        print_vec(s2);
    }
}
