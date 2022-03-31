#include <iostream>
#include <algorithm>
#include <set>
#include <vector>

using namespace std;

constexpr int max_n = 10;
constexpr int inf = 100000;

int n,m;
int a[max_n][max_n]; // interval time
bool rumor[max_n][max_n];

bool check(const vector<int> &order) {
    for (int i = 0; i < n-1; i++) {
        if (rumor[order[i]][order[i+1]]) {
            return false;
        }
    }
    return true;
}

int goal_time_rec(set<int> s, vector<int> order) {
    if (s.empty()) {
        /* cout << "goal_time_rec:"; */
        /* cout << "order: " << endl; */
        /* for (int i = 0; i < n; i++) { */
        /*     cout << order[i] << " "; */
        /* } */
        /* cout << endl; */
        if (check(order)) {
            // cout << "check passed" << endl;
            int sum = 0;
            for (int i = 0; i < n; i++) {
                sum += a[order[i]][i];
            }
            return sum;
        }
        else {
            // cout << "check failed" << endl;
            return inf;
        }
    }
    int res = inf;
    for (auto x : s) {
        set<int> t = s;
        vector<int> v = order;
        t.erase(x);
        v.push_back(x);
        int time = goal_time_rec(move(t),move(v));
        res = min(res,time);
    }
    return res;
}

int goal_time() {
    set<int> s;
    for (int i = 0; i < n; i++) {
        s.insert(i);
    }
    return goal_time_rec(s,vector<int>{});
}

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cin >> a[i][j];
        }
    }

    cin >> m;
    for (int i = 0; i < m; i++) {
        int x,y;
        cin >> x >> y;
        x--; y--;
        rumor[x][y] = true;
        rumor[y][x] = true;
    }

    /* cout << "rumor:"  << endl; */
    /* for (int i = 0; i < n; i++) { */
    /*     for (int j = 0; j < n; j++) { */
    /*         cout << rumor[i][j] << " "; */
    /*     } */
    /*     cout << endl; */
    /* } */

    int res = goal_time();
    if (res < inf) {
        cout << res << endl;
    }
    else {
        cout << -1 << endl;
    }
}
