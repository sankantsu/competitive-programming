#include <iostream>
#include <vector>
#include <algorithm>

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

int goal_time(const vector<int> &order) {
    if (check(order)) {
        int sum = 0;
        for (int i = 0; i < n; i++) {
            sum += a[order[i]][i];
        }
        return sum;
    }
    else {
        return inf;
    }
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

    vector<int> v;
    for (int i = 0; i < n; i++) {
        v.push_back(i);
    }

    int res = inf;
    do {
        int t = goal_time(v);
        res = min(res,t);
    } while(next_permutation(v.begin(),v.end()));

    if (res == inf) res = -1;
    cout << res << endl;
}
