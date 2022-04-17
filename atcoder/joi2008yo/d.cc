// D - 星座探し
// https://atcoder.jp/contests/joi2008yo/tasks/joi2008yo_d
#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
int delta_x,delta_y;

using pos = pair<int,int>; // x,y

int main() {
    cin >> m;
    set<pos> constellation;
    rep(i,m) {
        int x,y;
        cin >> x >> y;
        constellation.emplace(x,y);
    }
    cin >> n;
    set<pos> stars;
    rep(i,n) {
        int x,y;
        cin >> x >> y;
        stars.emplace(x,y);
    }
    auto [x,y] = *constellation.begin();
    for (auto [xs,ys] : stars) {
        int dx = x - xs;
        int dy = y - ys;
        bool flag = true;
        for (auto [xc,yc] : constellation) {
            xc -= dx;
            yc -= dy;
            if (stars.find(make_pair(xc,yc)) == stars.end()) {
                flag = false;
                break;
            }
        }
        if (flag) {
            delta_x = -dx;
            delta_y = -dy;
            break;
        }
    }
    cout << delta_x << " " << delta_y << endl;
}
