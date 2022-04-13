// 最古の遺跡
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
int x[3000];
int y[3000];

constexpr int max_x = 5000;
int field[max_x+1][max_x+1];

bool range_check(int x1, int y1) {
    return 0 <= x1 && x1 <= max_x && 0 <= y1 && y1 <= max_x;
}

int main() {
    cin >> n;
    rep(i,n) {
        cin >> x[i] >> y[i];
    }
    rep(i,n) {
        field[x[i]][y[i]] = 1;
    }
    long ans = 0;
    for (int i = 0; i < n; i++) {
        for (int j = i+1; j < n; j++) {
            int dx = x[j]-x[i];
            int dy = y[j]-y[i];
            int sign[] = {-1,1};
            for (auto s : sign) {
                int x1 = x[i]+s*dy; int y1 = y[i]-s*dx;
                int x2 = x[j]+s*dy; int y2 = y[j]-s*dx;
                if (!range_check(x1,y1) || !range_check(x2,y2)) {
                    continue;
                }
                if (field[x1][y1] && field[x2][y2]) {
                    long area = dx*dx+dy*dy;
                    ans = max(ans,area);
                }
            }
        }
    }
    cout << ans << endl;
}
