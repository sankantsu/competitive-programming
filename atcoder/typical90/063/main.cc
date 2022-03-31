#include <iostream>
#include <vector>

using namespace std;

int h,w;
int p[8][10000];

int main() {
    cin >> h >> w;
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            cin >> p[i][j];
        }
    }

    int ans = -1;
    for (int s = 1; s < (1<<h); s++) {
        int rows = 0;
        for (int i = 0; i < h; i++) {
            if ((s>>i)&1) {
                rows++;
            }
        }
        vector<int> cnt(h*w);
        for (int j = 0; j < w; j++) {
            int x = -1;
            bool check = true;
            for (int i = 0; i < h; i++) {
                if (!((s>>i)&1)) {
                    continue;
                }
                if (x == -1) {
                    x = p[i][j];
                }
                else if (p[i][j] == x) {
                    continue;
                }
                else {
                    check = false;
                    break;
                }
            }
            if (check) {
                cnt[x-1]++;
            }
        }
        for (int x = 0; x < h*w; x++) {
            ans = max(ans,rows*cnt[x]);
        }
    }
    cout << ans << endl;
}
