#include <iostream>

using namespace std;

constexpr int max_h = 2000;
constexpr int max_w = 2000;

int h, w;
int a[max_h][max_w];

int row[max_h];
int col[max_w];
int ans[max_h][max_w];

int main() {
    cin >> h >> w;
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            cin >> a[i][j];
        }
    }
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            row[i] += a[i][j];
            col[j] += a[i][j];
        }
    }
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            ans[i][j] = row[i] + col[j] - a[i][j];
        }
    }
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            cout << ans[i][j] << " ";
        }
        cout << endl;
    }
}
