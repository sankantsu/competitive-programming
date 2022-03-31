#include <iostream>
#include <cmath>

using namespace std;

int h,w;
long a[100][100];
long b[100][100];

int main() {
    cin >> h >> w;
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            cin >> a[i][j];
        }
    }
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            cin >> b[i][j];
        }
    }
    bool ans = true;
    long cnt = 0;
    for (int i = 0; i < h-1; i++) {
        for (int j = 0; j < w; j++) {
            if (j == w-1) {
                if (a[i][j] != b[i][j]) {
                    ans = false;
                    goto END;
                }
                else {
                    continue;
                }
            }
            long diff = b[i][j] - a[i][j];
            cnt += abs(diff);
            a[i][j] += diff;
            a[i][j+1] += diff;
            a[i+1][j] += diff;
            a[i+1][j+1] += diff;
        }
    }
    for (int j = 0; j < w; j++) {
        if (a[h-1][j] != b[h-1][j]) {
            ans = false;
            break;
        }
    }
END:
    if (ans) {
        cout << "Yes" << endl;
        cout << cnt << endl;
    }
    else {
        cout << "No" << endl;
    }
}
