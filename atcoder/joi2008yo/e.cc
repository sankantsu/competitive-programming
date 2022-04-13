#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int r,c;
int a[20][20000];

int main() {
    cin >> r >> c;
    for (int i = 0; i < r; i++) {
        for (int j = 0; j < c; j++) {
            cin >> a[i][j];
        }
    }
    /* cout << "a:" << endl; */
    /* for (int i = 0; i < r; i++) { */
    /*     for (int j = 0; j < c; j++) { */
    /*         cout << a[i][j] << " "; */
    /*     } */
    /*     cout << endl; */
    /* } */

    int res = -1;
    for (int i = 0; i < (1<<r); i++) {
        int sum = 0;
        /* cout << "i: " << i << endl; */
        for (int j = 0; j < c; j++) {
            int cnt = 0; // 反転している枚数
            for (int k = 0; k < r; k++) {
                /* cout << "j,k,a[j][k]: " << j << " " << k << " " << a[k][j] << endl; */
                cnt += a[k][j]^((i>>k)&1);
            }
            /* cout << "j,cnt: " << j << " " << cnt << endl; */
            sum += max(cnt,r-cnt);
        }
        /* cout << i << " " << sum << endl; */
        res = max(res,sum);
    }
    cout << res << endl;
}
