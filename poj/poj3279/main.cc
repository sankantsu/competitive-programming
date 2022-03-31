#include <iostream>

using namespace std;

#define MAX_M 15
#define MAX_N 15

int a[MAX_M][MAX_N]; // input tile color
int buf[MAX_M][MAX_N];
int cnt; // flip count

int flipped[MAX_M][MAX_N]; // flipped tiles
int ans[MAX_M][MAX_N];

// initialize buf[][] and flipped[][] and cnt
void init_tiles() {
    cnt = 0;
    for (int i = 0; i < MAX_M; i++) {
        for (int j = 0; j < MAX_N; j++) {
            buf[i][j] = a[i][j];
            flipped[i][j] = 0;
        }
    }
}

// flip buf[i][j]
void flip_tile(int i, int j, int m, int n) {
    cnt++;
    flipped[i][j] = 1;
    if (i-1 >= 0) buf[i-1][j] = 1 - buf[i-1][j];
    if (j-1 >= 0) buf[i][j-1] = 1 - buf[i][j-1];
    if (true)     buf[i][j]   = 1 - buf[i][j];
    if (j+1 < n)  buf[i][j+1] = 1 - buf[i][j+1];
    if (i+1 < m)  buf[i+1][j] = 1 - buf[i+1][j];
}

int main() {
    int m,n;
    cin >> m >> n;

    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            int c;
            cin >> c;
            a[i][j] = c;
        }
    }
    int min = MAX_M*MAX_N + 1;
    
    // search all patterns for first line
    for (int k = 0; k < 1<<m; k++) {
        init_tiles();
        for (int j = 0; j < n; j++) {
            if (k & (0x1<<j)) {
                flip_tile(0,j,m,n);
            }
        }
        // operation for rest panels are uniquely determined
        for (int i = 1; i < m; i++) {
            for (int j = 0; j < n; j++) {
                if (buf[i-1][j]) {
                    flip_tile(i,j,m,n);
                }
            }
        }
        // check if last line tiles are while
        bool flag = true;
        for (int j = 0; j < n; j++) {
            if (buf[m-1][j] == 1) {
                flag = false;
                break;
            }
        }
        // debug
        /* cout << "k: " << k << endl; */
        /* cout << "flipped:" << endl; */
        /* for (int i = 0; i < m; i++) { */
        /*     for (int j = 0; j < n; j++) { */
        /*         cout << flipped[i][j]; */
        /*     } */
        /*     cout << endl; */
        /* } */
        /* cout << "buf:" << endl; */
        /* for (int i = 0; i < m; i++) { */
        /*     for (int j = 0; j < n; j++) { */
        /*         cout << buf[i][j]; */
        /*     } */
        /*     cout << endl; */
        /* } */
        /* cout << endl; */

        if (!flag) continue;
        if (cnt < min) {
            min = cnt;
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    ans[i][j] = flipped[i][j];
                }
            }
        }
    }
    if (min > m*n) {
        cout << "IMPOSSIBLE" << endl;
    }
    else {
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                cout << ans[i][j] << " ";
            }
            cout << endl;
        }
    }
}
