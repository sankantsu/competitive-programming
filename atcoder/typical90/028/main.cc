#include <iostream>

using namespace std;

constexpr int max_n = 100000;
constexpr int max_x = 1000;
constexpr int max_y = 1000;

int n;
int a[max_x+1][max_y+1];
int cnt[max_n+1];

void print_board() {
    cout << "print_board:" << endl;
    int lim = 10;
    for (int i = 0; i < lim; i++) {
        for (int j = 0; j < lim; j++) {
            cout << a[i][j] << " ";
        }
        cout << endl;
    }
}

int main() {
    cin >> n;
    for (int k = 0; k < n; k++) {
        int lx,ly,rx,ry;
        cin >> lx >> ly >> rx >> ry;
        a[lx][ly]++;
        a[lx][ry]--;
        a[rx][ly]--;
        a[rx][ry]++;
    }

    // print_board();

    for (int i = 0; i < max_x; i++) {
        for (int j = 1; j < max_y; j++) {
            a[i][j] = a[i][j-1] + a[i][j];
        }
    }

    // print_board();

    for (int j = 0; j < max_y; j++) {
        for (int i = 1; i < max_x; i++) {
            a[i][j] = a[i-1][j] + a[i][j];
        }
    }

    // print_board();

    for (int i = 0; i < max_x; i++) {
        for (int j = 0; j < max_y; j++) {
            cnt[a[i][j]]++;
        }
    }

    for (int k = 1; k <= n; k++) {
        cout << cnt[k] << endl;
    }
}
