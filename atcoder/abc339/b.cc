#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long h, w, n;
    cin >> h >> w >> n;
    
    long dir = 0;
    long delta[4][2] = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};

    vector<string> board(h, string(w, '.'));
    long i = 0, j = 0;
    rep(k,n) {
        /* cerr << "i,j: " << i << ", " << j << endl; */
        if (board[i][j] == '.') {
            board[i][j] = '#';
            dir = (dir + 1)%4;
        }
        else {
            board[i][j] = '.';
            dir = (dir + 3)%4;
        }
        auto [di,dj] = delta[dir];
        i = (i + di + h) % h;
        j = (j + dj + w) % w;
    }

    rep(k,h) {
        cout << board[k] << endl;
    }
}
