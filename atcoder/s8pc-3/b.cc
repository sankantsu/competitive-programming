// B - 石落としゲーム
// https://atcoder.jp/contests/s8pc-3/tasks/s8pc_3_b
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int h,w,K;
int board[50][50];
int initial[50][50];

int pow(int x, int y) {
    return (y <= 0) ? 1 : x*pow(x,y-1);
}

void drop() {
    rep(j,w) {
        vector<int> v;
        rep(i,h) {
            if (board[i][j] != 0) {
                v.push_back(board[i][j]);
            }
        }
        rep(i,h-v.size()) board[i][j] = 0;
        rep(i,v.size()) board[h-v.size()+i][j] = v[i];
    }
}

int erase(int cnt) {
    vector<vector<bool>> flag(h,vector<bool>(w,false));
    rep(i,h) {
        rep(j,w) {
            if (w < j+K) break;
            int num = board[i][j];
            for (int k = j; k < j+K; k++) {
                if (board[i][k] != num) break;
                if (k == j+K-1) {
                    for (int l = j; l < j+K; l++) flag[i][l] = true;
                }
            }
        }
    }
    int score = 0;
    rep(i,h) {
        rep(j,w) {
            if (flag[i][j]) {
                score += pow(2,cnt)*board[i][j];
                board[i][j] = 0;
            }
        }
    }
    return score;
}

int select(int i, int j) {
    int score = 0;
    int cnt = 0;
    board[i][j] = 0;
    while (true) {
        drop();
        int s = erase(cnt++);
        if (s == 0) break;
        score += s;
    }
    return score;
}

void init() {
    rep(i,h) rep(j,w) board[i][j] = initial[i][j];
}

int main() {
    cin >> h >> w >> K;
    string s;
    rep(i,h) {
        cin >> s;
        rep(j,w) initial[i][j] = s[j]-'0';
    }
    int res = 0;
    rep(i,h) rep(j,w) {
        init();
        int s = select(i,j);
        res = max(res,s);
    }
    cout << res << endl;
}
