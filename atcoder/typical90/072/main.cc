#include <iostream>

using namespace std;

int h,w;
int field[16][16];

int dp[1<<16][256];

int dx[4] = {1,0,-1,0};
int dy[4] = {0,-1,0,1};

int encode(int i, int j) {
    return w*i + j;
}

auto decode(int v) {
    return make_pair(v/w,v%w);
}

int bit_sum(int set) {
    int res = 0;
    while (set > 0) {
        if (set&1) {
            res++;
        }
        set >>= 1;
    }
    return res;
}

int dfs(int set, int start, int cur) {
    if (dp[set][cur] != 0) {
        return dp[set][cur];
    }
    if (set&(1<<start)) {
        int sum = bit_sum(set);
        // cout << "sum: " << sum << endl;
        if (sum < 3) {
            dp[set][cur] = -1;
            return -1;
        }
        else {
            dp[set][cur] = sum;
            return sum;
        }
    }
    int res = -1;
    auto [i,j] = decode(cur);
    for (int dir = 0; dir < 4; dir++) {
        int next_i = i + dx[dir];
        int next_j = j + dy[dir];
        if (next_i < 0 || h <= next_i || next_j < 0 || w <= next_j || field[next_i][next_j]) {
            continue;
        }
        int next = encode(next_i,next_j);
        if ((set>>next)&1) {
            continue;
        }
        int r = dfs(set|(1<<next),start,next);
        res = max(res,r);
    }
    dp[set][cur] = res;
    return res;
}

int main() {
    cin >> h >> w;
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            char c;
            cin >> c;
            if (c == '#') {
                field[i][j] = 1;
            }
        }
    }

    int res = -1;
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            int start = encode(i,j);
            if (!field[i][j]) {
                res = max(res,dfs(0,start,start));
            }
        }
    }

    cout << res << endl;
}
