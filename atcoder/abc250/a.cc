#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int h,w;
int r,c;

int dx[] = {1,0,-1,0};
int dy[] = {0,-1,0,1};

int main() {
    cin >> h >> w;
    cin >> r >> c; r--; c--;
    int cnt = 0;
    rep(d,4) {
        int x = r + dx[d];
        int y = c + dy[d];
        if (0 <= x && x < h && 0 <= y && y < w) cnt++;
    }
    cout << cnt << endl;
}
