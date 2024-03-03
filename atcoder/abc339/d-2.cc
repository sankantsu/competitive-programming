#include <iostream>
#include <vector>
#include <string>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

bool visited[60][60][60][60];

struct State {
    long d;
    long i1;
    long j1;
    long i2;
    long j2;
};

int main() {
    long n;
    cin >> n;

    vector<string> s(n);
    rep(i,n) {
        cin >> s[i];
    }
    
    long i1 = -1, j1, i2, j2;
    rep(i,n) {
        rep(j,n) {
            if (s[i][j] == 'P' && i1 == -1) {
                i1 = i;
                j1 = j;
            }
            else if (s[i][j] == 'P') {
                i2 = i;
                j2 = j;
            }
        }
    }

    constexpr long delta[4][2] = {{-1,0}, {0,1}, {1,0}, {0,-1}};

    queue<State> queue;
    queue.push(State{0, i1,j1,i2,j2});
    visited[i1][j1][i2][j2] = true;
    bool found = false;
    while (!queue.empty()) {
        auto [d,i,j,ii,jj] = queue.front();
        queue.pop();
        if (i == ii && j == jj) {
            found = true;
            cout << d << endl;
            break;
        }
        rep(dir,4) {
            long ni, nj, nii, njj;
            auto [di,dj] = delta[dir];
            ni = i + di;
            nj = j + dj;
            nii = ii + di;
            njj = jj + dj;
            if (ni < 0 || n <= ni || nj < 0 || n <= nj || s[ni][nj] == '#') {
                ni = i;
                nj = j;
            }
            if (nii < 0 || n <= nii || njj < 0 || n <= njj || s[nii][njj] == '#') {
                nii = ii;
                njj = jj;
            }
            if (visited[ni][nj][nii][njj]) {
                continue;
            }
            visited[ni][nj][nii][njj] = true;
            queue.push(State{d+1,ni,nj,nii,njj});
        }
    }
    if (!found) {
        cout << -1 << endl;
    }
}
