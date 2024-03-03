#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <queue>
#include <cmath>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

constexpr long max_n = 60;
bool visited[max_n][max_n][max_n][max_n];

// A* search state
struct State {
    long f;
    long g;
    long i1;
    long j1;
    long i2;
    long j2;
};

long h(long i1, long j1, long i2, long j2) {
    return std::abs(i2 - i1) + std::abs(j2 - j1);
}

int main() {
    long n;
    cin >> n;

    vector<string> s(n);
    rep(i,n) {
        cin >> s[i];
    }

    long i1, j1, i2, j2;
    long cnt = 0;
    rep(i,n) {
        rep(j,n) {
            if (s[i][j] == 'P' && cnt == 0) {
                i1 = i;
                j1 = j;
                cnt++;
            }
            else if (s[i][j] == 'P') {
                i2 = i;
                j2 = j;
            }
        }
    }

    long delta[4][2] = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};

    auto compare = [](const State& lhs, const State& rhs) {
        return lhs.f > rhs.f;
    };

    priority_queue<State, vector<State>, decltype(compare)> queue;
    bool found = false;
    queue.push(State{h(i1,j1,i2,j2), 0, i1, j1, i2, j2});
    while (!queue.empty()) {
        auto [f, g, i, j, ii, jj] = queue.top();
        /* cerr << "f,g,i,j,ii,jj: " << f << " " << g << " " << i << " " << j << " " << ii << " " << jj << endl; */
        queue.pop();

        if (i == ii && j == jj) {
            found = true;
            cout << g << endl;
            break;
        }
        
        if (visited[i][j][ii][jj]) {
            continue;
        }
        visited[i][j][ii][jj] = true;
        rep(dir, 4) {
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
            long ng = g + 1;
            long nf = ng + std::abs(nii - ni) + std::abs(njj - nj);
            /* cerr << "nf,ng,ni,nj,nii,njj: " << nf << " " << ng << " " << ni << " " << nj << " " << nii << " " << njj << endl; */
            queue.push(State{nf, ng, ni, nj, nii, njj});
        }
    }
    if (!found) {
        cout << -1 << endl;
    }
    return 0;
}
