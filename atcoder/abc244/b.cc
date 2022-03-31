#include <iostream>
#include <string>
#include <vector>

using namespace std;

struct vel {
    int dx;
    int dy;
};

void solve(int n, const string& t) {
    vector<vel> vs = {{1,0},{0,-1},{-1,0},{0,1}};
    int idx = 0;

    int x = 0;
    int y = 0;

    for (int i = 0; i < n; i++) {
        char c = t[i];
        if (c == 'S') {
            x += vs[idx].dx;
            y += vs[idx].dy;
        }
        else {
            idx = (idx + 1)%4;
        }
    }
    cout << x << " " << y << endl;
}

int main() {
    int n;
    string t;

    cin >> n;
    cin >> t;

    solve(n,t);
}
