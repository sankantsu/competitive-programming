#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int h,w;
char s[100][100];

int main() {
    cin >> h >> w;
    vector<int> x,y;
    rep(i,h) rep(j,w) {
        cin >> s[i][j];
        if (s[i][j] == 'o') {
            x.push_back(i);
            y.push_back(j);
        }
    }
    int dx = abs(x[0]-x[1]);
    int dy = abs(y[0]-y[1]);
    cout << dx + dy << endl;
}
