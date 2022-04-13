#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n, m;
vector<int> s[10];
int p[10];

int main() {
    cin >> n >> m;
    for (int i = 0; i < m; i++) {
        int k;
        cin >> k;
        for (int j = 0; j < k; j++) {
            int ss;
            cin >> ss; ss--;
            s[i].push_back(ss);
        }
    }
    for (int i = 0; i < m; i++) {
        cin >> p[i];
    }

    int res = 0;
    for (int i = 0; i < (1<<n); i++) {
        vector<bool> light(m,false);
        for (int j = 0; j < m; j++) {
            int cnt = 0;
            for (auto ss : s[j]) {
                if ((i>>ss)&1) {
                    cnt++;
                }
            }
            if (cnt%2 != p[j]) {
                break;
            }
            if (j == m-1) {
                res++;
            }
        }
    }
    cout << res << endl;
}
