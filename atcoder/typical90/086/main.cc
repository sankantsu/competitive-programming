#include <iostream>
#include <vector>
#include <algorithm>
#include <atcoder/modint>

using namespace std;
using mint = atcoder::modint1000000007;

int n, q;
int x[50];
int y[50];
int z[50];
long w[50];

struct restriction {
    int x;
    int y;
    int z;
    int w;
};

vector<restriction> rv[61];
mint counts[61];

void make_restrictions() {
    for (int i = 0; i < q; i++) {
        for (int j = 0; j < 60; j++) {
            int a = (w[i]>>j)&1;
            rv[j].push_back(restriction{x[i],y[i],z[i],a});
        }
    }
}

void solve() {
    for (int j = 0; j < 60; j++) {
        int s = (1<<n)-1;
        for (auto r : rv[j]) {
            auto [x,y,z,w] = r;
            if (w == 0) {
                int t = (1<<x)|(1<<y)|(1<<z);
                s = (s&~t);
            }
        }
        int cnt = 0;
        int k = s;
        bool flag;
        do {
            flag = true;
            for (auto r : rv[j]) {
                auto [x,y,z,w] = r;
                if (w == 0) continue;
                int t = (1<<x)|(1<<y)|(1<<z);
                if ((k&t) == 0) {
                    flag=false;
                    break;
                }
            }
            if (flag) {
                cnt++;
            }
            k = (k-1)&s;
        } while(k != s);
        counts[j] = cnt;
    }
    mint ans = 1;
    for (int j = 0; j < 60; j++) {
        ans *= counts[j];
    }
    cout << ans.val() << endl;
}

int main() {

    cin >> n >> q;
    for (int i = 0; i < q; i++) {
        cin >> x[i] >> y[i] >> z[i] >> w[i];
        x[i]--; y[i]--; z[i]--;
    }
    make_restrictions();
    solve();
}
