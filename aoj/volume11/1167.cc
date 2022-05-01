// Pollock's conjecture (ポロック予想)
// https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=1167&lang=jp
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_n = 1000000;

vector<int> tetra;
bool is_tetra[max_n+1];

int dp[max_n+1];
int dp2[max_n+1];

void calc_tetra_numbers() {
    rep(i,max_n+1) is_tetra[i] = false;
    int i = 1;
    while (true) {
        int num = i*(i+1)*(i+2)/6;
        if (num > max_n) break;
        is_tetra[num] = true;
        tetra.push_back(num);
        i++;
    }
}

void pollock1() {
    for (int i = 1; i <= max_n; i++) {
        if (is_tetra[i]) {
            dp[i] = 1;
        }
        else {
            for (int j = 0; j < tetra.size() && tetra[j] < i; j++) {
                int t = tetra[j];
                dp[i] = min(dp[i],dp[t]+dp[i-t]);
            }
        }
    }
}

void pollock2() {
    for (int i = 1; i <= max_n; i++) {
        if (is_tetra[i] && (i%2 == 1)) {
            dp2[i] = 1;
        }
        else {
            for (int j = 0; j < tetra.size() && tetra[j] < i; j++) {
                int t = tetra[j];
                dp2[i] = min(dp2[i],dp2[t]+dp2[i-t]);
            }
        }
    }
}

void pollock() {
    const int inf = 1<<21;
    rep(i,max_n+1) {
        dp[i] = inf;
        dp2[i] = inf;
    }
    pollock1();
    pollock2();
}

int main() {
    calc_tetra_numbers();
    pollock();
    while(true) {
        int m;
        cin >> m;
        if (m == 0) break;
        cout << dp[m] << " " << dp2[m] << endl;
    }
}
