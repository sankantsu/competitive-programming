#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n,k;
int a[200000];
int b[200000];

int cnt[5000][5000];
int sum[5000][5000];

int main() {
    constexpr int max_a = 5000;
    constexpr int max_b = 5000;
    cin >> n >> k;
    for (int i = 0; i < n; i++) {
        cin >> a[i] >> b[i];
        a[i]--; b[i]--;
    }

    for (int i = 0; i < n; i++) {
        cnt[a[i]][b[i]]++;
    }
    for (int i = 0; i < max_a; i++) {
        for (int j = 0; j < max_b; j++) {
            sum[i][j] = cnt[i][j];
        }
    }
    for (int i = 0; i < max_a; i++) {
        for (int j = 1; j < max_b; j++) {
            sum[i][j] = sum[i][j-1] + sum[i][j];
        }
    }
    for (int i = 1; i < max_a; i++) {
        for (int j = 0; j < max_b; j++) {
            sum[i][j] = sum[i-1][j] + sum[i][j];
        }
    }
    auto field_sum = [](int i_min, int i_max, int j_min, int j_max) {
        int s1,s2,s3,s4;
        s1 = sum[i_max-1][j_max-1];
        s2 = (i_min-1 >= 0) ? sum[i_min-1][j_max-1] : 0;
        s3 = (j_min-1 >= 0) ? sum[i_max-1][j_min-1] : 0;
        s4 = (i_min-1 >= 0 && j_min > 1) ? sum[i_min-1][j_min-1] : 0;
        return s1 - s2 - s3 + s4;
    };
    int res = -1;
    for (int i = 0; i < max_a-k; i++) {
        for (int j = 0; j < max_b-k; j++) {
            int s = field_sum(i,i+k+1,j,j+k+1);
            if (res < s) {
                res = s;
                /* cout << "i,j,s: " << i << " " << j << " " << s << endl; */
            }
        }
    }
    cout << res << endl;
}
