#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>

using namespace std;

int n;
int l[100];
int r[100];

double expectation(int i, int j) {
    int res = 0;
    for (int x = l[i]; x <= r[i]; x++) {
        for (int y = l[j]; y <= r[j]; y++) {
            if (x > y) res++;
        }
    }
    return (double)res/((r[i]-l[i]+1)*(r[j]-l[j]+1));
}

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) cin >> l[i] >> r[i];

    double ans = 0;
    for (int i= 0; i < n; i++) {
        for (int j = i+1; j < n; j++) {
            ans += expectation(i,j);
        }
    }
    cout << fixed << setprecision(8) << ans << endl;
}
