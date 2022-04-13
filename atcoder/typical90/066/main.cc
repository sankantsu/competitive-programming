// BIT + random WA

#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <random>

using namespace std;

int n;
int l[100];
int r[100];

int a[100];

struct BIT {
    BIT(int n_) : n(n_), bit(n+1) {}
    int sum(int i) {
        int s = 0;
        while (i > 0) {
            s += bit[i];
            i -= i&-i;
        }
        return s;
    }
    void add(int i, int x) {
        while (i <= n) {
            bit[i] += x;
            i += i&-i;
        }
    }
    private:
    int n;
    vector<int> bit;
};

int inversion_count(int a[], int n) {
    /* for (int i = 0; i < n; i++) { */
    /*     cout << a[i] << " "; */
    /* } cout << endl; */
    BIT bit(100);
    int ans = 0;
    for (int j = 0; j < n; j++) {
        ans += j - bit.sum(a[j]);
        bit.add(a[j],1);
        /* cout << ans << " "; */
    }
    /* cout << endl; */
    return ans;
}

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) cin >> l[i] >> r[i];

    random_device rnd;
    mt19937 mt;
    mt.seed(rnd());
    int sim = 500000;
    /* int sim = 1; */
    long sum = 0;
    for (int k = 0; k < sim; k++) {
        for (int i = 0; i < n; i++) {
            a[i] = l[i] + (mt()%(r[i]-l[i]+1));
        }
        int cnt = inversion_count(a,n);
        sum += cnt;
    }
    cout << fixed << setprecision(8) << (double)sum/sim << endl;
}
