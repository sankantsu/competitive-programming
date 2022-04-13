#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n,k;
int a[200000];
int b[200000];

class BIT {
    public:
    BIT(int n_) : n(n_), v(n+1) {}
    void add(int a, int x) {
        while (a <= n) {
            v[a] += x;
            a += (a&-a);
        }
    }
    int sum(int a) {
        int s = 0;
        while (a > 0) {
            s += v[a];
            a -= (a&-a);
        }
        return s;
    }
    private:
    int n;
    vector<int> v;
};

class BIT2d {
    public:
    BIT2d(int h_, int w_) : h(h_), w(w_), vs(h+1,BIT(w)) {}
    void add(int a, int b, int x) {
        while (a <= h) {
            vs[a].add(b,x);
            a += (a&-a);
        }
    }
    int sum(int a, int b) {
        int s = 0;
        while (a > 0) {
            s += vs[a].sum(b);
            a -= (a&-a);
        }
        return s;
    }
    int sum(int h_min, int h_max, int w_min, int w_max) {
        int s1 = sum(h_max-1,w_max-1);
        int s2 = sum(h_min-1,w_max-1);
        int s3 = sum(h_max-1,w_min-1);
        int s4 = sum(h_min-1,w_min-1);
        return s1 - s2 - s3 + s4;
    }
    private:
    int h;
    int w;
    vector<BIT> vs;
};

int main() {
    constexpr int max_a = 1000;
    constexpr int max_b = 1000;
    cin >> n >> k;
    for (int i = 0; i < n; i++) cin >> a[i] >> b[i];

    BIT2d bit(max_a,max_b);
    for (int i = 0; i < n; i++) {
        bit.add(a[i],b[i],1);
    }
    int res = -1;
    for (int i = 1; i <= max_a-k; i++) {
        for (int j = 1; j <= max_b-k; j++) {
            res = max(res,bit.sum(i,i+k+1,j,j+k+1));
        }
    }
    cout << res << endl;
}
