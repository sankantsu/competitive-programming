#include <iostream>
#include <vector>
#include <string>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n, q;
    cin >> n >> q;

    vector<string> p(n);
    rep(i,n) cin >> p[i];

    vector<vector<int>> sum(n+1, vector<int>(n+1));
    rep(i,n) rep(j,n) {
        int x = (p[i][j] == 'B');
        sum[i+1][j+1] = sum[i+1][j] + sum[i][j+1] - sum[i][j] + x;
    }

    /* cerr << "sum:" << endl; */
    /* rep(i,n) { */
    /*     rep(j,n) cerr << sum[i+1][j+1] << " "; */
    /*     cerr << endl; */
    /* } */

    auto query = [&](int h1, int w1, int h2, int w2) {
        return sum[h2][w2] - sum[h1][w2] - sum[h2][w1] + sum[h1][w1];
    };

    rep(_,q) {
        int a, b, c, d;
        cin >> a >> b >> c >> d;
        long x1 = (a + n)/n;
        long y1 = (b + n)/n;
        long x2 = (c + 1)/n;
        long y2 = (d + 1)/n;
        /* cerr << "x1,y1,x2,y2: " << x1 << " " << y1 << " " << x2 << " " << y2 << endl; */
        
        long ans = 0;
        {
            long in = query(0, 0, n, n) * (x2 - x1) * (y2 - y1);
            long ul = query(a%n, b%n, n, n);
            long ur = query(a%n, 0, n, (d+1)%n);
            long ll = query(0, b%n, (c+1)%n, n);
            long lr = query(0, 0, (c+1)%n, (d+1)%n);
            long l = query(0, b%n, n, n) * (x2 - x1);
            long r = query(0, 0, n, (d+1)%n) * (x2 - x1);
            long u = query(a%n, 0, n, n) * (y2 - y1);
            long b = query(0, 0, (c+1)%n, n) * (y2 - y1);
            /* cerr << "in: " << in << endl; */
            /* cerr << "ul,ur,ll,lr: " << ul << " " << ur << " " << ll << " " << lr << endl; */
            /* cerr << "l,r,u,b: " << l << " " << r << " " << u << " " << b << endl; */
            ans = in + ul + ur + ll + lr + l + r + u + b;
        }
        cout << ans << endl;
    }
}
