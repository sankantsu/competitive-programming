#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const long max_n = 100000;

long n;
long x[max_n+10];
long y[max_n+10];

template <typename T>
T abs(T x) {
    return (x >= 0) ? x : -x;
}

auto triangle_area(long i, long j) { // 頂点i,j,j+1の囲む三角形の面積(の8倍)
    auto x1 = x[i];
    auto y1 = y[i];
    auto x2 = x[j];
    auto y2 = y[j];
    auto x3 = x[(j+1)%n];
    auto y3 = y[(j+1)%n];
    auto area = 4*abs((x2-x1)*(y3-y1) - (x3-x1)*(y2-y1));
    return area;
}

auto calc_total_area() {
    long p = 0;
    long area = 0;
    for (long q = 1; q < n-1; q++) {
        area += triangle_area(p,q);
    }
    return area;
}

int main() {
    cin >> n;
    rep(i,n) cin >> x[i] >> y[i];

    auto total_area = calc_total_area();

    long p = 0;
    long q = 1;
    long area = 0;
    long res = total_area;
    while (true) {
        if (p == n) break;
        while (area < total_area/4) {
            area += triangle_area(p,q);
            q = (q+1)%n;
            res = min(res,abs(total_area/4 - area));
        }
        area -= triangle_area(q,p);
        p++;
        res = min(res,abs(total_area/4 - area));
    }
    cout << res << endl;
}
