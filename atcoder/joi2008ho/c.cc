// C - ダーツ
// https://atcoder.jp/contests/joi2008ho/tasks/joi2008ho_c
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const long max_n = 1000;

long n,m;
long p[max_n+10];

vector<long> sum;

bool check(long c) {
    /* cerr << "check: " << c << endl; */
    for (auto x : sum) {
        long y = c-x;
        auto it = lower_bound(sum.begin(),sum.end(),y);
        if (it != sum.end()) {
            auto yy = *it;
            auto score = x + yy;
            if (score <= m) {
                return true;
            }
        }
    }
    return false;
}

int main() {
    cin >> n >> m;
    p[0] = 0;
    rep(i,n) cin >> p[i+1];
    
    rep(i,n+1) rep(j,n+1) {
        sum.push_back(p[i]+p[j]);
    }
    sort(sum.begin(),sum.end());

    long lb = 0;
    long ub = m+1;
    while(ub - lb > 1) {
        long c = (lb+ub)/2;
        if (check(c)) lb = c;
        else ub = c;
    }
    cout << lb << endl;
}
