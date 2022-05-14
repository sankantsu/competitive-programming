// Cut the Cake (ケーキカット)
// https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=1149&lang=jp
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

struct piece {
    piece(long n_, long w_, long d_) : cut_num(n_), w(w_), d(d_) {
        area = w*d;
    }
    long cut_num; // 何回目のカットでできたか
    long w;
    long d;
    long area;
    auto cut_pos(long s) {
        s = s%(w+d);
        if (s < w) {
            return make_pair(0,s);
        }
        else {
            return make_pair(1,s-w);
        }
    }
};

bool operator< (piece lhs, piece rhs) {
    if (lhs.cut_num != rhs.cut_num) {
        return lhs.cut_num < rhs.cut_num;
    }
    else {
        return lhs.area < rhs.area;
    }
}

bool solve() {
    long n,w,d;
    cin >> n >> w >> d;
    if (n == 0 && w == 0 && d == 0) return false;

    vector<piece> v;
    v.emplace_back(0,w,d);
    auto cut = [n,&v](long i, long p, long s) {
        v[p].cut_num = 2*n;
        auto [h,pos] = v[p].cut_pos(s);
        if (h == 0) {
            v.push_back(piece{i,pos,v[p].d});
            v.push_back(piece{i,v[p].w-pos,v[p].d});
        }
        else {
            v.push_back(piece{i,v[p].w,pos});
            v.push_back(piece{i,v[p].w,v[p].d-pos});
        }
    };
    rep(i,n) {
        long p,s;
        cin >> p >> s; p--;
        cut(i,p,s);
        sort(v.begin(),v.end());
    }
    v.erase(next(v.begin(),n+1),v.end());
    sort(v.begin(),v.end(),[](auto lhs, auto rhs) {
        return lhs.area < rhs.area;
    });
    rep(i,n+1) {
        cout << v[i].area;
        cout << ((i == n) ? "\n" : " ");
    }
    return true;
}

int main() {
    while(solve());
}
