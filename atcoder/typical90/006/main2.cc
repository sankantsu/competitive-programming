#include <iostream>
#include <vector>
#include <string>
#include <utility>

using namespace std;

template <typename T>
class RMQ {
    public:
    RMQ(int n, T max_val) : max_(max_val) {
        int size = 1;
        while (size < n) {
            size <<= 1;
        }
        size = 2*size-1;
        data = vector<T>(size,max_);
        index = vector<int>(size);
        init_index(0,0,1+size/2);
    }
    // return minimum value and index
    auto query(int a, int b) {
        int n = 1+data.size()/2;
        return query_rec(0,0,n,a,b);
    }
    void update(int k, int a) {
        k += data.size()/2;
        data[k] = a;
        while (k > 0) {
            k = (k-1)/2;
            T left = data[2*k+1];
            T right = data[2*k+2];
            if (left <= right) {
                data[k] = left;
                index[k] = index[2*k+1];
            }
            else {
                data[k] = right;
                index[k] = index[2*k+2];
            }
        }
    }
    private:
    void init_index(int k, int l, int r) {
        index[k] = l;
        if (r - l > 1) {
            init_index(2*k+1,l,(l+r)/2);
            init_index(2*k+2,(l+r)/2,r);
        }
    }
    auto query_rec(int k, int l, int r, int a, int b) {
        if (r <= a || b <= l) {
            return make_pair(max_,-1);
        }
        else if (a <= l && r <= b) {
            return make_pair(data[k],index[k]);
        }
        else {
            int left = 2*k+1;
            int right = 2*k+2;
            int m = (l+r)/2;
            auto res1 = query_rec(left,l,m,a,b);
            auto res2 = query_rec(right,m,r,a,b);
            return (res1.first <= res2.first) ? res1 : res2;
        }
    }
    T max_;
    vector<T> data;
    vector<int> index;
};

int main() {
    int n,k;
    string s;
    cin >> n >> k;
    cin >> s;

    RMQ<char> rmq(n,static_cast<char>('z'+1));
    for (int i = 0; i < n; i++) {
        rmq.update(i,s[i]);
    }

    string t;
    int cur = 0;
    for (int i = 0; i < k; i++) {
        auto res = rmq.query(cur,n-k+i+1);
        t.push_back(res.first);
        cur = res.second + 1;
    }

    cout << t << endl;
}
