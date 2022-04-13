#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,x,y;
int a[300000];

int main() {
    cin >> n >> x >> y;
    rep(i,n) cin >> a[i];
    int p = 0;
    int q = 0;
    long res = 0;
    const long inf = 1L<<60;
    vector<int> min_ids;
    vector<int> max_ids;
    while (q <= n) {
        if (q == n || a[q] < y || a[q] > x) {
            /* cout << "p,q: " << p << " " << q << endl; */
            /* cout << "size: " << min_ids.size() << " " << max_ids.size() << endl; */
            while (p < q) {
                auto it1 = lower_bound(min_ids.begin(),min_ids.end(),p);
                auto it2 = lower_bound(max_ids.begin(),max_ids.end(),p);
                long i = (it1 == min_ids.end()) ? inf : *it1;
                long j = (it2 == max_ids.end()) ? inf : *it2;
                /* cout << "i,j: " << i << " " << j << endl; */
                long k = max(i,j);
                if (k == inf) {
                    break;
                }
                else {
                    res += q-k;
                }
                p++;
            }
            min_ids.clear();
            max_ids.clear();
            p = q+1;
            q = p;
        }
        else {
            /* cout << "q,a[q]: " << q << " " << a[q] << endl; */
            if (a[q] == y) {
                min_ids.push_back(q);
            }
            if (a[q] == x) {
                max_ids.push_back(q);
            }
            q++;
        }
    }
    cout << res << endl;
}
