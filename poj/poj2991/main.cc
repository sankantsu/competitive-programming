#include <vector>
#include <utility>
#include <cmath>
#include <cstdio>

using namespace std;

int n,c;

class stree {
    public:
    stree(int n, const vector<double> &len)
        : vx(4*n), vy(4*n), angle(4*n)
    {
        init(0,0,n,len);
    }
    pair<double,double> get_endpoint() {
        return make_pair(vx[0],vy[0]);
    }
    void change_angle(int v, double a) {
        change_angle_rec(0,0,n,v,a);
    }
    private:
    void init(int k, int l, int r, const vector<double> &len) {
        if (r - l == 1) {
            vy[k] = len[l];
            return;
        }
        int left = 2*k + 1;
        int right = 2*k + 2;
        int m = (l+r)/2;
        init(left,l,m,len);
        init(right,m,r,len);
        vy[k] = vy[left] + vy[right];
    }
    void change_angle_rec(int k, int l, int r, int v, double a) {
        if (v <= l || r <= v) {
            return;
        }
        int left = 2*k + 1;
        int right = 2*k + 2;
        int m = (l+r)/2;
        change_angle_rec(left,l,m,v,a);
        change_angle_rec(right,m,r,v,a);
        if (v <= m) angle[k] += a;
        double c = cos(angle[k]);
        double s = sin(angle[k]);
        vx[k] = vx[left] + c*vx[right] - s*vy[right];
        vy[k] = vy[left] + s*vx[right] + c*vy[right];
    }
    vector<double> vx;
    vector<double> vy;
    vector<double> angle;
};

bool solve() {
    int cnt = scanf("%d %d",&n,&c);
    if (cnt < 2) {
        return false;
    }
    vector<double> l(n);
    vector<double> cur_angle(n);
    for (int i = 0; i < n; i++) scanf("%lf",&l[i]);

    double pi = 4*atan(1);
    for (int i = 0; i < n; i++) {
        cur_angle[i] = pi;
    }
    stree st(n,l);

    for (int i = 0; i < c; i++) {
        int v;
        double a;
        scanf("%d %lf",&v,&a);
        a = a*pi/180;
        st.change_angle(v,a - cur_angle[v]);
        cur_angle[v] = a;
        pair<double,double> pos = st.get_endpoint();
        double x = pos.first;
        double y = pos.second;
        printf("%.2f %.2f\n",x,y);
    }
    printf("\n");
    return true;
}

int main() {
    while (solve()) {}
    return 0;
}
