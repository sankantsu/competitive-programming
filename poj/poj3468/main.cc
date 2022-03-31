#include <cstdio>

using namespace std;

int n, n_, q;
long long data[1<<20];
long long lazy[1<<20];

void init() {
    n = 1;
    while (n < n_) {
        n <<= 1;
    }
}

void eval(int k, int l, int r) {
    if (lazy[k] == 0) {
        return;
    }
    if (r - l > 1) {
        lazy[2*k+1] += lazy[k];
        lazy[2*k+2] += lazy[k];
    }
    data[k] += (long long)(r-l)*lazy[k];
    lazy[k] = 0;
}

long long query(int a, int b, int k, int l, int r) {
    eval(k,l,r);
    if (r <= a || b <= l) {
        return 0LL;
    }
    else if (a <= l && r <= b) {
        return data[k];
    }
    else {
        int m = (l+r)/2;
        long long s1 = query(a,b,2*k+1,l,m);
        long long s2 = query(a,b,2*k+2,m,r);
        long long s = s1 + s2;
        return s;
    }
}

void update(int a, int b, int c, int k, int l, int r) {
    eval(k,l,r);
    if (r <= a || b <= l) {
        return;
    }
    else if (a <= l && r <= b) {
        lazy[k] = c;
        eval(k,l,r);
    }
    else {
        int m = (l+r)/2;
        update(a,b,c,2*k+1,l,m);
        update(a,b,c,2*k+2,m,r);
        data[k] = data[2*k+1] + data[2*k+2];
    }
}

int main() {
    scanf("%d %d",&n_,&q);
    init();
    for (int i = 0; i < n_; i++) {
        long long a;
        scanf("%lld",&a);
        update(i,i+1,a,0,0,n);
    }
    for (int i = 0; i < q; i++) {
        char c;
        scanf(" %c",&c);
        if (c == 'Q') {
            int a,b;
            scanf("%d %d",&a,&b); a--;
            long long ans = query(a,b,0,0,n);
            printf("%lld\n",ans);
        }
        else if (c == 'C') {
            int a,b,c;
            scanf("%d %d %d",&a,&b,&c); a--;
            update(a,b,c,0,0,n);
        }
        else {
            throw;
        }
    }
    return 0;
}
