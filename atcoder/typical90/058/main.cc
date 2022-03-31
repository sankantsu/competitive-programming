#include <iostream>
#include <vector>

using namespace std;

constexpr int m = 100000;

long long n,k;
int first[m];

constexpr int pow(int a, int b) {
    return b > 0 ? a*pow(a,b-1) : 1;
}

constexpr int digit_sum(int x) {
    return (x > 0) ? (x%10) + digit_sum(x/10) : 0;
}

int button(int x) {
    int y = digit_sum(x);
    x = (x + y)%pow(10,5);
    return x;
}

int main() {
    cin >> n >> k;
    for (int i = 0; i < m; i++) {
        first[i] = -1;
    }

    long cnt = 0;
    long cycle = -1;
    vector<int> v;

    v.push_back(n);
    while (cnt < k) {
        first[n] = cnt;

        n = button(n);
        v.push_back(n);
        cnt++;

        if (first[n] >= 0) {
            cycle = cnt - first[n];
            break;
        }
    }

    long i;
    if (cycle == -1) {
        i = cnt;
    }
    else {
        i = first[n] + (k-first[n])%cycle;
    }
    cout << v[i] << endl;
}
