#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <random>
#include <fstream>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long solve(long n, const vector<long>& a) {
    long ans = 0;
    for (long i = 0; i < n; i++) for (long j = i + 2; j < n; j++) {
        for (long k = i; k <= j; k++) for (long l = k + 1; l <= j; l++) for (long m = l + 1; m <= j; m++) {
            if (a[l] - a[k] == a[m] - a[l]) {
                ans++;
                goto next;
            }
        }
next:
        ;
    }
    return ans;
}

void gen_test(long n, long id) {
    static mt19937 mt;
    vector<long> a;
    rep(i,n) {
        long r = (mt() % 10) + 1;
        a.push_back(r);
    }
    long ans = solve(n, a);

    string infile = string("in/in-") + to_string(id) + string(".txt");
    ofstream in(infile);
    in << n << endl;
    rep(i,n) {
        in << a[i] << " ";
    }
    in << endl;

    string outfile = string("out/out-") + to_string(id) + string(".txt");
    ofstream out(outfile);
    out << ans << endl;
}

int main(int argc, char **argv) {
    long n = stoi(string(argv[1]));
    rep(id,100) {
        gen_test(n, id);
    }
}
