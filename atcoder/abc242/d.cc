#include <iostream>
#include <atcoder/all>
#include <string>
#include <vector>
#include <deque>

using namespace std;
using namespace atcoder;

struct query {
    long t;
    long k;
};

char transform(char c, int d) {
    // cout << "debug " << c << " " << d << endl;
    switch(c) {
        case 'A':
            return !d ? 'B' : 'C';
        case 'B':
            return !d ? 'C' : 'A';
        case 'C':
            return !d ? 'A' : 'B';
        default:
            throw;
    }
}

char answer_query(const string &s, query q) {
    long t = q.t;
    long k = q.k;

    deque<int> deq;
    while (k > 1 && t > 0) {
        deq.push_front(!(k&0x1));
        k = (k+1)>>1;
        t--;
    }

    char c = s[k-1];
    t = t%3;
    while (t > 0) {
        c = transform(c,0);
        t--;
    }
    for (int d : deq) {
        c = transform(c,d);
    }

    return c;
}

int main() {
    string s;
    int q;
    cin >> s;
    cin >> q;

    vector<query> v(q);
    for (int i = 0; i < q; i++) {
        long t,k;
        cin >> t >> k;
        v[i] = query{t,k};
    }

    for (auto qu : v) {
        char c = answer_query(s,qu);
        cout << c << endl;
    }
}
