#include <iostream>
#include <vector>

using namespace std;

constexpr int max_h = 2000;
constexpr int max_w = 2000;

int h,w;
int q;

class uftree {
    public:
        uftree(int n) : par(n) {
            for (int i = 0; i < n; i++) {
                par[i] = i;
            }
        }
        int find(int x) {
            if (par[x] == x) {
                return x;
            }
            else {
                int root = find(par[x]);
                par[x] = root;
                return root;
            }
        }
        void unite(int x, int y) {
            int p1,p2;
            p1 = find(x);
            p2 = find(y);
            if (p1 == p2) {
                return;
            }
            par[p1] = p2;
        }
        bool is_same(int x, int y) {
            return find(x) == find(y);
        }
        void print_par() {
            cout << "print_par: ";
            for (size_t i = 0; i < h*w; i++) {
                cout << find(i) << " ";
            }
            cout << endl;
        }
    private:
        vector<int> par;
};

int board[max_h][max_w];
uftree uf(max_h*max_w);

int pos(int r, int c) {
    return w*r+c;
}

void unite_path(int r, int c) {
    int k = pos(r,c);
    if (r-1 >= 0 && board[r-1][c] == 1) uf.unite(k,pos(r-1,c));
    if (c-1 >= 0 && board[r][c-1] == 1) uf.unite(k,pos(r,c-1));
    if (c+1 <  w && board[r][c+1] == 1) uf.unite(k,pos(r,c+1));
    if (r+1 <  h && board[r+1][c] == 1) uf.unite(k,pos(r+1,c));
}

bool reachable(int r1, int c1, int r2, int c2) {
    if (board[r1][c1] == 0 || board[r2][c2] == 0) {
        return false;
    }
    if (uf.is_same(pos(r1,c1),pos(r2,c2))) {
        return true;
    }
    return false;
}

void print_board() {
    cout << "print_board:" << endl;
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            cout << board[i][j] << " ";
        }
        cout << endl;
    }
}

int main() {
    cin >> h >> w;
    cin >> q;
    for (int i = 0; i < q; i++) {
        /* cout << "query " << i << endl; */
        int t;
        cin >> t;
        if (t == 1) {
            int r,c;
            cin >> r >> c;
            r--; c--;
            board[r][c] = 1;
            unite_path(r,c);
            /* print_board(); */
            /* uf.print_par(); */
        }
        else {
            /* print_board(); */
            /* uf.print_par(); */
            int r1,c1,r2,c2;
            cin >> r1 >> c1 >> r2 >> c2;
            r1--; c1--; r2--; c2--;
            bool ans = reachable(r1,c1,r2,c2);
            if (ans) {
                cout << "Yes" << endl;
            }
            else {
                cout << "No" << endl;
            }
        }
    }
}
