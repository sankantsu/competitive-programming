// chain disappearance puzzle

#include <iostream>
#include <vector>
#include <algorithm>
#include <deque>

constexpr int col = 5;
constexpr int chain = 3;

struct puzzle {
    puzzle(int h_) : h(h_), w(col), score(0) {
        board.resize(h);
        for (int i = 0; i < h; i++) {
            board[i].resize(w);
        }
    }
    int get_score() {
        /* std::cout << "initial:" << std::endl; */
        /* print(); */
        while(update());
        return score;
    }
    void print() {
        for (int i = 0; i < h; i++) {
            for (int j = 0; j < w; j++) {
                std::cout << board[i][j] << " ";
            }
            std::cout << std::endl;
        }
    }
    friend std::istream& operator>>(std::istream& stream, puzzle& pz);
    private:
    bool update() {
        int s = erase();
        if (s == 0) {
            return false;
        }
        else {
            /* std::cout << "after erase:" << std::endl; */
            /* print(); */
            score += s;
            drop();
            /* std::cout << "after drop:" << std::endl; */
            /* print(); */
            return true;
        }
    }
    int erase() {
        int s = 0;
        for (int i = 0; i < h; i++) {
            std::vector<bool> erase(w,false);
            int p = 0;
            int last = 1;
            for (int j = 1; j < w; j++) {
                if (board[i][j] == board[i][p]) {
                    last++;
                    if (last - p == chain) {
                        for (int k = p; k < last; k++) {
                            erase[k] = true;
                        }
                    }
                    else if (last - p > chain) {
                        erase[j] = true;
                    }
                }
                else {
                    p = j;
                    last = j+1;
                }
            }
            for (int j = 0; j < w; j++) {
                if (erase[j]) {
                    s += board[i][j];
                    board[i][j] = 0;
                }
            }
        }
        return s;
    }
    void drop() {
        for (int j = 0; j < w; j++) {
            std::deque<int> new_col;
            for (int i = 0; i < h; i++) {
                if (board[i][j] != 0) {
                    new_col.push_back(board[i][j]);
                }
            }
            int pad = h - new_col.size();
            for (int i = 0; i < pad; i++) {
                new_col.push_front(0);
            }
            for (int i = 0; i < h; i++) {
                board[i][j] = new_col[i];
            }
        }
    }
    int h,w;
    std::vector<std::vector<int>> board;
    int score;
};

std::istream& operator>>(std::istream& stream, puzzle& pz) {
    for (int i = 0; i < pz.h; i++) {
        for (int j = 0; j < pz.w; j++) {
            stream >> pz.board[i][j];
        }
    }
    return stream;
}

bool solve() {
    int h;
    std::cin >> h;
    if (h == 0) {
        return false;
    }

    puzzle pz(h);
    std::cin >> pz;

    int score = pz.get_score();
    std::cout << score << std::endl;
    return true;
}

int main() {
    while (solve());
    return 0;
}
