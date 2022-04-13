#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

constexpr int max_n = 1000;
constexpr int max_k = 100000;

int m,n,k;
char field[max_n+1][max_n+1];
int a[max_k];
int b[max_k];
int c[max_k];
int d[max_k];

/* int sum_jungle[max_n+1][max_n+1]; */
/* int sum_ocean[max_n+1][max_n+1]; */
/* int sum_ice[max_n+1][max_n+1]; */
vector<vector<int>> sum_jungle(max_n+1,vector<int>(max_n+1));
vector<vector<int>> sum_ocean(max_n+1,vector<int>(max_n+1));
vector<vector<int>> sum_ice(max_n+1,vector<int>(max_n+1));

/* void calc_each_sum(int *sum[max_n+1], char c) { */
template <typename Vec>
void calc_each_sum(Vec &sum, char c) {
    rep(i,m) {
        rep(j,n) {
            sum[i+1][j+1] = sum[i+1][j] + sum[i][j+1] - sum[i][j];
            if (field[i][j] == c) {
                sum[i+1][j+1]++;
            }
        }
    }
    /* rep(i,m) { */
    /*     rep(j,n) { */
    /*         cout << sum[i+1][j+1] << " "; */
    /*     } */
    /*     cout << endl; */
    /* } */
}

void calc_all_sum() {
    calc_each_sum(sum_jungle,'J');
    calc_each_sum(sum_ocean,'O');
    calc_each_sum(sum_ice,'I');
}

/* int area_sum(int *sum[max_n+1], int a, int b, int c, int d) { */
template <typename Vec>
int area_sum(const Vec& sum, int a, int b, int c, int d) {
    int s1 = sum[c][d];
    int s2 = sum[a][d];
    int s3 = sum[c][b];
    int s4 = sum[a][b];
    return s1-s2-s3+s4;
}

auto query(int a, int b, int c, int d) {
    int j = area_sum(sum_jungle,a,b,c,d);
    int o = area_sum(sum_ocean,a,b,c,d);
    int i = area_sum(sum_ice,a,b,c,d);
    return make_tuple(j,o,i);
}

int main() {
    cin >> m >> n;
    cin >> k;
    rep(i,m) rep(j,n) {
        cin >> field[i][j];
    }
    /* cout << endl; */
    /* rep(i,m) { */
    /*     rep(j,n) { */
    /*         cout << field[i][j]; */
    /*     } */
    /*     cout << endl; */
    /* } */
    rep(i,k) {
        cin >> a[i] >> b[i] >> c[i] >> d[i];
        a[i]--; b[i]--;
    }
    calc_all_sum();
    rep(i,k) {
        auto [jun,oce,ice] = query(a[i],b[i],c[i],d[i]);
        cout << jun << " " << oce << " " << ice << endl;
    }
}
