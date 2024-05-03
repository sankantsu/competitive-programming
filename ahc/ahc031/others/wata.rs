#![allow(non_snake_case)]

use itertools::Itertools;
use proconio::input;
use rand::prelude::*;
use std::{collections::BinaryHeap, vec};

fn main() {
    get_time();
    let input = read_input();
    let mut rng = rand_pcg::Pcg64Mcg::seed_from_u64(5437987);
    if input.max_a.iter().sum::<i32>() <= input.W * input.W {
        if let Some(out) = solve_optimal(&input) {
            eprintln!("!log easy 1");
            let out = vec![out];
            for _ in 0..input.D {
                write_output(&out);
            }
            eprintln!("Time = {:.3}", get_time());
            return;
        }
        eprintln!("!log easy 1000");
    }
    let state = solve(&input, 1.5, &mut rng);
    let state = solve2(&input, &state, &mut rng);
    let out = state.get_out(&input);
    write_output(&out);
    eprintln!("Time = {:.3}", get_time());
}

/// スコア=1に挑戦する
/// 最大3つまで選んで余り領域が一番小さくなる組を使う貪欲法
fn solve_optimal(input: &Input) -> Option<Vec<(i32, i32, i32, i32)>> {
    let mut used = vec![false; input.N];
    let mut out = vec![(0, 0, 0, 0); input.N];
    let mut w = 0;
    for i in (0..input.N).rev() {
        if used[i] {
            continue;
        }
        let mut best = vec![i];
        let mut best_w = (input.max_a[i] + input.W - 1) / input.W;
        let mut min_room = input.W * best_w - input.max_a[i];
        for j in 0..i {
            if used[j] {
                continue;
            }
            for k in 0..j {
                if used[k] {
                    continue;
                }
                let w = (input.max_a[i] + input.max_a[j] + input.max_a[k] + input.W - 1) / input.W;
                if (input.max_a[i] + w - 1) / w + (input.max_a[j] + w - 1) / w + (input.max_a[k] + w - 1) / w <= input.W {
                    let room = input.W * w - input.max_a[i] - input.max_a[j] - input.max_a[k];
                    if min_room.setmin(room) {
                        best = vec![i, j, k];
                        best_w = w;
                    }
                }
            }
            let w = (input.max_a[i] + input.max_a[j] + input.W - 1) / input.W;
            if (input.max_a[i] + w - 1) / w + (input.max_a[j] + w - 1) / w <= input.W {
                let room = input.W * w - input.max_a[i] - input.max_a[j];
                if min_room.setmin(room) {
                    best = vec![i, j];
                    best_w = w;
                }
            }
        }
        let mut y0 = 0;
        for &p in &best {
            used[p] = true;
            let y1 = y0 + (input.max_a[p] + best_w - 1) / best_w;
            out[p] = (y0, w, y1, w + best_w);
            y0 = y1;
        }
        w += best_w;
    }
    if w <= input.W {
        return Some(out);
    } else {
        None
    }
}

const MIN_W: i32 = 1;

/// まず初めに最初から最後まで固定する縦線を山登り法で構築する
fn solve(input: &Input, tl: f64, rng: &mut rand_pcg::Pcg64Mcg) -> State {
    let mut state = State::new(&input);
    let stime = get_time();
    let mut score = state.eval(&input);
    let mut last_t = -100.0;
    for iter in 0.. {
        let t = (get_time() - stime) / (tl - stime);
        if t >= 1.0 {
            eprintln!("{:.3}: {} ({})", get_time(), score, state.ws.len());
            eprintln!("!log iter1 {}", iter);
            break;
        }
        if last_t + 0.05 <= t {
            last_t = t;
            eprintln!("{:.3}: {} ({})", get_time(), score, state.ws.len());
        }
        let lot = rng.gen_range(0..2);
        if lot == 0 {
            if state.ws.len() == input.N {
                continue;
            }
            // iを分割
            let i = rng.gen_range(0..state.ws.len());
            if state.ws[i] < MIN_W * 2 {
                continue;
            }
            let w = rng.gen_range(MIN_W..=state.ws[i] - MIN_W);
            let mut next = state.clone();
            next.split(&input, i, w, rng);
            let score2 = next.eval(&input);
            if score >= score2 {
                score = score2;
                state = next;
            }
        } else {
            if state.ws.len() == 1 {
                continue;
            }
            // iからjへ幅を移す
            let i = rng.gen_range(0..state.ws.len());
            let j = rng.gen_range(0..state.ws.len());
            if i == j || state.ws[i] <= MIN_W {
                continue;
            }
            let w = rng.gen_range(1..=state.ws[i] - MIN_W);
            let mut next = state.clone();
            next.change_x(&input, i, j, w, rng);
            let score2 = next.eval(&input);
            if score >= score2 {
                score = score2;
                state = next;
            }
        }
    }
    for d in 0..input.D {
        state.optimize(&input, d, 1000000 / input.D as i32, rng);
    }
    state
}

/// 次に、縦線は固定した状態で横線を焼き鈍しで構築する
/// どの区画がどの予約に対応するかは保持せず、スコア計算時にソートして対応を決める
fn solve2(input: &Input, state: &State, rng: &mut rand_pcg::Pcg64Mcg) -> State2 {
    let mut state = State2::new(&input, &state);
    let mut score = state.eval(input);
    let stime = get_time();
    let tl = 2.9;
    let temp0 = 1e2;
    let temp1 = 1e0;
    let mut temp = temp0;
    let mut last_t = -100.0;
    for iter in 0.. {
        if iter & 0xff == 0 {
            let t = (get_time() - stime) / (tl - stime);
            if t >= 1.0 || last_t + 0.05 < t {
                last_t = t;
                eprintln!("{:.3}: {}", get_time(), score);
                if t >= 1.0 {
                    eprintln!("!log iter2 {}", iter);
                    break;
                }
                temp = f64::powf(temp0, 1.0 - t) * f64::powf(temp1, t);
            }
        }
        let tol = (-rng.gen::<f64>().ln() * temp).floor() as i64;
        let mut lot = rng.gen::<u32>();
        let d = rng.gen_range(0..input.D);
        let i = rng.gen_range(0..state.ws.len());
        if state.ys[d][i].len() == 0 {
            continue;
        }
        let p = rng.gen_range(0..state.ys[d][i].len());
        let y0 = state.ys[d][i][p];
        let j = if lot & 255 < 128 {
            i
        } else {
            rng.gen_range(0..state.ws.len())
        };
        lot >>= 8;
        let (d0, d1) = if lot & 255 < 224 {
            (d, d)
        } else {
            let mut d0 = d;
            let mut d1 = d;
            while d0 > 0 && state.ys[d0 - 1][i].contains(&y0) {
                d0 -= 1;
            }
            while d1 + 1 < input.D && state.ys[d1 + 1][i].contains(&y0) {
                d1 += 1;
            }
            (d0, d1)
        };
        lot >>= 8;
        let y = if lot & 3 == 0 && d0 > 0 && state.ys[d0 - 1][j].len() > 0 {
            *state.ys[d0 - 1][j].choose(rng).unwrap()
        } else if lot & 3 == 1 && d1 + 1 < input.D && state.ys[d1 + 1][j].len() > 0 {
            *state.ys[d1 + 1][j].choose(rng).unwrap()
        } else if lot & 3 == 2 && i == j {
            rng.gen_range((y0 - 20).max(1)..=(y0 + 20).min(input.W - 1))
        } else {
            rng.gen_range(1..input.W)
        };
        if (d0..=d1).any(|d| state.ys[d][j].contains(&y)) {
            continue;
        }
        let is = if i == j { vec![i] } else { vec![i, j] };
        let mut diff = -state.eval_range(input, d0, d1 + 1, &is);
        for d in d0..=d1 {
            state.remove(input, d, i, y0);
            state.add(input, d, j, y);
        }
        diff += state.eval_range(input, d0, d1 + 1, &is);
        if diff <= tol {
            score += diff;
        } else {
            for d in d0..=d1 {
                state.remove(input, d, j, y);
                state.add(input, d, i, y0);
            }
        }
        // assert_eq!(score, state.eval(input));
    }
    state
}

const INF: i64 = 1 << 40;

/// 横線構築用の状態
#[derive(Clone, Debug)]
struct State2 {
    /// ws[i] := i番目の区画の幅 (和は常にW)
    ws: Vec<i32>,
    /// ys[d][i] := d日目のi番の区画の横区切りの位置
    ys: Vec<Vec<Vec<i32>>>,
    /// b[d] := d日目の区画の面積を昇順ソートしたもの
    b: Vec<Vec<i32>>,
}

impl State2 {
    fn new(input: &Input, state: &State) -> Self {
        let ws = state.ws.clone();
        let mut ys = mat![vec![]; input.D; ws.len()];
        let mut b = vec![vec![]; input.D];
        for d in 0..input.D {
            for i in 0..ws.len() {
                let mut ks = state.rs[d][i].ks.clone();
                ks.sort();
                let mut y0 = 0;
                for j in 0..ks.len() {
                    if y0 > 0 {
                        ys[d][i].push(y0);
                    }
                    y0 += state.rs[d][i].get_h(input, ks[j]);
                    y0.setmin(input.W - ks.len() as i32 + j as i32 + 1);
                }
            }
            for i in 0..ws.len() {
                let mut y0 = 0;
                for &y in ys[d][i].iter().chain(&[input.W]) {
                    b[d].push((y - y0) * ws[i]);
                    y0 = y;
                }
            }
            b[d].sort();
        }
        Self { ws, ys, b }
    }
    /// d日目のi番目のx区画に区切り線yを追加し、bを更新する
    fn add(&mut self, input: &Input, d: usize, i: usize, y: i32) {
        let p = self.ys[d][i].iter().position(|&y2| y2 >= y).unwrap_or(self.ys[d][i].len());
        let y0 = if p == 0 { 0 } else { self.ys[d][i][p - 1] };
        let y1 = if p == self.ys[d][i].len() { input.W } else { self.ys[d][i][p] };
        self.ys[d][i].insert(p, y);
        let mut b0 = (y - y0) * self.ws[i];
        let mut b1 = (y1 - y) * self.ws[i];
        if b0 > b1 {
            std::mem::swap(&mut b0, &mut b1);
        }
        let b2 = b0 + b1;
        let mut p = input.N - 1;
        while self.b[d][p - 1] > b2 {
            self.b[d][p] = self.b[d][p - 1];
            p -= 1;
        }
        while p >= 2 && self.b[d][p - 2] > b1 {
            self.b[d][p] = self.b[d][p - 2];
            p -= 1;
        }
        self.b[d][p] = b1;
        p -= 1;
        while p >= 1 && self.b[d][p - 1] > b0 {
            self.b[d][p] = self.b[d][p - 1];
            p -= 1;
        }
        self.b[d][p] = b0;
    }
    /// d日目のi番目のx区画から区切り線yを削除し、bを更新する
    fn remove(&mut self, input: &Input, d: usize, i: usize, y: i32) {
        let p = self.ys[d][i].iter().position(|&y2| y2 == y).unwrap();
        let y0 = if p == 0 { 0 } else { self.ys[d][i][p - 1] };
        let y1 = if p + 1 == self.ys[d][i].len() {
            input.W
        } else {
            self.ys[d][i][p + 1]
        };
        self.ys[d][i].remove(p);
        let mut b0 = (y - y0) * self.ws[i];
        let mut b1 = (y1 - y) * self.ws[i];
        if b0 > b1 {
            std::mem::swap(&mut b0, &mut b1);
        }
        let b2 = b0 + b1;
        let mut p = 0;
        while self.b[d][p] < b0 {
            p += 1;
        }
        while self.b[d][p + 1] < b1 {
            self.b[d][p] = self.b[d][p + 1];
            p += 1;
        }
        while p + 2 < input.N && self.b[d][p + 2] < b2 {
            self.b[d][p] = self.b[d][p + 2];
            p += 1;
        }
        self.b[d][p] = b2;
        p += 1;
        while p + 1 < input.N {
            self.b[d][p] = self.b[d][p + 1];
            p += 1;
        }
        self.b[d][p] = i32::MAX;
    }
    fn eval(&self, input: &Input) -> i64 {
        self.eval_range(input, 0, input.D, &(0..self.ws.len()).collect_vec())
    }
    fn eval_range(&self, input: &Input, d0: usize, d1: usize, is: &[usize]) -> i64 {
        let mut score = 1;
        for d in d0..=d1 {
            if d < d1 {
                for i in 0..input.N {
                    if self.b[d][i] < input.a[d][i] {
                        score += 100 * (input.a[d][i] - self.b[d][i]) as i64;
                    }
                }
            }
            if 0 < d && d < input.D {
                for &i in is {
                    let mut p1 = 0;
                    let mut p2 = 0;
                    let ys1 = &self.ys[d - 1][i];
                    let ys2 = &self.ys[d][i];
                    while p1 < ys1.len() && p2 < ys2.len() {
                        if ys1[p1] == ys2[p2] {
                            p1 += 1;
                            p2 += 1;
                        } else if ys1[p1] < ys2[p2] {
                            score += self.ws[i] as i64;
                            p1 += 1;
                        } else {
                            score += self.ws[i] as i64;
                            p2 += 1;
                        }
                    }
                    score += (ys1.len() - p1 + ys2.len() - p2) as i64 * self.ws[i] as i64;
                }
            }
        }
        score
    }
    fn get_out(&self, input: &Input) -> Vec<Vec<(i32, i32, i32, i32)>> {
        let mut out = vec![vec![]; input.D];
        for d in 0..input.D {
            let mut x0 = 0;
            let mut list = vec![];
            for i in 0..self.ws.len() {
                let mut y0 = 0;
                for &y in self.ys[d][i].iter().chain(&[input.W]) {
                    list.push((y0, x0, y, x0 + self.ws[i], i));
                    y0 = y;
                }
                x0 += self.ws[i];
            }
            list.sort_by_key(|r| (r.2 - r.0) * (r.3 - r.1));
            // 細長い区画が2つ並んでいると90度回転することでパーティションを少し短く出来る

            // ks[i] := i番目の区画に含まれる予約の(y座標,id)のリスト
            let mut ks = vec![vec![]; self.ws.len()];
            for k in 0..input.N {
                ks[list[k].4].push((list[k].0, k));
            }
            x0 = 0;
            for i in 0..self.ws.len() {
                ks[i].sort();
                let mut s = 0;
                let mut y0 = 0;
                while s < ks[i].len() {
                    let mut t = s + 1;
                    while t < ks[i].len()
                        && (d == 0 || !self.ys[d - 1][i].contains(&list[ks[i][t].1].0))
                        && (d + 1 == input.D || !self.ys[d + 1][i].contains(&list[ks[i][t].1].0))
                    {
                        t += 1;
                    }
                    // [s,t) は並び替えてもスコアが変化しない
                    let mut tmp = ks[i][s..t].iter().map(|r| r.1).collect_vec();
                    tmp.sort();
                    let mut hs = tmp
                        .iter()
                        .map(|&k| (input.a[d][k] + self.ws[i] - 1) / self.ws[i])
                        .collect_vec();
                    let mut rem_h = tmp.iter().map(|&k| list[k].2 - list[k].0).sum::<i32>() - hs.iter().sum::<i32>();
                    if rem_h < 0 {
                        let mut rem = (0..hs.len()).map(|k| (input.a[d][tmp[k]] % self.ws[i], k)).collect_vec();
                        rem.sort();
                        for (_, k) in rem {
                            if hs[k] > 1 {
                                hs[k] -= 1;
                                rem_h += 1;
                            }
                            if rem_h == 0 {
                                break;
                            }
                        }
                        hs.sort();
                        for h in &mut hs {
                            if rem_h < 0 {
                                let v = (-rem_h).min(*h - 1);
                                rem_h += v;
                                *h -= v;
                            }
                        }
                    } else {
                        hs.sort();
                        *hs.last_mut().unwrap() += rem_h;
                    }
                    let ys = y0;
                    for k in 0..hs.len() {
                        list[tmp[k]].0 = y0;
                        list[tmp[k]].2 = y0 + hs[k];
                        y0 += hs[k];
                    }
                    if hs.len() >= 2 {
                        let (h1, k1) = (hs[0], tmp[0]);
                        let (h2, k2) = (hs[1], tmp[1]);
                        let h = h1 + h2;
                        let mut w1 = (input.a[d][k1] + h - 1) / h;
                        let mut w2 = (input.a[d][k2] + h - 1) / h;
                        if w1 + w2 > self.ws[i] {
                            if w1 > 1 && (w2 == 1 || input.a[d][k1] % h < input.a[d][k2] % h) {
                                w1 -= 1;
                                if w2 > 1 && w1 + w2 > self.ws[i] {
                                    w2 -= 1;
                                }
                            } else {
                                w2 -= 1;
                                if w1 > 1 && w1 + w2 > self.ws[i] {
                                    w1 -= 1;
                                }
                            }
                            while w1 + w2 > self.ws[i] {
                                if w1 > 1 {
                                    w1 -= 1;
                                } else {
                                    w2 -= 1;
                                }
                            }
                        } else {
                            w2 = self.ws[i] - w1;
                        }
                        let mut diff = self.ws[i] * input.weight_d(d) as i32
                            + 100 * (input.a[d][k1] - self.ws[i] * h1).max(0)
                            + 100 * (input.a[d][k2] - self.ws[i] * h2).max(0);
                        diff -= h * input.weight_d(d) as i32
                            + 100 * (input.a[d][k1] - h * w1).max(0)
                            + 100 * (input.a[d][k2] - h * w2).max(0);
                        if diff > 0 {
                            list[k1] = (ys, x0, ys + h, x0 + w1, k1);
                            list[k2] = (ys, x0 + w1, ys + h, x0 + self.ws[i], k2);
                        }
                    }
                    s = t;
                }
                x0 += self.ws[i];
            }
            out[d] = list.into_iter().map(|r| (r.0, r.1, r.2, r.3)).collect();
        }
        out
    }
}

#[derive(Clone, Debug)]
struct Section {
    d: usize,
    w: i32,
    h: i32,
    /// 属する予約番号
    ks: Vec<usize>,
    /// 残っている高さ
    rem_h: i32,
}

impl Section {
    fn empty(d: usize, w: i32, h: i32) -> Self {
        Self {
            d,
            w,
            h,
            ks: vec![],
            rem_h: h,
        }
    }
    fn new(input: &Input, d: usize) -> Self {
        let mut s = Self::empty(d, input.W, input.W);
        for k in 0..input.N {
            s.add(input, k);
        }
        s
    }
    fn get_h(&self, input: &Input, k: usize) -> i32 {
        (input.a[self.d][k] + self.w - 1) / self.w
    }
    fn clear(&mut self) -> Vec<usize> {
        let mut ks = vec![];
        std::mem::swap(&mut self.ks, &mut ks);
        self.rem_h = self.h;
        ks
    }
    fn add(&mut self, input: &Input, k: usize) {
        self.ks.push(k);
        self.rem_h -= self.get_h(input, k);
    }
    fn remove(&mut self, input: &Input, k: usize) {
        let i = self.ks.iter().position(|&x| x == k).unwrap();
        self.ks.swap_remove(i);
        self.rem_h += self.get_h(input, k)
    }
    fn eval(&self, input: &Input) -> i64 {
        if self.ks.len() == 0 {
            return INF;
        }
        self.w as i64 * (self.ks.len() - 1) as i64 * input.weight_d(self.d) + 100 * (self.w * (-self.rem_h).max(0)) as i64
    }
}

/// 縦線構築用の状態
#[derive(Clone, Debug)]
struct State {
    /// ws[i] := i番目の区画の幅 (和は常にW)
    ws: Vec<i32>,
    /// rs[d][i] := d日目のi番の区画
    rs: Vec<Vec<Section>>,
}

impl State {
    fn new(input: &Input) -> Self {
        Self {
            ws: vec![input.W],
            rs: (0..input.D).map(|d| vec![Section::new(input, d)]).collect(),
        }
    }
    fn eval(&self, input: &Input) -> i64 {
        let mut score = 1;
        for d in 0..input.D {
            for r in &self.rs[d] {
                score += r.eval(input);
            }
        }
        score
    }
    /// i番目の区画を幅wと残りで分割する
    fn split(&mut self, input: &Input, i: usize, w: i32, rng: &mut rand_pcg::Pcg64Mcg) {
        let w2 = self.ws[i] - w;
        self.ws[i] = w;
        self.ws.push(w2);
        let j = self.ws.len() - 1;
        for d in 0..input.D {
            self.rs[d][i].w = w;
            let ks = self.rs[d][i].clear();
            self.rs[d].push(Section::empty(d, w2, input.W));
            self.greedy(input, d, &[i, j], ks);
            self.optimize(input, d, 100, rng);
        }
    }
    /// i番目のx区画からj番目のx区画へ幅wを移す
    fn change_x(&mut self, input: &Input, i: usize, j: usize, w: i32, rng: &mut rand_pcg::Pcg64Mcg) {
        self.ws[i] -= w;
        self.ws[j] += w;
        for d in 0..input.D {
            let mut ks = self.rs[d][i].clear();
            ks.extend(self.rs[d][j].clear());
            self.rs[d][i].w = self.ws[i];
            self.rs[d][j].w = self.ws[j];
            self.greedy(input, d, &[i, j], ks);
            self.optimize(input, d, 100, rng);
        }
    }
    /// 指定された区画へksを割り振る
    fn greedy(&mut self, input: &Input, d: usize, ss: &[usize], mut ks: Vec<usize>) {
        ks.sort();
        for k in ks.into_iter().rev() {
            let mut best = (i32::MIN, 0);
            for i in 0..ss.len() {
                let s = &self.rs[d][ss[i]];
                let rem_h = s.rem_h - s.get_h(input, k);
                best.setmax((rem_h, i));
            }
            let i = best.1;
            self.rs[d][ss[i]].add(input, k);
        }
        // 空の区画が出来てしまった場合は他から移動する
        for &i in ss {
            if self.rs[d][i].ks.is_empty() {
                for j in 0..self.rs[d].len() {
                    if self.rs[d][j].ks.len() >= 2 {
                        let k = self.rs[d][j].ks[self.rs[d][j].ks.len() - 1];
                        self.rs[d][j].remove(input, k);
                        self.rs[d][i].add(input, k);
                        break;
                    }
                }
            }
        }
    }
    fn optimize(&mut self, input: &Input, d: usize, iter: i32, rng: &mut rand_pcg::Pcg64Mcg) {
        if self.rs[d].len() == 1 {
            return;
        }

        // 各x区画から一番大きいものを取り除いて大きい順に一番空きが多いところに入れて次に大きいやつを取り除く、を繰り返す
        let mut que = BinaryHeap::new();
        let mut ks = self.rs[d].iter().map(|r| r.ks.clone()).collect_vec();
        let mut ps = vec![0; ks.len()];
        for i in 0..self.rs[d].len() {
            ks[i].sort();
            ks[i].reverse();
            que.push((
                (self.rs[d][i].rem_h + self.rs[d][i].get_h(input, ks[i][0])) * self.rs[d][i].w,
                i,
            ));
        }
        let mut t = input.N;
        while let Some((_, i)) = que.pop() {
            t -= 1;
            self.rs[d][i].rem_h += self.rs[d][i].get_h(input, ks[i][ps[i]]);
            ks[i][ps[i]] = t;
            ps[i] += 1;
            self.rs[d][i].rem_h -= self.rs[d][i].get_h(input, t);
            if ps[i] < ks[i].len() {
                que.push((
                    (self.rs[d][i].rem_h + self.rs[d][i].get_h(input, ks[i][ps[i]])) * self.rs[d][i].w,
                    i,
                ));
            }
        }
        for i in 0..self.rs[d].len() {
            std::mem::swap(&mut self.rs[d][i].ks, &mut ks[i]);
        }

        let weight_d = input.weight_d(d) as i32;
        let mut order = vec![0; input.N];
        for _ in 0..iter {
            let i = rng.gen_range(0..self.rs[d].len());
            let mut j = rng.gen_range(0..self.rs[d].len() - 1);
            if j >= i {
                j += 1;
            }
            // 1つ動かし、はみ出した分が解消されるようにランダムに戻す
            let k1 = *self.rs[d][i].ks.choose(rng).unwrap();
            let hi1 = self.rs[d][i].get_h(input, k1);
            let hj1 = self.rs[d][j].get_h(input, k1);
            if hj1 > self.rs[d][i].h {
                continue;
            }
            let m = self.rs[d][j].ks.len();
            for k in 0..m {
                order[k] = k;
            }
            let mut rem_h = self.rs[d][j].rem_h - hj1;
            if rem_h >= 0 && self.rs[d][i].ks.len() == 1 {
                continue;
            }
            let mut ks2 = vec![];
            for p in 0..m {
                if rem_h >= 0 {
                    break;
                }
                order.swap(p, rng.gen_range(p..m));
                let k2 = self.rs[d][j].ks[order[p]];
                rem_h += self.rs[d][j].get_h(input, k2);
                ks2.push(k2);
            }
            let mut diff = self.rs[d][i].w * (self.rs[d][i].ks.len() - 1) as i32 * weight_d
                + 100 * self.rs[d][i].w * (-self.rs[d][i].rem_h).max(0)
                + self.rs[d][j].w * (self.rs[d][j].ks.len() - 1) as i32 * weight_d
                + 100 * self.rs[d][j].w * (-self.rs[d][j].rem_h).max(0);
            let hi2 = ks2.iter().map(|&k2| self.rs[d][i].get_h(input, k2)).sum::<i32>();
            let hj2 = ks2.iter().map(|&k2| self.rs[d][j].get_h(input, k2)).sum::<i32>();
            diff -= self.rs[d][i].w * (self.rs[d][i].ks.len() - 1 - 1 + ks2.len()) as i32 * weight_d
                + 100 * self.rs[d][i].w * (-self.rs[d][i].rem_h - hi1 + hi2).max(0)
                + self.rs[d][j].w * (self.rs[d][j].ks.len() - 1 - ks2.len() + 1) as i32 * weight_d
                + 100 * self.rs[d][j].w * (-self.rs[d][j].rem_h - hj2 + hj1).max(0);
            if diff >= 0 {
                self.rs[d][i].remove(input, k1);
                for &k2 in &ks2 {
                    self.rs[d][j].remove(input, k2);
                    self.rs[d][i].add(input, k2);
                }
                self.rs[d][j].add(input, k1);
            }
        }
    }
}

// 入出力と得点計算
struct Input {
    W: i32,
    D: usize,
    N: usize,
    a: Vec<Vec<i32>>,
    max_a: Vec<i32>,
}

impl Input {
    fn weight_d(&self, d: usize) -> i64 {
        (if d == 0 { 0 } else { 1 }) + (if d == self.D - 1 { 0 } else { 1 })
    }
}

fn read_input() -> Input {
    input! {
        W: i32,
        D: usize,
        N: usize,
        a: [[i32; N]; D],
    }
    let max_a = (0..N).map(|i| (0..D).map(|d| a[d][i]).max().unwrap()).collect();
    Input { W, D, N, a, max_a }
}

fn write_output(out: &Vec<Vec<(i32, i32, i32, i32)>>) {
    for d in 0..out.len() {
        for &(a, b, c, d) in &out[d] {
            println!("{} {} {} {}", a, b, c, d);
        }
    }
}

// ここからライブラリ

pub trait SetMinMax {
    fn setmin(&mut self, v: Self) -> bool;
    fn setmax(&mut self, v: Self) -> bool;
}
impl<T> SetMinMax for T
where
    T: PartialOrd,
{
    fn setmin(&mut self, v: T) -> bool {
        *self > v && {
            *self = v;
            true
        }
    }
    fn setmax(&mut self, v: T) -> bool {
        *self < v && {
            *self = v;
            true
        }
    }
}

#[macro_export]
macro_rules! mat {
	($($e:expr),*) => { vec![$($e),*] };
	($($e:expr,)*) => { vec![$($e),*] };
	($e:expr; $d:expr) => { vec![$e; $d] };
	($e:expr; $d:expr $(; $ds:expr)+) => { vec![mat![$e $(; $ds)*]; $d] };
}

pub fn get_time() -> f64 {
    static mut STIME: f64 = -1.0;
    let t = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap();
    let ms = t.as_secs() as f64 + t.subsec_nanos() as f64 * 1e-9;
    unsafe {
        if STIME < 0.0 {
            STIME = ms;
        }
        // ローカル環境とジャッジ環境の実行速度差はget_timeで吸収しておくと便利
        #[cfg(feature = "local")]
        {
            (ms - STIME) * 1.0
        }
        #[cfg(not(feature = "local"))]
        {
            ms - STIME
        }
    }
}
