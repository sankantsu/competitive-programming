use ac_library::mincostflow::MinCostFlowGraph;
use rand::prelude::*;

#[derive(Clone, Debug)]
struct Input {
    n: usize,
    h: Vec<Vec<i64>>,
}

#[derive(Clone, Debug, Copy)]
enum Direction {
    Up,
    Down,
    Right,
    Left,
}

impl Direction {
    fn as_char(&self) -> char {
        match self {
            Self::Up => 'U',
            Self::Right => 'R',
            Self::Down => 'D',
            Self::Left => 'L',
        }
    }
}

#[derive(Clone, Debug, Copy)]
enum Action {
    Move(Direction),
    Load(i64),
}

impl Action {
    fn as_str(&self) -> String {
        match self {
            Self::Move(dir) => dir.as_char().to_string(),
            Self::Load(val) => {
                if *val >= 0 {
                    format!("+{val}")
                } else {
                    format!("{val}")
                }
            }
        }
    }
}

struct Solution {
    cost: i64,
    actions: Vec<Action>,
}

impl Solution {
    fn empty() -> Self {
        let cost = 20 * 20 * 10000;
        let actions = vec![];
        Self { cost, actions }
    }
    fn print(&self) {
        for act in &self.actions {
            let s = act.as_str();
            println!("{s}");
        }
    }
}

fn input() -> Input {
    proconio::input! {
        n: usize,
        h: [[i64; n]; n],
    }
    Input { n, h }
}

#[derive(Clone, Debug)]
struct Route {
    route: Vec<(usize, usize)>,
}

impl Route {
    fn new(n: usize, cols: &[usize]) -> Self {
        let mut route = vec![];
        let mut ci = 0;
        let mut cj = 0;
        while ci < n {
            while cj < n {
                let dj = if ci % 2 == 0 { 1 } else { !0 };
                route.push((ci, cj));
                let nj = usize::wrapping_add(cj, dj);
                if nj >= n {
                    break;
                }
                cj = nj;
            }
            if ci == n - 1 { break; }
            ci += 1;
        }
        for (j, &col) in cols.iter().enumerate() {
            loop {
                route.push((ci, cj));
                if cj == col { break; }
                cj += 1;
            }
            while ci < n {
                let di = if j % 2 == 0 { !0 } else { 1 };
                route.push((ci, cj));
                let ni = usize::wrapping_add(ci, di);
                if ni >= n { break; }
                ci = ni;
            }
        }
        Route { route }
    }
    fn len(&self) -> usize {
        self.route.len()
    }
    fn nth(&self, i: usize) -> (usize, usize) {
        self.route[i]
    }
    fn get_move(&self, i: usize) -> Option<Direction> {
        let (si, sj) = self.nth(i);
        let (ti, tj) = self.nth(i + 1);
        let di = ti as i64 - si as i64;
        let dj = tj as i64 - sj as i64;
        if di == 0 && dj == 0 {
            None
        } else if di == -1 && dj == 0 {
            Some(Direction::Up)
        } else if di == 1 && dj == 0 {
            Some(Direction::Down)
        } else if di == 0 && dj == 1 {
            Some(Direction::Right)
        } else if di == 0 && dj == -1 {
            Some(Direction::Left)
        } else {
            panic!("Invalid move");
        }
    }
}

fn solve_cols(input: &Input, cols: &[usize]) -> Option<Solution> {
    let n = input.n;
    let h = &input.h;

    let mut max_flow = 0;
    for i in 0..n {
        for j in 0..n {
            if h[i][j] > 0 {
                max_flow += h[i][j];
            }
        }
    }

    let route = Route::new(n, &cols);
    let m = route.len();
    // Vertices
    // 0 .. n*n: correspond to each square
    // n*m .. n*n + m: correspond to route
    // n*n + m: source
    // n*n + m + 1: sink
    let mut graph = MinCostFlowGraph::new(n*n + m + 2);
    // Edges
    // source -> square: amount of soil to carry out (cap=h, cost=0)
    let source = n*n + m;
    for k in 0..n*n {
        let i = k/n;
        let j = k%n;
        if h[i][j] > 0 {
            graph.add_edge(source, k, h[i][j], 0);
        }
    }
    // square -> sink: amount of soil to carry in (cap=-h, cost=0)
    let sink = n*n + m + 1;
    for k in 0..n*n {
        let i = k/n;
        let j = k%n;
        if h[i][j] < 0 {
            graph.add_edge(k, sink, -h[i][j], 0);
        }
    }
    // route(i) -> route(i + 1): dump can carry soils along this flow (cap=inf, cost=1)
    let inf = 1000000;
    for k in 0..(m - 1) {
        graph.add_edge(n*n + k, n*n + k + 1, inf, 1);
    }
    // square -> route(i): load soils (cap=inf, cost=1)
    // route(i) -> square: unload soils (cap=inf, cost=1)
    for k in 0..m {
        let (i, j) = route.nth(k);
        graph.add_edge(n*i + j, n*n + k, inf, 1);
        graph.add_edge(n*n + k, n*i + j, inf, 1);
    }
    let (flow, _) = graph.flow(source, sink, inf);
    if flow < max_flow {
        return None
    }

    let edges = graph.edges();
    let n_edge = edges.len();
    let mut load_seq = vec![];
    for k in 0..m {
        let base = n_edge - 2*m + 2*k;
        let load = edges[base].flow;
        let unload = edges[base + 1].flow;
        if load > 0 {
            load_seq.push(load);
        } else if unload > 0 {
            load_seq.push(-unload);
        } else {
            load_seq.push(0);
        }
    }

    let mut v = 0;
    let mut cost = 0;
    let mut actions = vec![];
    for k in 0..(m - 1) {
        let load = load_seq[k];
        if load != 0 {
            actions.push(Action::Load(load));
            cost += load.abs();
            v += load;
        }
        let mv = route.get_move(k);
        if mv.is_some() {
            actions.push(Action::Move(mv.unwrap()));
            cost += 100 + v;
        }
    }
    let load = load_seq[m - 1];
    if load != 0 {
        actions.push(Action::Load(load));
        cost += load;
        v += load;
    }
    assert_eq!(v, 0);
    Some(Solution { cost, actions })
}

fn climb(input: &Input, cols: &mut Vec<usize>) -> Option<Solution> {
    let n = input.n;
    let mut rng = thread_rng();
    let mut best_sol = solve_cols(input, &cols)?;

    let n_iter = 20;
    for _ in 0..n_iter {
        let r = rng.gen_range(0..10);
        if r < 8 {
            // remove col
            if cols.len() == 1 {
                continue;
            }
            let idx = rng.gen_range(0..cols.len());
            let col = cols[idx];
            cols.remove(idx);
            let sol = solve_cols(input, &cols);
            if sol.is_none() { continue; }
            let sol = sol.unwrap();
            if sol.cost < best_sol.cost {
                // eprintln!("Iter: {}, Cost: {}", iter, sol.cost);
                best_sol = sol;
            } else {
                cols.insert(idx, col);
            }
        } else {
            // add col
            let col = rng.gen_range(0..n);
            if cols.contains(&col) { continue; }
            let idx = cols.iter().position(|&c| c > col).unwrap_or(cols.len());
            cols.insert(idx, col);
            let sol = solve_cols(input, &cols);
            if sol.is_none() { continue; }
            let sol = sol.unwrap();
            if sol.cost < best_sol.cost {
                // eprintln!("Iter: {}, Cost: {}", iter, sol.cost);
                best_sol = sol;
            } else {
                cols.remove(idx);
            }
        }
    }
    Some(best_sol)
}

fn gen_random_cols(n: usize) -> Vec<usize> {
    let mut rng = thread_rng();
    let mut cols = vec![];
    for col in 0..n {
        let r = rng.gen_range(0..2);
        if r == 0 {
            cols.push(col);
        }
    }
    cols
}

fn solve(input: &Input) -> Solution {
    let n = input.n;
    let mut best_sol = Solution::empty();
    let start = std::time::Instant::now();
    let mut iter = 0;
    loop {
        iter += 1;
        let t = start.elapsed();
        if t > std::time::Duration::from_millis(1800) {
            break;
        }
        let mut cols = gen_random_cols(n);
        let sol = climb(input, &mut cols);
        if sol.is_none() { continue; }
        let sol = sol.unwrap();
        if sol.cost < best_sol.cost {
            eprintln!("Iter: {}, Cost: {}", iter, sol.cost);
            best_sol = sol;
        }
    }
    best_sol
}

fn main() {
    let input_ = input();
    let sol = solve(&input_);
    sol.print();
}
