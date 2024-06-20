use ac_library::mincostflow::MinCostFlowGraph;

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
    actions: Vec<Action>,
}

impl Solution {
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

fn solve(input: &Input) -> Solution {
    let n = input.n;
    let h = &input.h;

    let cols = (0..n/2).map(|j| 2*j).collect::<Vec<_>>();
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
    let (flow, cost) = graph.flow(source, sink, inf);
    dbg!(flow, cost);

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

    let mut actions = vec![];
    for k in 0..(m - 1) {
        let load = load_seq[k];
        if load != 0 {
            actions.push(Action::Load(load));
        }
        let mv = route.get_move(k);
        if mv.is_some() {
            actions.push(Action::Move(mv.unwrap()));
        }
    }
    let load = load_seq[m - 1];
    if load != 0 {
        actions.push(Action::Load(load));
    }
    Solution { actions }
}

fn main() {
    let inpt = input();
    let sol = solve(&inpt);
    sol.print();
}
