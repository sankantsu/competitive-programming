use std::collections::VecDeque;

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
    fn from_int(dir: usize) -> Self {
        match dir {
            0 => Self::Up,
            1 => Self::Right,
            2 => Self::Down,
            3 => Self::Left,
            _ => panic!("Invalid direction!"),
        }
    }
    fn as_int(&self) -> usize {
        match self {
            Self::Up => 0,
            Self::Right => 1,
            Self::Down => 2,
            Self::Left => 3,
        }
    }
    fn as_char(&self) -> char {
        match self {
            Self::Up => 'U',
            Self::Right => 'R',
            Self::Down => 'D',
            Self::Left => 'L',
        }
    }
    fn as_dij(&self) -> (usize, usize) {
        let di = [!0, 0, 1, 0];
        let dj = [0, 1, 0, !0];
        let dir = self.as_int();
        (di[dir], dj[dir])
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

#[derive(Clone, Debug, Copy, PartialEq)]
enum Task {
    Load,
    UnLoad,
}

struct Board {
    n: usize,
    h: Vec<Vec<i64>>,
    pos: (usize, usize),
    load: i64,
    actions: Vec<Action>,
    cost: i64,
}

impl Board {
    fn new(input: &Input) -> Self {
        let n = input.n;
        let h = input.h.clone();
        let pos = (0, 0);
        let load = 0;
        let actions = vec![];
        let cost = 0;
        Self {
            n,
            h,
            pos,
            load,
            actions,
            cost,
        }
    }
    fn at(&self, i: usize, j: usize) -> i64 {
        self.h[i][j]
    }
    fn cur_height(&self) -> i64 {
        let (i, j) = self.pos;
        self.at(i, j)
    }
    fn do_move(&mut self, dir: Direction) -> Option<()> {
        let (i, j) = self.pos;
        let (di, dj) = dir.as_dij();
        let ni = usize::wrapping_add(i, di);
        let nj = usize::wrapping_add(j, dj);
        if ni == !0 || ni == self.n || nj == !0 || nj == self.n {
            return None;
        }
        self.pos = (ni, nj);
        self.actions.push(Action::Move(dir));
        self.cost += 100 + self.load;
        Some(())
    }
    fn do_load(&mut self, val: i64) -> Option<()> {
        let (i, j) = self.pos;
        if val < 0 {
            if self.load < val.abs() {
                return None;
            }
            self.h[i][j] += val.abs();
            self.load -= val.abs();
        } else {
            self.h[i][j] -= val;
            self.load += val;
        }
        self.actions.push(Action::Load(val));
        self.cost += val.abs();
        Some(())
    }
    fn bfs(&self, task_type: Task) -> Option<(usize, usize)> {
        let n = self.n;
        let (si, sj) = self.pos;
        let mut visited = vec![vec![false; n]; n];
        let mut q = VecDeque::new();
        q.push_back((si, sj));
        visited[si][sj] = true;
        while !q.is_empty() {
            let (i, j) = q.pop_front().unwrap();
            let h = self.at(i, j);
            if (h > 0 && task_type == Task::Load) || h < 0 && task_type == Task::UnLoad {
                return Some((i, j));
            }
            let dirs = vec![0, 1, 2, 3];
            for dir in dirs {
                let mv = Direction::from_int(dir);
                let (di, dj) = mv.as_dij();
                let ni = usize::wrapping_add(i, di);
                let nj = usize::wrapping_add(j, dj);
                if ni == !0 || ni == self.n || nj == !0 || nj == self.n {
                    continue;
                }
                if visited[ni][nj] {
                    continue;
                }
                q.push_back((ni, nj));
                visited[ni][nj] = true;
            }
        }
        None
    }
    fn gen_move(&mut self, ti: usize, tj: usize) -> Vec<Direction> {
        let mut mvs = vec![];
        let (mut i, mut j) = self.pos;
        while i != ti {
            let mv = if ti < i { Direction::Up } else { Direction::Down };
            let (di, _) = mv.as_dij();
            i = usize::wrapping_add(i, di);
            mvs.push(mv);
        }
        while j != tj {
            let mv = if tj < j { Direction::Left } else { Direction::Right };
            let (_, dj) = mv.as_dij();
            j = usize::wrapping_add(j, dj);
            mvs.push(mv);
        }
        mvs
    }
    fn unload_greedy(&mut self) {
        let (i, j) = self.pos;
        let h = self.at(i, j);
        if h < 0 && self.load > 0 {
            if self.load > h.abs() {
                self.do_load(h);
            } else {
                self.do_load(-self.load);
            }
        }
    }
    fn load_step(&mut self) {
        let h = self.cur_height();
        if h > 0 {
            self.do_load(h);
        } else if h < 0 {
            self.unload_greedy();
        }
    }
    fn load_until(&mut self, max_load: i64) {
        loop {
            let target = self.bfs(Task::Load);
            if target.is_none() {
                return;
            }
            let (i, j) = target.unwrap();
            let mvs = self.gen_move(i, j);
            for mv in mvs {
                self.load_step();
                if self.load >= max_load {
                    return;
                }
                self.do_move(mv);
            }
            self.load_step();
            if self.load >= max_load {
                return;
            }
        }
    }
    fn unload_all(&mut self) {
        loop {
            let target = self.bfs(Task::UnLoad);
            if target.is_none() {
                return;
            }
            let (i, j) = target.unwrap();
            let mvs = self.gen_move(i, j);
            for mv in mvs {
                let h = self.cur_height();
                if h < 0 {
                    self.unload_greedy();
                    if self.load == 0 {
                        return;
                    }
                }
                self.do_move(mv);
            }
            let h = self.cur_height();
            if h < 0 {
                self.unload_greedy();
                if self.load == 0 {
                    return;
                }
            }
        }
    }
}

fn solve_with_delta(input: &Input, base_load: i64, delta: &Vec<i64>) -> Board {
    let mut board = Board::new(input);
    for d in delta {
        let max_load = base_load + d;
        board.load_until(max_load);
        board.unload_all();
    }
    loop {
        board.load_until(base_load);
        let load = board.load;
        board.unload_all();
        if load < base_load {
            break;
        }
    }
    board
}

fn solve(input: &Input) -> Solution {
    let mut best_sol = vec![];
    let mut best_load = !0;
    let mut best_score = 20*20*10000;

    let base_load = 100;
    for i in 0..100 {
        let mut board = Board::new(input);
        let max_load = base_load + i;
        loop {
            board.load_until(max_load);
            let load = board.load;
            board.unload_all();
            if load < max_load {
                break;
            }
        }
        let score = board.cost;
        if score < best_score {
            best_score = score;
            best_load = max_load;
            best_sol = board.actions;
            eprintln!("max_load: {max_load}, score: {best_score}");
        }
    }
    
    // playout
    let base_load = best_load;
    let mut delta = vec![];
    for _ in 0..15 {
        let mut best_delta = -100;
        let mut best_score = 20*20*10000;
        for d in 1-base_load..50 {
            let mut new_delta = delta.clone();
            new_delta.push(d);
            let board = solve_with_delta(input, base_load, &new_delta);
            let score = board.cost;
            if score < best_score {
                best_score = score;
                best_delta = d;
                best_sol = board.actions;
                eprintln!("delta: {d}, score: {best_score}");
            }
        }
        delta.push(best_delta);
    }

    Solution {
        actions: best_sol,
    }
}

fn main() {
    let inpt = input();
    let sol = solve(&inpt);
    sol.print();
}
