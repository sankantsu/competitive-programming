use itertools::Itertools;
use std::collections::VecDeque;

struct Input {
    n: usize,
    a: Vec<Vec<u32>>,
}

impl Input {
    fn from_stdin() -> Self {
        proconio::input! {
            n: usize,
            a: [[u32; n]; n],
        }
        Input { n, a }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Move {
    Stay,
    Pick,
    Release,
    Move(usize),
    Bomb,
}

impl Move {
    #[allow(unused)]
    fn from_char(c: char) -> Self {
        match c {
            '.' => Self::Stay,
            'P' => Self::Pick,
            'Q' => Self::Release,
            'U' => Self::Move(0),
            'L' => Self::Move(1),
            'D' => Self::Move(2),
            'R' => Self::Move(3),
            'B' => Self::Bomb,
            _ => panic!("Invalid character for crane action."),
        }
    }
    fn to_char(&self) -> char {
        match self {
            Self::Stay => '.',
            Self::Pick => 'P',
            Self::Release => 'Q',
            Self::Move(0) => 'U',
            Self::Move(1) => 'L',
            Self::Move(2) => 'D',
            Self::Move(3) => 'R',
            Self::Bomb => 'B',
            _ => panic!("Invalid character for crane action."),
        }
    }
    fn next(&self, xy: (usize, usize), n: usize) -> Option<(usize, usize)> {
        let (x, y) = xy;
        let nxy = match self {
            Self::Stay | Self::Pick | Self::Release => (x, y),
            Self::Bomb => (!0, !0),
            Self::Move(dir) => {
                let dx = [-1, 0, 1, 0];
                let dy = [0, -1, 0, 1];
                let nx = x as i32 + dx[*dir];
                let ny = y as i32 + dy[*dir];
                if nx < 0 || n as i32 <= nx || ny < 0 || n as i32 <= ny {
                    return None;
                }
                (nx as usize, ny as usize)
            }
        };
        Some(nxy)
    }
}

#[derive(Clone, Debug)]
struct Crane {
    large: bool,    // large carne can overlap with container while carrying
    x: usize,       // x position
    y: usize,       // y position
    container: i32, // container id carried by the crane (-1 if not carrying)
}

impl Crane {
    fn bombed(&self) -> bool {
        self.x == !0
    }
}

#[derive(Clone, Debug)]
struct State {
    queue: Vec<Vec<u32>>, // container queue which contains containers have not yet carried in
    done: Vec<Vec<u32>>,  // carried out containers from each row
    board: Vec<Vec<i32>>,
    cranes: Vec<Crane>,
}

impl State {
    fn new(input: &Input) -> Self {
        let n = input.n;
        let queue = input
            .a
            .iter()
            .map(|v| v.clone().into_iter().rev().collect_vec())
            .collect_vec();
        let done = vec![vec![]; n];
        let board = vec![vec![-1; n]; n];
        let cranes = (0..n)
            .map(|i| Crane {
                large: if i == 0 { true } else { false },
                x: i,
                y: 0,
                container: -1,
            })
            .collect_vec();
        let mut state = Self {
            queue,
            done,
            board,
            cranes,
        };
        state.carry_in();
        state
    }
    fn len(&self) -> usize {
        self.cranes.len()
    }
    fn carry_in(&mut self) {
        for i in 0..self.len() {
            if self.board[i][0] == -1
                && self
                    .cranes
                    .iter()
                    .all(|c| c.container == -1 || (c.x, c.y) != (i, 0))
            {
                self.board[i][0] = self.queue[i].pop().map_or(-1, |v| v as i32)
            }
        }
    }
    fn carry_out(&mut self) {
        let n = self.len();
        for i in 0..self.len() {
            let c = self.board[i][n - 1];
            if c != -1 {
                self.done[i].push(c as u32);
                self.board[i][n - 1] = -1;
            }
        }
    }
    fn execute(&mut self, moves: &Vec<Vec<Move>>) -> Result<(), String> {
        for mv in moves {
            self.step(mv)?
        }
        Ok(())
    }
    fn step(&mut self, mv: &Vec<Move>) -> Result<(), String> {
        let mut next = self.clone();
        let n = self.len();
        for i in 0..self.len() {
            let x = self.cranes[i].x;
            let y = self.cranes[i].y;
            let c = self.cranes[i].container;
            let large = self.cranes[i].large;
            match mv[i] {
                Move::Stay => (),
                Move::Pick => {
                    if self.cranes[i].bombed() {
                        return Err(format!("Crane {i} has already bombed."));
                    }
                    if self.cranes[i].container != -1 {
                        return Err(format!("Crane {i} holds container."));
                    }
                    if self.board[x][y] == -1 {
                        return Err(format!("No container at {x} {y}."));
                    }
                    next.cranes[i].container = self.board[x][y];
                    next.board[x][y] = -1;
                }
                Move::Release => {
                    if self.cranes[i].bombed() {
                        return Err(format!("Crane {i} has already bombed."));
                    }
                    if self.cranes[i].container == -1 {
                        return Err(format!("Crane {i} does not hold a container."));
                    }
                    if self.board[x][y] != -1 {
                        return Err(format!("Container already exists at {x} {y}."));
                    }
                    next.cranes[i].container = -1;
                    next.board[x][y] = self.cranes[i].container;
                }
                Move::Move(_) => {
                    if self.cranes[i].bombed() {
                        return Err(format!("crane {i} has already bombed."));
                    }
                    let nxy = mv[i].next((x, y), n);
                    if let Some((nx, ny)) = nxy {
                        if !large && c != -1 && self.board[nx][ny] != -1 {
                            return Err(format!("Crane {i} cannot move over a container."));
                        }
                        next.cranes[i].x = nx;
                        next.cranes[i].y = ny;
                    } else {
                        return Err(format!("Crane {i} moved out of the board."));
                    }
                }
                Move::Bomb => {
                    if self.cranes[i].bombed() {
                        return Err(format!("Crane {i} has already bombed."));
                    }
                    if self.cranes[i].container != -1 {
                        return Err(format!("Cannot bomb crane {i} carrying a container."));
                    }
                    next.cranes[i].x = !0;
                    next.cranes[i].y = !0;
                }
            }
        }
        for i in 0..self.len() {
            if next.cranes[i].bombed() {
                continue;
            }
            for j in 0..i {
                if next.cranes[j].bombed() {
                    continue;
                }
                let pi = (self.cranes[i].x, self.cranes[i].y);
                let pj = (self.cranes[j].x, self.cranes[j].y);
                let qi = (next.cranes[i].x, next.cranes[i].y);
                let qj = (next.cranes[j].x, next.cranes[j].y);
                if qi == qj || (qi == pj && qj == pi) {
                    return Err(format!("Crane {j} and {i} collided."));
                }
            }
        }
        *self = next;
        self.carry_out();
        self.carry_in();
        Ok(())
    }
    fn search(&self, container: u32) -> (usize, usize, usize) {
        let n = self.len();
        for i in 0..n {
            for j in 0..n {
                if self.board[i][j] == container as i32 {
                    return (0, i, j);
                }
            }
        }
        //for crane in &self.cranes {
        //    if crane.container == container as i32 {
        //        let x = crane.x;
        //        let y = crane.y;
        //        return (1, x, y);
        //    }
        //}
        for i in 0..n {
            let k = self.queue[i].len();
            for j in 0..k {
                if self.queue[i][k - 1 - j] == container {
                    return (2, j, i);
                }
            }
        }
        (!0, !0, !0)
    }
    fn search_free_space(&self) -> (usize, usize) {
        let n = self.len();
        for i in 0..n {
            for j in (2..n - 1).rev() {
                if self.board[i][j] == -1 {
                    return (i, j);
                }
            }
        }
        (!0, !0)
    }
    // search shortest path and returns (all possible) first move if possible
    fn bfs(&self, from: (usize, usize), to: (usize, usize), move_over: bool) -> Vec<Move> {
        let dx = [-1, 0, 1, 0];
        let dy = [0, -1, 0, 1];
        let n = self.len();
        let (sx, sy) = from;
        let (tx, ty) = to;
        let out_of_range = |x, y| {
            x < 0 || n as i32 <= x || y < 0 || n as i32 <= y
        };
        let mut mv = vec![];
        let mut min_dist = 1usize<<60;
        for dir in 0..4 {
            let mut dist = vec![vec![1usize<<60; n]; n];
            let sx1 = sx as i32 + dx[dir];
            let sy1 = sy as i32 + dy[dir];
            if out_of_range(sx1, sy1) { continue; }
            if !move_over && self.board[sx1 as usize][sy1 as usize] != -1 { continue; }
            let mut queue = VecDeque::new();
            queue.push_back((sx1, sy1, 1));
            dist[sx][sy] = 0;
            dist[sx1 as usize][sy1 as usize] = 1;
            while !queue.is_empty() {
                let (x, y, d) = queue.pop_front().unwrap();
                if x == tx as i32 && y == ty as i32 {
                    if d < min_dist {
                        min_dist = d;
                        mv = vec![Move::Move(dir)];
                    } else if d == min_dist {
                        mv.push(Move::Move(dir));
                    }
                    break;
                }
                for dir1 in 0..4 {
                    let nx = x as i32 + dx[dir1];
                    let ny = y as i32 + dy[dir1];
                    if out_of_range(nx, ny) { continue; }
                    if !move_over && self.board[nx as usize][ny as usize] != -1 { continue; }
                    if dist[nx as usize][ny as usize] < d { continue; }
                    queue.push_back((nx, ny, d + 1));
                    dist[nx as usize][ny as usize] = d + 1;
                }
            }
        }
        mv
    }
}

struct Solution {
    actions: Vec<Vec<Move>>,
}

impl Solution {
    fn print(&self) {
        for act in &self.actions {
            let s = act.iter().map(|mv| mv.to_char()).collect::<String>();
            println!("{}", s);
        }
    }
}

fn extend_move(moves: &Vec<Move>, n: usize) -> Vec<Move> {
    let mut ext = moves.clone();
    while ext.len() < n {
        ext.push(Move::Stay);
    }
    ext
}

struct Solver {
    input: Input,
}

impl Solver {
    fn new(input: Input) -> Self {
        Self { input }
    }
    fn initial_moves(&self, n_crane: usize) -> Vec<Vec<Move>> {
        let n = self.input.n;
        let mut actions = vec![];
        let mut bomb = vec![Move::Bomb; n];
        for i in 0..n_crane {
            bomb[i] = Move::Stay;
        }
        actions.push(bomb);
        actions
    }
    fn solve(&self) -> Solution {
        let n = self.input.n;
        let mut actions = vec![];
        let mut cnt = vec![0; n];
        let mut state = State::new(&self.input);

        let n_crane = 2;
        let mut bombed = vec![false; n_crane];
        actions.append(&mut self.initial_moves(n_crane));
        state.execute(&actions).unwrap();

        let mut turn = 0;
        while !state.done.iter().map(|v| v.len()).all(|x| x == n) {
            if actions.len() > 1000 {
                break;
            }
            let cand = cnt.iter().enumerate().map(|(i, x)| n * i + x).collect_vec();
            let mut pos = cand.iter().map(|id| state.search(*id as u32)).collect_vec();
            pos = pos.into_iter().filter(|(k,_,_)| *k != 1).collect();
            pos.sort();
            let mut target = 0;
            let mut act = vec![];
            let mut next = vec![];
            for i in 0..n_crane {
                let mut override_next = vec![];
                let x = state.cranes[i].x as i32;
                let y = state.cranes[i].y as i32;
                let large = state.cranes[i].large;
                let dest;
                if bombed[i] {
                    dest = (-1, -1);
                } else if state.cranes[i].container != -1 {
                    if cand.contains(&(state.cranes[i].container as usize)) {
                        dest = (state.cranes[i].container / n as i32, (n - 1) as i32);
                    } else {
                        let (i, j) = state.search_free_space();
                        dest = (i as i32, j as i32);
                    }
                }
                else {
                    let (k, i, j) = pos[target];
                    target += 1;
                    if k == 0 {
                        dest = (i as i32, j as i32);
                    } else if k == 2 {
                        let i = j;
                        dest = (i as i32, 0);
                    }
                    else {
                        dest = (-1, -1);
                        eprintln!("Container is already holded by another crane!");
                    }
                }
                let ok = |nx, ny| {
                    if !large && state.cranes[i].container != -1 && state.board[nx as usize][ny as usize] != -1 {
                        return false
                    }
                    for i in 0..next.len() {
                        let p1 = (state.cranes[i].x as i32, state.cranes[i].y as i32);
                        let p2 = (x, y);
                        let q1 = next[i];
                        let q2 = (nx, ny);
                        if q1 == q2 {
                            return false
                        }
                        if q1 == p2 && q2 == p1 {
                            return false
                        }
                    }
                    true
                };
                let mut nx = x;
                let mut ny = y;
                let mut mv = None;
                if dest == (-1, -1) {
                    if !bombed[i] {
                        bombed[i] = true;
                        mv = Some(Move::Bomb);
                    } else {
                        mv = Some(Move::Stay);
                    }
                }
                else if (x,y) == dest {
                    if state.cranes[i].container == -1 && ok(x, y) {
                        nx = x;
                        ny = y;
                        mv = Some(Move::Pick);
                    } else if state.cranes[i].container != -1 {
                        for j in 0..i {
                            // cancel previous move
                            if next[j] == (x, y) {
                                override_next.push((j, (state.cranes[j].x as i32, state.cranes[j].y as i32)));
                                act[j] = Move::Stay;
                            }
                        }
                        mv = Some(Move::Release);
                        if dest.1 as usize == n - 1 {
                            cnt[dest.0 as usize] += 1;
                        }
                    }
                } else {
                    let (tx, ty) = dest;
                    let mv_cand = state.bfs(
                            (x as usize, y as usize),
                            (tx as usize, ty as usize),
                            large || state.cranes[i].container == -1
                        );
                    for mv1 in &mv_cand {
                        let (nx1, ny1) = mv1.next((x as usize, y as usize), n).unwrap();
                        if ok(nx1 as i32, ny1 as i32) {
                            nx = nx1 as i32; ny = ny1 as i32;
                            mv = Some(mv1.clone());
                        }
                    }
                }
                if mv.is_none() {
                    if next.iter().all(|(px,py)| (*px,*py) != (x,y)) {
                        mv = Some(Move::Stay);
                    } else {
                        // prefer evacuate to left rather than up.
                        // it sometimes prevent small crane stucks at wrong destination.
                        for dir in [1, 3, 2, 0] {
                            let mv1 = Move::Move(dir);
                            if let Some((nx1, ny1)) = mv1.next((x as usize, y as usize), n) {
                                if ok(nx1 as i32, ny1 as i32) {
                                    nx = nx1 as i32; ny = ny1 as i32;
                                    mv = Some(mv1);
                                    break;
                                }
                            }
                        }
                    }
                }
                if mv.is_none() {
                    for j in 0..i {
                        // cancel previous move
                        if next[j] == (x, y) {
                            next[j] = (state.cranes[j].x as i32, state.cranes[j].y as i32);
                            act[j] = Move::Stay;
                        }
                    }
                    if state.cranes[i].container != -1 {
                        if y == (n - 1) as i32 {
                            panic!("You cannot release container at a wrong destination.");
                        }
                        mv = Some(Move::Release);
                    } else {
                        panic!("Unknown situation! debug me.");
                    }
                }
                for (j, (x, y)) in override_next.into_iter() {
                    next[j] = (x, y);
                }
                act.push(mv.unwrap());
                next.push((nx, ny));
            }
            let ext_act = extend_move(&act, n);
            eprintln!("turn: {}: {}", turn, ext_act.iter().map(|mv| mv.to_char()).collect::<String>());
            state.step(&ext_act).unwrap();
            actions.push(ext_act);
            turn += 1;
        }
        actions = (0..actions[0].len())
            .map(|i| actions.iter().map(|inner| inner[i].clone()).collect_vec())
            .collect();
        Solution { actions }
    }
}

fn main() {
    let input = Input::from_stdin();
    let solver = Solver::new(input);
    let sol = solver.solve();
    sol.print();
}
