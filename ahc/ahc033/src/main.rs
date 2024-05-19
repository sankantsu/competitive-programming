use itertools::Itertools;
use std::iter;

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

#[derive(Clone, Debug)]
enum Move {
    Stay,
    Pick,
    Release,
    Move(usize),
    Bomb,
}

impl Move {
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
        for crane in &self.cranes {
            if crane.container == container as i32 {
                let x = crane.x;
                let y = crane.y;
                return (1, x, y);
            }
        }
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
            for j in 0..n - 1 {
                if self.board[i][j] == -1 {
                    return (i, j);
                }
            }
        }
        (!0, !0)
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

fn gen_move(p: (usize, usize), q: (usize, usize)) -> Vec<Move> {
    let di = q.0 as i32 - p.0 as i32;
    let dj = q.1 as i32 - p.1 as i32;
    let mut moves = vec![];
    if di > 0 {
        moves.append(&mut iter::repeat('D').take(di as usize).collect_vec());
    } else {
        moves.append(&mut iter::repeat('U').take(-di as usize).collect_vec());
    }
    if dj > 0 {
        moves.append(&mut iter::repeat('R').take(dj as usize).collect_vec());
    } else {
        moves.append(&mut iter::repeat('L').take(-dj as usize).collect_vec());
    }
    moves.iter().map(|c| Move::from_char(*c)).collect_vec()
}

fn extend_moves(moves: &Vec<Move>, n: usize) -> Vec<Vec<Move>> {
    let mut ext_moves = vec![];
    for mv in moves {
        let mut ext = vec![Move::Stay; n];
        ext[0] = mv.clone();
        ext_moves.push(ext);
    }
    ext_moves
}

struct Solver {
    input: Input,
}

impl Solver {
    fn new(input: Input) -> Self {
        Self { input }
    }
    fn initial_moves(&self) -> Vec<Vec<Move>> {
        let n = self.input.n;
        let mut actions = vec![];
        for i in 0..n-2 {
            actions.push(vec![Move::Pick; n]);
            for _ in 0..n-2-i {
                actions.push(vec![Move::from_char('R'); n]);
            }
            actions.push(vec![Move::Release; n]);
            for _ in 0..n-2-i {
                actions.push(vec![Move::from_char('L'); n]);
            }
        }
        let mut bomb = vec![Move::Bomb; n];
        bomb[0] = Move::Stay;
        actions.push(bomb);

        actions
    }
    fn solve(&self) -> Solution {
        let n = self.input.n;
        let mut actions = vec![];
        let mut cnt = vec![0; n];
        let mut state = State::new(&self.input);

        actions.append(&mut self.initial_moves());
        state.execute(&actions).unwrap();

        while !state.done.iter().map(|v| v.len()).all(|x| x == n) {
            let cand = cnt.iter().enumerate().map(|(i, x)| n * i + x).collect_vec();
            let mut pos = cand.iter().map(|id| state.search(*id as u32)).collect_vec();
            pos.sort();

            let mut act = vec![];
            let x = state.cranes[0].x;
            let y = state.cranes[0].y;
            let (k, i, j) = pos[0];
            if k == 0 {
                let dest = state.board[i][j] as usize / n;
                act.append(&mut gen_move((x, y), (i, j)));
                act.push(Move::Pick);
                act.append(&mut gen_move((i, j), (dest, n - 1)));
                act.push(Move::Release);
                cnt[dest] += 1;
            } else if k == 2 {
                let i = j;
                let (tx, ty) = state.search_free_space();
                act.append(&mut gen_move((x, y), (i, 0)));
                act.push(Move::Pick);
                act.append(&mut gen_move((i, 0), (tx, ty)));
                act.push(Move::Release);
            }
            let mut ext_act = extend_moves(&act, n);
            state.execute(&ext_act).unwrap();
            actions.append(&mut ext_act);
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
