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
                let dx = [!0, 0, 1, 0];
                let dy = [0, !0, 0, 1];
                let nx = usize::wrapping_add(x, dx[*dir]);
                let ny = usize::wrapping_add(y, dy[*dir]);
                if !(0..n).contains(&nx) || !(0..n).contains(&ny) {
                    return None;
                }
                (nx, ny)
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
    fn get_pos(&self) -> (usize, usize) {
        (self.x, self.y)
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
    fn next_containers_to_caryy_out(&self) -> Vec<i32> {
        let n = self.len();
        (0..n)
            .enumerate()
            .map(|(i, x)| {
                if self.done[i].len() == n {
                    -1
                } else {
                    (n * x + self.done[i].len()) as i32
                }
            })
            .collect()
    }
    fn get_crane_pos(&self, i: usize) -> (usize, usize) {
        self.cranes[i].get_pos()
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
    fn search_free_space_list(&self) -> Vec<(usize, usize)> {
        let n = self.len();
        let mut res = vec![];
        for i in 0..n {
            for j in (2..n - 1).rev() {
                if self.board[i][j] == -1 {
                    res.push((i, j));
                }
            }
        }
        res
    }
    // search shortest path and returns (all possible) first move if possible
    fn bfs(&self, from: (usize, usize), to: (usize, usize), move_over: bool) -> Vec<Move> {
        let dx = [!0, 0, 1, 0];
        let dy = [0, !0, 0, 1];
        let n = self.len();
        let (sx, sy) = from;
        let (tx, ty) = to;
        let out_of_range = |x: usize, y: usize| !(0..n).contains(&x) || !(0..n).contains(&y);
        let mut mv = vec![];
        let mut min_dist = 1usize << 60;
        for dir in 0..4 {
            let mut dist = vec![vec![1usize << 60; n]; n];
            let sx1 = usize::wrapping_add(sx, dx[dir]);
            let sy1 = usize::wrapping_add(sy, dy[dir]);
            if out_of_range(sx1, sy1) {
                continue;
            }
            if !move_over && self.board[sx1][sy1] != -1 {
                continue;
            }
            let mut queue = VecDeque::new();
            queue.push_back((sx1, sy1, 1));
            dist[sx1][sy1] = 1;
            while !queue.is_empty() {
                let (x, y, d) = queue.pop_front().unwrap();
                if x == tx && y == ty {
                    if d < min_dist {
                        min_dist = d;
                        mv = vec![Move::Move(dir)];
                    } else if d == min_dist {
                        mv.push(Move::Move(dir));
                    }
                    break;
                }
                for dir1 in 0..4 {
                    let nx = usize::wrapping_add(x, dx[dir1]);
                    let ny = usize::wrapping_add(y, dy[dir1]);
                    if out_of_range(nx, ny) {
                        continue;
                    }
                    if !move_over && self.board[nx][ny] != -1 {
                        continue;
                    }
                    if dist[nx][ny] < d {
                        continue;
                    }
                    queue.push_back((nx, ny, d + 1));
                    dist[nx][ny] = d + 1;
                }
            }
        }
        mv
    }
    fn reachable(&self, from: (usize, usize), to: (usize, usize), move_over: bool) -> bool {
        self.bfs(from, to, move_over).len() != 0
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
    state: State,
}

impl Solver {
    fn new(input: Input) -> Self {
        let state = State::new(&input);
        Self { input, state }
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
    // returns mapping of: crane id => destination
    fn match_crane_with_target(&self, n_crane: usize) -> Vec<(usize, usize)> {
        let n = self.input.n;
        let cand = self.state.next_containers_to_caryy_out();
        let mut pos = cand
            .iter()
            .map(|id| self.state.search(*id as u32))
            .collect_vec();
        pos.sort();

        let dest_of_container = |cont, start, is_large, dests: &Vec<(usize, usize)>| {
            if cand.contains(&cont) {
                // this container can be carried out
                (cont as usize / n, n - 1)
            } else {
                let lst = self.state.search_free_space_list();
                for &dest in &lst {
                    if self.state.reachable(start, dest, is_large) && !dests.contains(&dest) {
                        return dest;
                    }
                }
                (!0, !0)
            }
        };
        // if dests[i] remains (!0, !0), there is no task for crane i in this turn
        let mut dests = vec![(!0, !0); n_crane];
        for i in 0..n_crane {
            let (x, y) = self.state.get_crane_pos(i);
            let cont = self.state.cranes[i].container;
            let large = self.state.cranes[i].large;
            if cont != -1 {
                // crane is holding some container
                let dest = dest_of_container(cont, (x, y), large, &dests);
                let reachable = self.state.reachable((x, y), dest, large);
                if reachable {
                    // Move toward the destination
                    dests[i] = dest;
                } else if y != n - 1 {
                    // The container currently holded by this crane
                    // cannot be carried to the destination.
                    // Release the container at current position.
                    dests[i] = (x, y);
                } else {
                    // maybe stuck
                    // stuck[i] = true;
                    dests[i] = dest;
                }
            } else {
                // crane is currently not holding a container
                // pick some container from stock
                let mut target = !0;
                for j in 0..pos.len() {
                    let (k, a, b) = pos[j];
                    let dest1: (usize, usize); // pick at dest1
                    let dest2: (usize, usize); // release at dest1
                    if k == 0 {
                        // pick a container already on the board
                        let c = self.state.board[a][b];
                        dest1 = (a, b);
                        dest2 = dest_of_container(c, dest1, large, &dests);
                    } else if k == 2 {
                        // move away a container in front of the queue
                        let c = self.state.board[b][0];
                        dest1 = (b, 0);
                        dest2 = dest_of_container(c, dest1, large, &dests);
                    } else {
                        // container is holded by another crane
                        continue;
                    }
                    let reachable = self.state.reachable(dest1, dest2, large);
                    if reachable {
                        // task of crane i was determined
                        target = j;
                        dests[i] = dest1;
                        break;
                    }
                }
                if target != !0 {
                    pos.remove(target);
                }
            }
        }
        dests
    }
    fn validate_turn_action(&self, cand: &Vec<Move>) -> bool {
        let n = self.input.n;
        let n_crane = cand.len();
        let mut next = vec![];
        for i in 0..n_crane {
            let cur = self.state.get_crane_pos(i);
            let mv = &cand[i];
            next.push(mv.next(cur, n).unwrap());
        }
        // check colllision
        let mut ok = true;
        for i in 0..n_crane {
            for j in (i + 1)..n_crane {
                let pi = self.state.get_crane_pos(i);
                let pj = self.state.get_crane_pos(j);
                let qi = next[i];
                let qj = next[j];
                if qi == qj || (qi == pj && qj == pi) {
                    ok = false;
                }
            }
        }
        ok
    }
    fn consider_next_move(&self, dests: &Vec<(usize, usize)>) -> Vec<Move> {
        let n = self.input.n;
        let n_crane = dests.len();
        let mut preferable_moves = vec![];
        for i in 0..n_crane {
            let mut mvs;
            let dest = dests[i];
            let (x, y) = self.state.get_crane_pos(i);
            let cont = self.state.cranes[i].container;
            let large = self.state.cranes[i].large;
            if dest == (!0, !0) {
                // No task for this crane
                // Any move is ok
                mvs = vec![Move::Stay];
                for dir in 0..4 {
                    let mv = Move::Move(dir);
                    if mv.next((x, y), n).is_some() {
                        mvs.push(mv);
                    }
                }
            } else if dest == (x, y) {
                // current position is the destination
                if cont == -1 {
                    assert_ne!(self.state.board[x][y], -1);
                    mvs = vec![Move::Pick];
                } else {
                    mvs = vec![Move::Release];
                }
            } else {
                let mv_cand =
                    self.state
                        .bfs((x, y), dest, large || self.state.cranes[i].container == -1);
                mvs = mv_cand;
            }
            preferable_moves.push(mvs);
        }
        let mut possible_moves = vec![];
        for i in 0..n_crane {
            let mut mvs = vec![Move::Stay];
            let (x, y) = self.state.get_crane_pos(i);
            let cont = self.state.cranes[i].container;
            let large = self.state.cranes[i].large;
            for dir in 0..4 {
                let mv = Move::Move(dir);
                if let Some((nx, ny)) = mv.next((x, y), n) {
                    if large || cont == -1 || self.state.board[nx][ny] == -1 {
                        mvs.push(mv);
                    }
                }
            }
            possible_moves.push(mvs);
        }
        // try the candidates with more preferable moves first
        for k in 0..=n_crane {
            let mut acceptable_cands = vec![];
            for comb in (0..n_crane).combinations(k) {
                let mut cand_moves = vec![];
                for i in 0..n_crane {
                    if comb.contains(&i) {
                        cand_moves.push(possible_moves[i].clone());
                    } else {
                        cand_moves.push(preferable_moves[i].clone());
                    }
                }
                for cand in cand_moves.iter().multi_cartesian_product() {
                    let cand = cand.into_iter().cloned().collect_vec();
                    if cand.iter().all(|mv| *mv == Move::Stay) {
                        continue;
                    }
                    let ok = self.validate_turn_action(&cand);
                    if ok {
                        acceptable_cands.push(cand);
                    }
                }
            }
            if !acceptable_cands.is_empty() {
                // Prioritize pick/release over other actions
                acceptable_cands.sort_by_key(|cand| {
                    cand.iter()
                        .filter(|&mv| *mv == Move::Pick || *mv == Move::Release)
                        .count()
                });
                return acceptable_cands.pop().unwrap();
            }
        }
        panic!("Cannot find move candidate!");
    }
    fn solve(&mut self) -> Solution {
        let n = self.input.n;
        let mut actions = vec![];

        let n_crane = 3;
        actions.append(&mut self.initial_moves(n_crane));
        self.state.execute(&actions).unwrap();

        let mut turn = 0;
        let max_turn = 1000;
        while !self.state.done.iter().map(|v| v.len()).all(|x| x == n) {
            if turn >= max_turn {
                break;
            }
            let dests = self.match_crane_with_target(n_crane);
            let act = self.consider_next_move(&dests);
            let ext_act = extend_move(&act, n);
            eprintln!(
                "turn: {}: {}",
                turn,
                ext_act.iter().map(|mv| mv.to_char()).collect::<String>()
            );
            self.state.step(&ext_act).unwrap();
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
    let mut solver = Solver::new(input);
    let sol = solver.solve();
    sol.print();
}
