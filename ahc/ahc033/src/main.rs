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

fn manhattan_distance(x: (usize, usize), y: (usize, usize)) -> usize {
    let dist = |a, b| usize::max(a, b) - usize::min(a, b);
    dist(x.0, y.0) + dist(x.1, y.1)
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

#[derive(Clone)]
enum ContainerState {
    Done,                // already have carried out
    Carrying(usize),     // Carrying(i): i th crane is carrying this container
    Queue(usize, usize), // Queue(i, depth): this container is in i the queue
    Board(usize, usize), // Board(x, y): this container is at (x, y) position of the board
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
    #[allow(unused)]
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
    fn next_containers_to_caryy_out(&self) -> Vec<usize> {
        let n = self.len();
        let n_done = (0..n).map(|i| self.done[i].len()).collect_vec();
        (0..n).filter(|i| n_done[*i] < n).map(|i| n*i + n_done[i]).collect()
    }
    fn get_crane_pos(&self, i: usize) -> (usize, usize) {
        self.cranes[i].get_pos()
    }
    fn search_container(&self, container: u32) -> ContainerState {
        let n = self.len();
        for i in 0..n {
            for j in 0..n {
                if self.board[i][j] == container as i32 {
                    return ContainerState::Board(i, j)
                }
            }
        }
        for i in 0..n {
            let k = self.queue[i].len();
            for depth in 0..k {
                if self.queue[i][k - 1 - depth] == container {
                    return ContainerState::Queue(i, depth)
                }
            }
        }
        for i in 0..n {
            let cont = self.cranes[i].container;
            if container as i32 == cont {
                return ContainerState::Carrying(i)
            }
        }
        ContainerState::Done
    }
    fn list_all_container_state(&self) -> Vec<ContainerState> {
        let n = self.len();
        let mut states = vec![ContainerState::Done; n*n];
        for i in 0..n {
            for j in 0..n {
                if self.board[i][j] != -1 {
                    states[self.board[i][j] as usize] = ContainerState::Board(i, j);
                }
            }
        }
        for i in 0..n {
            let k = self.queue[i].len();
            for depth in 0..k {
                states[self.queue[i][k - 1 - depth] as usize] = ContainerState::Queue(i, depth);
            }
        }
        for i in 0..n {
            let cont = self.cranes[i].container;
            if cont != -1 {
                states[self.cranes[i].container as usize] = ContainerState::Carrying(i);
            }
        }
        states
    }
    fn determine_target_containers(&self) -> Vec<usize> {
        let n = self.len();
        let container_states = (0..n*n).map(|cont| self.search_container(cont as u32)).collect_vec();
        let mut cand = self.next_containers_to_caryy_out();
        let mut targets = vec![];
        let mut n_kicked = vec![0; n];
        while targets.len() < n && !cand.is_empty() {
            let calc_n_kick = |cs: &ContainerState| {
                match cs {
                    ContainerState::Board(_, _) => 0,
                    ContainerState::Carrying(_) => 0,
                    ContainerState::Queue(i, d) => usize::max(0, d + 1 - n_kicked[*i]),
                    ContainerState::Done => usize::MAX,
                }
            };
            cand.sort_by_key(|cont| calc_n_kick(&container_states[*cont]));
            let cont = cand[0];
            targets.push(cont);
            if let ContainerState::Queue(i, d) = container_states[cont] {
                n_kicked[i] = usize::max(n_kicked[i], d + 1);
            }
            //if (cont + 1) % 5 != 0 {
            //    cand.push(cont + 1);
            //}
            cand.remove(0);
        }
        targets
    }
    fn make_destinations(&self) -> Vec<(usize, usize)> {
        let n = self.len();
        let targets = self.determine_target_containers();
        let mut dests = vec![];
        let mut n_kicked = vec![0; n];
        for t in &targets {
            let cs = self.search_container(*t as u32);
            match cs {
                ContainerState::Done => continue,
                ContainerState::Carrying(_) => continue,
                ContainerState::Board(x, y) => {
                    dests.push((x, y));
                },
                ContainerState::Queue(i, d) => {
                    while n_kicked[i] < d + 1 {
                        dests.push((i, 0));
                        n_kicked[i] += 1;
                    }
                }
            }
        }
        dests
    }
    fn search_free_cells(&self) -> Vec<(usize, usize)> {
        let n = self.len();
        let mut res = vec![];
        for i in 0..n {
            if i == n/2 {
                continue;
            }
            for j in (2..n - 1).rev() {
                if self.board[i][j] == -1 {
                    res.push((i, j));
                }
            }
        }
        // already emptied carry-in cell can be used as free cell
        for i in 0..n {
            if self.queue[i].is_empty() && self.board[i][0] == -1 {
                res.push((i, 0));
            }
        }
        res
    }
    // returns the distance to the destination after each move (empty vector when unreachable)
    fn bfs(&self, from: (usize, usize), to: (usize, usize), move_over: bool) -> Vec<(Move, i32)> {
        let dx = [!0, 0, 1, 0, 0]; // 5 th move is stay
        let dy = [0, !0, 0, 1, 0];
        let n = self.len();
        let (sx, sy) = from;
        let (tx, ty) = to;
        let out_of_range = |x: usize, y: usize| !(0..n).contains(&x) || !(0..n).contains(&y);
        let mut res = vec![];
        for dir in 0..5 {
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
                    let mv = if dir < 4 { Move::Move(dir) } else { Move::Stay };
                    res.push((mv, d as i32));
                    break;
                }
                for dir1 in 0..4 {
                    let nx = usize::wrapping_add(x, dx[dir1]);
                    let ny = usize::wrapping_add(y, dy[dir1]);
                    if out_of_range(nx, ny)
                        || !move_over && self.board[nx][ny] != -1
                        || dist[nx][ny] < d
                    {
                        continue;
                    }
                    queue.push_back((nx, ny, d + 1));
                    dist[nx][ny] = d + 1;
                }
            }
        }
        res
    }
    fn calc_distance(&self, from: (usize, usize), to: (usize, usize), move_over: bool) -> usize {
        if from == to {
            return 0;
        }
        let res = self.bfs(from, to, move_over);
        if res.is_empty() {
            !0
        } else {
            let d = res.iter().map(|(_, d)| d).min().unwrap();
            *d as usize
        }
    }
    fn reachable(&self, from: (usize, usize), to: (usize, usize), move_over: bool) -> bool {
        self.bfs(from, to, move_over).len() != 0
    }
    fn estimate_time_to_complete(&self, worker: usize, cont: usize) -> usize {
        let n = self.len();
        let state = self.search_container(cont as u32);
        let cur = self.get_crane_pos(worker);
        match state {
            ContainerState::Board(x, y) => {
                let dest1 = (x, y);
                let dest2 = (cont / n, n - 1);
                manhattan_distance(cur, dest1) + 1 + manhattan_distance(dest1, dest2) + 1
            },
            ContainerState::Carrying(i) => {
                assert_eq!(i, worker);
                let dest = (cont / n, n - 1);
                manhattan_distance(cur, dest) + 1
            }
            _ => !0,
        }
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
    // returns mapping of: crane id => destination
    fn match_crane_with_target(&self, n_crane: usize) -> Vec<(usize, usize)> {
        let n = self.input.n;
        let cand = self.state.next_containers_to_caryy_out().into_iter().map(|x| x as i32).collect_vec();
        let new_dest_set = self.state.make_destinations();

        let is_carry_out_target = |cont| {
            cand.contains(&cont)
        };
        // if dests[i] remains (!0, !0), there is no task for crane i in this turn
        let mut dests = vec![(!0, !0); n_crane];
        let mut busy_list = (0..n_crane)
            .into_iter()
            .filter(|i| self.state.cranes[*i].container != -1)
            .collect_vec();
        let free_cells = self.state.search_free_cells();
        let mut n_free_cells = free_cells.len();
        let mut needs_free_cell = vec![];
        for &i in &busy_list {
            let (x, y) = self.state.get_crane_pos(i);
            let cont = self.state.cranes[i].container;
            let large = self.state.cranes[i].large;
            if cont != -1 {
                // crane is holding some container
                if is_carry_out_target(cont) {
                    let dest = (cont as usize / n, n - 1);
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
                }
                else {
                    needs_free_cell.push(i);
                }
            }
        }
        for dest in &free_cells {
            if needs_free_cell.is_empty() {
                break;
            }
            let mut distances = vec![];
            for i in &needs_free_cell {
                let cur = self.state.get_crane_pos(*i);
                let large = self.state.cranes[*i].large;
                let dist = self.state.calc_distance(cur, *dest, large);
                distances.push((dist, i));
            }
            distances.sort();
            let (d, i) = distances[0];
            if d != !0 {
                dests[*i] = *dest;
                let pos = needs_free_cell.iter().position(|x| *x == *i).unwrap();
                needs_free_cell.remove(pos);
                n_free_cells -= 1;
            }
        }
        // Assign the nearest crane to each task
        for task in &new_dest_set {
            let dest1 = *task;
            let c = self.state.board[dest1.0][dest1.1];
            let mut cand = vec![];
            for i in 0..n_crane {
                if busy_list.contains(&i) {
                    continue;
                }
                if is_carry_out_target(c) || n_free_cells > 0 {
                    let cur = self.state.get_crane_pos(i);
                    let dist = manhattan_distance(cur, dest1);
                    cand.push((dist, i));
                }
            }
            if !cand.is_empty() {
                cand.sort();
                let i = cand.into_iter().next().unwrap().1;
                dests[i] = dest1;
                busy_list.push(i);
                if !is_carry_out_target(c) {
                    n_free_cells -= 1;
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
        // check inversion in last row
        let cand = self.state.next_containers_to_caryy_out();
        for i in 0..n_crane {
            let qi = next[i];
            let ci = self.state.cranes[i].container as usize;
            for j in (i + 1)..n_crane {
                let qj = next[j];
                let cj = self.state.cranes[j].container as usize;
                if !(qi.1 == n - 1 && qj.1 == n - 1 && cand.contains(&ci) && cand.contains(&cj)) {
                    continue;
                }
                let ri = ci as usize / n;
                let rj = cj as usize / n;
                if (ri < rj && qi.0 > qj.0) || (ri > rj && qi.0 < qj.0) {
                    ok = false;
                }
            }
        }
        ok
    }
    fn consider_next_move(&self, dests: &Vec<(usize, usize)>) -> Vec<Move> {
        let n = self.input.n;
        let n_crane = dests.len();
        let mut possible_moves = vec![];
        for i in 0..n_crane {
            let mut mvs = vec![];
            let dest = dests[i];
            let (x, y) = self.state.get_crane_pos(i);
            let cont = self.state.cranes[i].container;
            let large = self.state.cranes[i].large;
            if dest == (!0, !0) {
                // No task for this crane
                // prevent staying at carry out entrance
                let stay_cost = if y == n - 1 { 1 } else { 0 };
                mvs.push((Move::Stay, stay_cost));
                for dir in 0..4 {
                    let mv = Move::Move(dir);
                    if mv.next((x, y), n).is_some() {
                        mvs.push((mv, 0));
                    }
                }
            } else if dest == (x, y) {
                // current position is the destination
                if cont == -1 {
                    assert_ne!(self.state.board[x][y], -1);
                    mvs.push((Move::Pick, 0));
                } else {
                    mvs.push((Move::Release, 0));
                }
            } else {
                mvs = self
                    .state
                    .bfs((x, y), dest, large || self.state.cranes[i].container == -1);
            }
            possible_moves.push(mvs);
        }
        let mut acceptable_cands = vec![];
        for cand in possible_moves.iter().multi_cartesian_product() {
            let (cand, dists): (Vec<_>, Vec<_>) = cand.into_iter().cloned().unzip();
            if cand.iter().all(|mv| *mv == Move::Stay) {
                // no progress
                continue;
            }
            let d: i32 = dists.into_iter().sum();
            let ok = self.validate_turn_action(&cand);
            if ok {
                acceptable_cands.push((cand, d));
            }
        }
        if !acceptable_cands.is_empty() {
            // return candidate with best progress
            acceptable_cands.sort_by_key(|(_, d)| *d);
            return acceptable_cands.into_iter().next().unwrap().0;
        }
        panic!("Cannot find move candidate!");
    }
    fn assign_all_remaining_containers(&self, n_crane: usize) -> Vec<(usize, (usize, usize))> {
        let n = self.input.n;
        let container_states = self.state.list_all_container_state();
        let mut assignment = vec![(!0, (!0, !0)); n_crane];  // crane id -> (container id, dest)
        let mut free_cranes = vec![];
        for i in 0..n_crane {
            if self.state.cranes[i].container != -1 {
                // already has task
                let cont = self.state.cranes[i].container as usize;
                let dest = (cont / n, n - 1);
                assignment[i] = (cont, dest);
            } else {
                free_cranes.push(i);
            }
        }
        let mut tasks = vec![];
        for cont in 0..n*n {
            match container_states[cont] {
                ContainerState::Board(x, y) => {
                    tasks.push((cont, (x, y)));
                },
                ContainerState::Queue(i, _) => {
                    tasks.push((cont, (i, 0)));
                },
                _ => continue,
            }
        }
        for &(cont, pos) in &tasks {
            free_cranes.sort_by_key(|i| manhattan_distance(self.state.get_crane_pos(*i), pos));
            let worker = free_cranes[0];
            assignment[worker] = (cont, pos);
            free_cranes.remove(0);
        }
        assignment
    }
    fn estimate_turn_to_complete(&self, assignment: &Vec<(usize, (usize, usize))>) -> Vec<usize> {
        let n = self.input.n;
        let mut turn_to_complete = vec![!0 - 1; n*n];
        for (i, &(cont, _)) in assignment.iter().enumerate() {
            if cont == !0 {
                continue;
            }
            turn_to_complete[cont] = self.state.estimate_time_to_complete(i, cont);
        }
        turn_to_complete
    }
    fn consider_next_moves_in_last_phase(&self, assignment: &Vec<(usize, (usize, usize))>) -> Vec<Move> {
        let n = self.input.n;
        let next_containers = self.state.next_containers_to_caryy_out();
        let n_crane = assignment.len();
        let turn_to_complete = self.estimate_turn_to_complete(&assignment);
        let mut busy = vec![false; n*n];
        for cont in 0..(n*n) {
            if cont % n == 0 || next_containers.contains(&cont) {
                busy[cont] = true;
            } else if busy[cont - 1] && (turn_to_complete[cont - 1] < turn_to_complete[cont]) {
                busy[cont] = true;
            }
        }
        let mut possible_moves = vec![];
        for i in 0..n_crane {
            let mut mvs = vec![];
            let (cont, dest) = assignment[i];
            let (x, y) = self.state.get_crane_pos(i);
            if dest == (!0, !0) {
                // No task for this crane
                // prevent staying at carry out entrance
                let stay_cost = if y == n - 1 { 1 } else { 0 };
                mvs.push((Move::Stay, stay_cost));
                for dir in 0..4 {
                    let mv = Move::Move(dir);
                    if mv.next((x, y), n).is_some() {
                        mvs.push((mv, 0));
                    }
                }
            } else if dest == (x, y) {
                // current position is the destination
                if self.state.cranes[i].container == -1 {
                    assert_ne!(self.state.board[x][y], -1);
                    mvs.push((Move::Pick, 0));
                } else {
                    mvs.push((Move::Release, 0));
                }
            } else {
                let large = self.state.cranes[i].large;
                if self.state.cranes[i].container == -1 || busy[cont] {
                    mvs = self
                        .state
                        .bfs((x, y), dest, large || self.state.cranes[i].container == -1);
                } else {
                    mvs.push((Move::Stay, 0));
                    for dir in 0..4 {
                        let mv = Move::Move(dir);
                        let next = mv.next((x, y), n);
                        if next.is_some() && next.unwrap() != dest {
                            let (tx, ty) = next.unwrap();
                            if !large && self.state.cranes[i].container != -1 && self.state.board[tx][ty] != -1 {
                                continue;
                            }
                            mvs.push((mv, -1));
                        }
                    }
                }
            }
            possible_moves.push(mvs);
        }
        //dbg!(&possible_moves);
        let mut acceptable_cands = vec![];
        for cand in possible_moves.iter().multi_cartesian_product() {
            let (cand, dists): (Vec<_>, Vec<_>) = cand.into_iter().cloned().unzip();
            if cand.iter().all(|mv| *mv == Move::Stay) {
                // no progress
                continue;
            }
            let d: i32 = dists.into_iter().sum();
            let ok = self.validate_turn_action(&cand);
            if ok {
                acceptable_cands.push((cand, d));
            }
        }
        if !acceptable_cands.is_empty() {
            // return candidate with best progress
            acceptable_cands.sort_by_key(|(_, d)| *d);
            return acceptable_cands.into_iter().next().unwrap().0;
        }
        panic!("Cannot find move candidate!");
    }
    fn solve(&mut self) -> Solution {
        let n = self.input.n;
        let mut actions = vec![];

        let n_crane = 5;
        let mut turn = 0;
        let max_turn = 1000;
        let mut n_remaining_containers = n*n - self.state.done.iter().map(|v| v.len()).sum::<usize>();
        while n_remaining_containers > 0 {
            if turn >= max_turn {
                break;
            }
            let act;
            if n_remaining_containers <= n_crane {
                let assignment = self.assign_all_remaining_containers(n_crane);
                act = self.consider_next_moves_in_last_phase(&assignment);
            } else {
                let dests = self.match_crane_with_target(n_crane);
                act = self.consider_next_move(&dests);
            }
            let ext_act = extend_move(&act, n);
            eprintln!(
                "turn: {}: {}",
                turn,
                ext_act.iter().map(|mv| mv.to_char()).collect::<String>()
            );
            self.state.step(&ext_act).unwrap();
            actions.push(ext_act);
            turn += 1;
            n_remaining_containers = n*n - self.state.done.iter().map(|v| v.len()).sum::<usize>();
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
