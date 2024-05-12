use proconio::input;
use proconio::marker::Chars;

struct Node {
    to: [usize; 26],
    cnt: usize,
}

impl Node {
    fn new() -> Self {
        Node { to: [usize::MAX; 26], cnt: 0 }
    }
}

struct Trie {
    nodes: Vec<Node>,
}

fn to_idx(c: char) -> usize {
    (c as u8 - b'a') as usize
}

impl Trie {
    fn new() -> Self {
        let root = Node::new();
        Trie { nodes: vec![root] }
    }

    fn add(&mut self, s: &Vec<char>) {
        let mut v = 0;
        for c in s {
            self.nodes[v].cnt += 1;
            let sz = self.nodes.len();
            let next = &mut self.nodes[v].to[to_idx(*c)];
            if *next == usize::MAX {
                *next = sz;
                self.nodes.push(Node::new());
            }
            v = self.nodes[v].to[to_idx(*c)];
        }
        self.nodes[v].cnt += 1;
    }

    fn dfs(&self, v: usize) -> usize {
        let x = self.nodes[v].cnt;
        let mut res = if v == 0 { 0 } else { x * (x - 1) / 2 };
        for child in &self.nodes[v].to {
            if *child == usize::MAX {
                continue;
            }
            res += self.dfs(*child);
        }
        res
    }
}

fn main() {
    input! {
        n: usize,
        s: [Chars; n],
    }
    let mut trie = Trie::new();
    for s in &s {
        trie.add(s);
    }
    let ans = trie.dfs(0);
    println!("{ans}");
}
