#[derive(Eq, PartialEq, Debug)]
enum Token {
    LParen,
    RParen,
    Comma,
    Number(i32),
}

fn tokenise(s: &str) -> Vec<Token> {
    let chars = s.chars().collect::<Vec<char>>();

    let mut pos = 0;
    let mut tokens = vec![];

    while pos < chars.len() {
        let c = chars[pos];

        match c {
            '[' => {
                tokens.push(Token::LParen);
                pos += 1
            }
            ']' => {
                tokens.push(Token::RParen);
                pos += 1
            }
            ',' => {
                tokens.push(Token::Comma);
                pos += 1
            }
            ' ' => pos += 1,
            c => {
                let mut num_str = String::new();
                if c.is_digit(10) {
                    let mut c = c;

                    while c.is_digit(10) {
                        num_str.push(c);
                        pos += 1;
                        c = chars[pos];
                    }

                    tokens.push(Token::Number(num_str.parse().unwrap()))
                } else {
                    unreachable!()
                }
            }
        }
    }

    tokens
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Signal {
    List(Vec<Signal>),
    Number(i32),
}

fn parse_list(tokens: &[Token]) -> (Signal, usize) {
    let mut items = vec![];

    let mut pos = 0;
    while tokens[pos] != Token::RParen {
        let (s, size) = parse(&tokens[pos..]);
        items.push(s);
        pos += size;

        if tokens[pos] == Token::Comma {
            pos += 1;
        }
    }
    pos += 1;

    (Signal::List(items), pos)
}

fn parse(tokens: &[Token]) -> (Signal, usize) {
    match tokens[0] {
        Token::LParen => {
            let (s, pos) = parse_list(&tokens[1..]);
            (s, pos + 1)
        }
        Token::Number(num) => (Signal::Number(num), 1),
        _ => unreachable!(),
    }
}

fn parse_packet_string(s: &str) -> Signal {
    let tokens = tokenise(&s);
    let (signal, _) = parse(&tokens);
    signal
}

#[derive(Debug)]
pub enum Order {
    Ordered,
    OutOfOrder,
}

fn check_list_order(l1: Vec<Signal>, l2: Vec<Signal>) -> Option<Order> {
    for i in 0..l1.len().max(l2.len()) {
        match (l1.get(i), l2.get(i)) {
            (Some(l), Some(r)) => {
                if let Some(ord) = check_packet_order(l.clone(), r.clone()) {
                    return Some(ord);
                }
            }
            (None, Some(_)) => return Some(Order::Ordered),
            (Some(_), None) => return Some(Order::OutOfOrder),
            _ => (),
        }
    }

    None
}

fn check_packet_order(s1: Signal, s2: Signal) -> Option<Order> {
    match (s1, s2) {
        (Signal::Number(n1), Signal::Number(n2)) => {
            if n1 < n2 {
                Some(Order::Ordered)
            } else if n1 > n2 {
                Some(Order::OutOfOrder)
            } else {
                None
            }
        }
        (Signal::List(l1), Signal::List(l2)) => check_list_order(l1, l2),
        (n1, l2 @ Signal::List(_)) => check_packet_order(Signal::List(vec![n1]), l2),
        (l1 @ Signal::List(_), n2) => check_packet_order(l1, Signal::List(vec![n2])),
    }
}

fn part1(s: &str) {
    let signals = s
        .lines()
        .filter(|l| l.len() > 0)
        .map(parse_packet_string)
        .collect::<Vec<Signal>>();

    let mut ord_sum = 0;

    for i in 0..signals.len() / 2 {
        let order = check_packet_order(signals[2 * i].clone(), signals[2 * i + 1].clone());

        if let Some(Order::Ordered) = order {
            ord_sum += i + 1;
        }
    }

    println!("Sum of pair indices = {ord_sum}");
}

fn part2(s: &str) {
    let mut signals = s
        .lines()
        .filter(|l| l.len() > 0)
        .map(parse_packet_string)
        .collect::<Vec<Signal>>();


    let div1 = Signal::List(vec!(Signal::List(vec!(Signal::Number(2)))));
    let div2 = Signal::List(vec!(Signal::List(vec!(Signal::Number(6)))));
    signals.push(div1.clone());
    signals.push(div2.clone());

    let mut div1idx = signals.len()-2;
    let mut div2idx = signals.len()-1;

    for i in 0..signals.len() {
        for j in 1..signals.len()-i {
            if let Some(Order::OutOfOrder) = check_packet_order(signals[j-1].clone(), signals[j].clone()) {
                let tmp = signals[j].clone();
                signals[j] = signals[j-1].clone();
                signals[j-1] = tmp;

                if signals[j] == div1 {
                    div1idx += 1;
                } else if signals[j - 1] == div1 {
                    div1idx -= 1;
                }

                if signals[j] == div2 {
                    div2idx += 1;
                } else if signals[j - 1] == div2 {
                    div2idx -= 1;
                }
            }
        }
    }


    println!("divider 1 at {}", div1idx + 1);
    println!("divider 2 at {}", div2idx + 1);

    println!("decoder key = {}", (div1idx + 1) * (div2idx + 1));

}

fn main() {
    let filename = std::env::args().nth(1).unwrap();
    let s = std::fs::read_to_string(filename).unwrap();

    println!("-- Part 1 --");
    part1(&s);
    println!("-- Part 2 --");
    part2(&s);
}
