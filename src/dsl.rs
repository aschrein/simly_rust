use std::borrow::BorrowMut;
use std::io;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::{any::Any, string};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum TokenType {
    NUMBER,
    LITERAL,
    STRING,
    OPERATOR,
    SPECIAL,
}

pub struct ConsoleColor;

impl ConsoleColor {
    pub const RED: &'static str = "\x1b[91m";
    pub const GREEN: &'static str = "\x1b[92m";
    pub const YELLOW: &'static str = "\x1b[93m";
    pub const BLUE: &'static str = "\x1b[94m";
    pub const PURPLE: &'static str = "\x1b[95m";
    pub const CYAN: &'static str = "\x1b[96m";
    pub const END: &'static str = "\x1b[0m";
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub line: usize,
    pub col: usize,
}

impl Token {
    pub fn new(token_type: TokenType, value: String, line: usize, col: usize) -> Self {
        Token {
            token_type,
            value,
            line,
            col,
        }
    }

    pub fn is_float(&self) -> bool {
        self.token_type == TokenType::NUMBER && self.value.contains('.')
    }

    pub fn get_float(&self) -> Option<f64> {
        self.value.parse().ok()
    }
}

pub struct TokenStream {
    tokens: Vec<Token>,
    string: String,
    lines: Vec<String>,
    pos: usize,
    lines_scan: Vec<usize>,
}

impl TokenStream {
    pub fn new(
        input: String,
        build_ast: Option<fn(&TokenStream) -> Box<dyn Any>>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let lines: Vec<String> = input.lines().map(|s| s.to_string()).collect();
        let lines_sizes: Vec<usize> = lines.iter().map(|s| s.len()).collect();
        let lines_scan: Vec<usize> = lines_sizes
            .iter()
            .scan(0, |acc, x| {
                let old = *acc;
                *acc += x;
                Some(old)
            })
            .collect();
        let mut stream = TokenStream {
            tokens: Vec::new(),
            string: input,
            lines: lines,
            pos: 0,
            lines_scan,
        };

        stream.tokenize();

        if let Some(build_ast_fn) = build_ast {
            Some(build_ast_fn(&stream));
        }

        Ok(stream)
    }

    fn tokenize(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let string = self.string.clone();
        let mut line = 0;
        let mut col = 0;
        let mut idx = 0;
        let mut cur_str = String::new();

        fn flush_token(tokens: &mut Vec<Token>, cur_str: &mut String, line: usize, col: usize) {
            if !cur_str.is_empty() {
                tokens.push(Token {
                    token_type: TokenType::LITERAL,
                    value: cur_str.clone(),
                    line,
                    col,
                });
                cur_str.clear();
            }
        }

        while idx < string.len() {
            let c = string.chars().nth(idx).unwrap();
            let nc = string.chars().nth(idx + 1);
            let nnc = string.chars().nth(idx + 2);
            let nnnc = string.chars().nth(idx + 3);

            let dop = nc
                .map(|next| format!("{}{}", c, next))
                .unwrap_or_else(|| c.to_string());
            let trip = nnc
                .map(|next_next| format!("{}{}{}", c, nc.unwrap(), next_next))
                .unwrap_or_else(|| dop.clone());

            match c {
                '\n' => {
                    flush_token(&mut self.tokens, &mut cur_str, line, col);
                    line += 1;
                    col = 0;
                }
                ' ' | '\t' => {
                    flush_token(&mut self.tokens, &mut cur_str, line, col);
                    col += 1;
                }
                '/' if nc == Some('/') => {
                    flush_token(&mut self.tokens, &mut cur_str, line, col);
                    while let Some(next_char) = string.chars().nth(idx + 1) {
                        if next_char == '\n' {
                            break;
                        }
                        idx += 1;
                    }
                    line += 1;
                    col = 0;
                }
                '"' | '\'' if nc == Some(c) && nnc == Some(c) => {
                    flush_token(&mut self.tokens, &mut cur_str, line, col);
                    let start_col = col;
                    idx += 2;
                    col += 2;
                    while let (Some(nc), Some(nnc), Some(nnnc)) = (
                        string.chars().nth(idx + 1),
                        string.chars().nth(idx + 2),
                        string.chars().nth(idx + 3),
                    ) {
                        if nc == c && nnc == c && nnnc == c {
                            break;
                        }
                        cur_str.push(nc);
                        if nc == '\n' {
                            line += 1;
                            col = 0;
                        } else {
                            col += 1;
                        }
                        idx += 1;
                    }
                    self.tokens.push(Token {
                        token_type: TokenType::STRING,
                        value: cur_str.clone(),
                        line,
                        col: start_col,
                    });
                    cur_str.clear();
                    idx += 3;
                }
                '"' | '\'' => {
                    flush_token(&mut self.tokens, &mut cur_str, line, col);
                    let start_col = col;
                    // let mut skip_next = false; // skip next character if it is preceded by a backslash
                    while let Some(next_char) = string.chars().nth(idx + 1) {
                        if next_char == c {
                            // if skip_next {
                            // skip_next = false;
                            // } else {
                            break;
                            // }
                        }
                        // else if next_char == '\\' {
                        // skip_next = true;
                        // }
                        cur_str.push(next_char);
                        if next_char == '\n' {
                            line += 1;
                            col = 0;
                        } else {
                            col += 1;
                        }
                        idx += 1;
                    }
                    self.tokens.push(Token {
                        token_type: TokenType::STRING,
                        value: cur_str.clone(),
                        line,
                        col: start_col,
                    });
                    cur_str.clear();
                    idx += 1;
                }
                _ if c.is_digit(10)
                    || (c == '.' && nc.map_or(false, |nc| nc.is_digit(10)))
                    || ((c == '+' || c == '-') && nc.map_or(false, |nc| nc.is_digit(10)))
                    || (c == '0'
                        && nc.map_or(false, |nc| {
                            nc == 'x' || nc == 'X' || nc == 'b' || nc == 'B'
                        })) =>
                {
                    if cur_str.is_empty() {
                        flush_token(&mut self.tokens, &mut cur_str, line, col);
                        let start_col = col;
                        cur_str.push(c);
                        if let Some('x' | 'X' | 'b' | 'B') = nc {
                            cur_str.push(nc.unwrap());
                            idx += 1;
                            col += 1;
                        }
                        let mut is_science = false;
                        let mut is_float = false;
                        while let Some(next_char) = string.chars().nth(idx + 1) {
                            if next_char.is_digit(10)
                                || next_char == '.'
                                || next_char == 'e'
                                || next_char == 'E'
                                || (is_science && (next_char == '+' || next_char == '-'))
                            {
                                if next_char == 'e' || next_char == 'E' {
                                    is_science = true;
                                } else if next_char == '.' && !is_science {
                                    is_float = true;
                                }
                                cur_str.push(next_char);
                                idx += 1;
                                col += 1;
                            } else {
                                break;
                            }
                        }
                        self.tokens.push(Token {
                            token_type: TokenType::NUMBER,
                            value: cur_str.to_lowercase(),
                            line,
                            col: start_col,
                        });
                        cur_str.clear();
                    } else {
                        cur_str.push(c);
                    }
                }
                _ if (cur_str.is_empty() && (dop == "or" || dop == "OR"))
                    || (cur_str.is_empty()
                        && (trip == "and" || trip == "AND" || trip == "xor" || trip == "XOR"))
                    || [
                        '+', '-', '*', '/', '%', '>', '<', '=', '!', '&', '|', '^', '~', '?', ':',
                        ';', ',', '.', '@', '#', '$', '`', '\\', '/', '(', ')', '{', '}', '[', ']',
                    ]
                    .contains(&c) =>
                {
                    flush_token(&mut self.tokens, &mut cur_str, line, col);
                    let start_col = col;
                    if [
                        "==", "!=", ">=", "<=", "&&", "||", "++", "--", "+=", "-=", "*=", "/=",
                        "%=", "<<", ">>", "<-", "->", "<=", "=>", "::", "or", "OR",
                    ]
                    .contains(&dop.as_str())
                    {
                        self.tokens.push(Token {
                            token_type: TokenType::OPERATOR,
                            value: dop,
                            line,
                            col: start_col,
                        });
                        col += 1;
                        idx += 1;
                    } else if ["and", "AND", "xor", "XOR"].contains(&trip.as_str()) {
                        self.tokens.push(Token {
                            token_type: TokenType::OPERATOR,
                            value: trip,
                            line,
                            col: start_col,
                        });
                        col += 2;
                        idx += 2;
                    } else if [
                        '+', '-', '*', '/', '%', '>', '<', '=', '!', '&', '|', '^', '~', '?', ':',
                        ';', ',', '.', '@', '#', '$',
                    ]
                    .contains(&c)
                    {
                        self.tokens.push(Token {
                            token_type: TokenType::OPERATOR,
                            value: c.to_string(),
                            line,
                            col: start_col,
                        });
                    } else {
                        self.tokens.push(Token {
                            token_type: TokenType::SPECIAL,
                            value: c.to_string(),
                            line,
                            col: start_col,
                        });
                    }
                }
                _ => {
                    if cur_str.is_empty() {
                        col -= 1;
                    }
                    cur_str.push(c);
                }
            }
            idx += 1;
            col += 1;
        }

        flush_token(&mut self.tokens, &mut cur_str, line, col);
        self.pos = 0;
        Ok(())
    }

    pub fn get_line(&self, line: usize) -> Option<&str> {
        self.lines.get(line).map(|s| s.as_str())
    }

    pub fn peek(&self) -> Option<Token> {
        self.tokens.get(self.pos).cloned()
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.pos < self.tokens.len() {
            let token = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(token.clone())
        } else {
            None
        }
    }

    pub fn consume(&mut self, string: &str) -> bool {
        if let Some(token) = self.peek() {
            if token.value == string {
                self.move_forward();
                return true;
            }
        }
        false
    }

    pub fn get_list_until(&mut self, strings: &[&str], consume: bool) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(n) = self.peek() {
            if strings.contains(&n.value.as_str()) {
                if consume {
                    self.move_forward();
                }
                break;
            }
            tokens.push(self.next().unwrap());
        }
        tokens
    }

    pub fn unwrap_parentheses(&mut self) -> Result<Vec<Token>, Box<dyn std::error::Error>> {
        if !self.consume("(") {
            return Err("Expected '(' but got something else".into());
        }
        let mut tokens = Vec::new();
        while !self.consume(")") {
            if let Some(token) = self.next() {
                tokens.push(token);
            } else {
                return Err("Unexpected end of tokens while parsing parentheses".into());
            }
        }
        Ok(tokens)
    }

    pub fn print_error_at_current(&self, message: &str) {
        let token = &self.tokens[self.pos.min(self.tokens.len() - 1)];
        eprintln!("**************************************************");
        eprintln!(
            "Error at line {}, col {}: {}",
            token.line, token.col, message
        );
        // if let Some(file_path) = &self.file_path {
        //     eprintln!("{}:{}", file_path.display(), token.line + 1);
        // }
        if let Some(line) = self.get_line(token.line) {
            eprintln!("{}", line);
            eprintln!("{}^", "-".repeat(token.col));
        }
        eprintln!("**************************************************");
    }

    pub fn has_more_tokens(&self) -> bool {
        self.pos < self.tokens.len()
    }

    pub fn eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    pub fn move_back(&mut self) {
        if self.pos > 0 {
            self.pos -= 1;
        }
    }

    pub fn move_forward(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    pub fn emit_debug_position(&self) {
        if let Some(token) = self.peek() {
            println!("********************************");
            if let Some(line) = self.get_line(token.line) {
                println!("{}", line);
                println!("{}^", "-".repeat(token.col));
            }
        }
    }
}

pub enum TokenStreamInput {
    Tokens(Vec<Token>),
    String(String),
}

impl From<Vec<Token>> for TokenStreamInput {
    fn from(tokens: Vec<Token>) -> Self {
        TokenStreamInput::Tokens(tokens)
    }
}

impl From<String> for TokenStreamInput {
    fn from(string: String) -> Self {
        TokenStreamInput::String(string)
    }
}

use std::rc::Rc;
use std::{fmt, fs};

#[derive(Debug, Clone, PartialEq)]
pub enum ExprType {
    LITERAL,
    UNARY,
    BINARY,
    PARANTHESIS,
    CALL,
}

use std::cell::RefCell;

#[derive(Clone)]
pub struct Expr {
    id: usize,
    expr_type: ExprType,
    token: Token,
    args: Vec<Rc<RefCell<Expr>>>,
}

static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

impl Expr {
    pub fn new(expr_type: ExprType, token: Token, args: Vec<Rc<RefCell<Expr>>>) -> Self {
        let id = NEXT_ID.fetch_add(1, Ordering::SeqCst);

        Expr {
            id,
            expr_type,
            token,
            args,
        }
    }

    pub fn value(&self) -> &str {
        &self.token.value
    }

    pub fn left(&self) -> Option<&Rc<RefCell<Expr>>> {
        self.args.get(0)
    }

    pub fn right(&self) -> Option<&Rc<RefCell<Expr>>> {
        self.args.get(1)
    }

    // find left most leaf node with priority less than or equal to a given priority for balancing
    pub fn find_left_most_leaf(&self, priority: i32) -> Option<Rc<RefCell<Expr>>> {
        if self.expr_type == ExprType::BINARY && get_operator_priority(self.value()) <= priority {
            if let Some(left) = self.left() {
                let left_leaf = left.borrow().find_left_most_leaf(priority);
                if left_leaf.is_some() {
                    return left_leaf;
                }
            }
            // If no leaf found in left subtree, return self
            return Some(Rc::new(RefCell::new(self.clone())));
        }
        None
    }

    pub fn to_str(&self, indent: usize) -> String {
        let mut s = String::new();

        // Add indentation
        s.push_str(&" ".repeat(indent));

        // Add the current node's information
        s.push_str(&format!("{:?} ", self.expr_type));
        s.push_str(self.value());
        s.push('\n');

        // Add left child if it exists
        if let Some(left) = self.left() {
            s.push_str(&left.borrow().to_str(indent + 2));
        }

        // Add right child if it exists
        if let Some(right) = self.right() {
            s.push_str(&right.borrow().to_str(indent + 2));
        }

        s
    }

    // Write dot graph format for this expression
    pub fn _to_dot(&self) -> String {
        let mut s: String = String::new();
        s = s + "subgraph {\n";
        s = s + format!("node{} [label=\"{}\"];\n", self.id, self.value()).as_str();
        for arg in &self.args {
            s = s + format!("node{} -> node{};\n", self.id, arg.borrow().id).as_str();
            s = s + arg.borrow()._to_dot().as_str();
        }
        s = s + "}\n";
        s
    }
    pub fn to_dot(&self) -> String {
        let mut s: String = String::new();
        s = s + "digraph {\n";
        s = s + self._to_dot().as_str();
        s = s + "}\n";
        s
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str(0))
    }
}

use lazy_static::lazy_static;
use std::collections::HashMap;

// make a expression priority table for operators
lazy_static! {
    static ref OPERATOR_PRIORITY: HashMap<&'static str, i32> = {
        let mut priority = HashMap::new();
        priority.insert("||", 1);
        priority.insert("&&", 2);
        priority.insert("==", 3);
        priority.insert("!=", 3);
        priority.insert("<", 4);
        priority.insert(">", 4);
        priority.insert("<=", 4);
        priority.insert(">=", 4);
        priority.insert("+", 5);
        priority.insert("-", 5);
        priority.insert("*", 6);
        priority.insert("/", 6);
        priority.insert("%", 6);
        priority
    };
}

fn get_operator_priority(op: &str) -> i32 {
    *OPERATOR_PRIORITY.get(op).unwrap_or(&0)
}

use std::cell::RefCell;
use std::rc::Rc;

pub fn recursive_parse_expression(tk: &mut TokenStream) -> Option<Rc<RefCell<Expr>>> {
    let c = tk.next()?;

    if c.value == "(" {
        let inner_expr = recursive_parse_expression(tk)?;
        let p = Rc::new(RefCell::new(Expr::new(
            ExprType::PARANTHESIS,
            c.clone(),
            vec![inner_expr],
        )));
        assert!(tk.consume(")"), "Expected ')' but got {:?}", tk.peek());
        return Some(p);
    }

    if let Some(n) = tk.peek() {
        if n.value == "(" {
            tk.next();
            let inner_expr = recursive_parse_expression(tk)?;
            let p = Rc::new(RefCell::new(Expr::new(
                ExprType::CALL,
                c.clone(),
                vec![inner_expr],
            )));
            assert!(tk.consume(")"), "Expected ')' but got {:?}", tk.peek());
            return Some(p);
        } else if n.token_type == TokenType::OPERATOR {
            tk.next();
            let left = Rc::new(RefCell::new(Expr::new(
                ExprType::LITERAL,
                c.clone(),
                vec![],
            )));
            let mut right = recursive_parse_expression(tk)?;

            // Balance the expression tree
            {
                let right_borrow = right.borrow();
                if right_borrow.expr_type == ExprType::BINARY {
                    let right_priority = get_operator_priority(right_borrow.value());
                    let this_priority = get_operator_priority(&n.value);
                    if right_priority < this_priority {
                        // We need to rotate, but first we need to drop the borrow
                        drop(right_borrow);

                        // Now we can mutate
                        let mut right_mut = right.borrow_mut();
                        let right_left = right_mut.args[0].clone();
                        right_mut.args[0] = left.clone();

                        // We need to drop the mutable borrow before reassigning 'right'
                        drop(right_mut);

                        return Some(Rc::new(RefCell::new(Expr::new(
                            ExprType::BINARY,
                            n.clone(),
                            vec![right, right_left],
                        ))));
                    }
                }
            } // right_borrow goes out of scope here

            return Some(Rc::new(RefCell::new(Expr::new(
                ExprType::BINARY,
                n.clone(),
                vec![left, right],
            ))));
        }
    }

    Some(Rc::new(RefCell::new(Expr::new(
        ExprType::LITERAL,
        c.clone(),
        vec![],
    ))))
}

pub fn get_or_create_tmp_folder() -> io::Result<PathBuf> {
    let current_dir = std::env::current_dir()?;
    let tmp_dir = current_dir.join(".tmp");

    if !tmp_dir.exists() {
        fs::create_dir(&tmp_dir)?;
    }

    Ok(tmp_dir)
}

use std::io::Write;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expr() {
        let input = "1 + 2 * 3 - 4 / 5".to_string();
        let mut stream = TokenStream::new(input, None).unwrap();
        let expr = recursive_parse_expression(&mut stream).unwrap();
        println!("{:?}", expr);

        let tmp_file = get_or_create_tmp_folder().unwrap().join("test.dot");
        let mut file = fs::File::create(&tmp_file).unwrap();
        file.write_all(expr.to_dot().as_bytes()).unwrap();
    }

    #[test]
    fn test_tokenize() {
        let input = "1 + 2 * 3 - 4 / 5".to_string();
        let mut stream = TokenStream::new(input, None).unwrap();
        let tokens = stream.tokens;
        assert_eq!(tokens.len(), 9);
        assert_eq!(tokens[0].value, "1");
        assert_eq!(tokens[1].value, "+");
        assert_eq!(tokens[2].value, "2");
        assert_eq!(tokens[3].value, "*");
        assert_eq!(tokens[4].value, "3");
        assert_eq!(tokens[5].value, "-");
        assert_eq!(tokens[6].value, "4");
        assert_eq!(tokens[7].value, "/");
        assert_eq!(tokens[8].value, "5");
    }

    #[test]
    fn test_tokenize_string() {
        let input = r#"
        
        "Hello World" + 'Hello World' + "Hello World" + 'Hello World'
        
        
        "#
        .to_string();
        let mut stream = TokenStream::new(input, None).unwrap();
        let tokens = stream.tokens;
        // println!("{:?}", tokens);
        // for token in &tokens {
        // println!("{:?}", token);
        // }
        assert_eq!(tokens.len(), 7);
        assert_eq!(tokens[0].value, "Hello World");
        assert_eq!(tokens[1].value, "+");
        assert_eq!(tokens[2].value, "Hello World");
        assert_eq!(tokens[3].value, "+");
        assert_eq!(tokens[4].value, "Hello World");
        assert_eq!(tokens[5].value, "+");
        assert_eq!(tokens[6].value, "Hello World");
    }

    #[test]
    fn test_tokenize_string_with_escape() {
        let input = r#"
        'Hello "World"'
        
        "#
        .to_string();
        let mut stream = TokenStream::new(input, None).unwrap();
        let tokens = stream.tokens;
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].value, "Hello \"World\"");
    }

    #[test]
    fn test_tokenize_string_with_escape_single_quote() {
        let input = r#"
        "Hello 'World'"
        "#
        .to_string();
        let mut stream = TokenStream::new(input, None).unwrap();
        let tokens = stream.tokens;
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].value, "Hello 'World'");
    }
}
