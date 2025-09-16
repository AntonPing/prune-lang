use logos::Logos;

pub type Span = logos::Span;

#[derive(Clone, Copy, Debug, Eq, Logos, PartialEq)]
#[logos(skip r"[ \r\t\f]+")]
pub enum Token {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(":")]
    Colon,
    #[token(";")]
    Semi,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("|")]
    Bar,
    #[token("=")]
    Equal,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    #[token("==")]
    EqualEqual,
    #[token(">=")]
    GreaterEqual,
    #[token(">")]
    Greater,
    #[token("!=")]
    BangEqual,
    #[token("^")]
    Caret,
    #[token("&&")]
    DoubleAmpersand,
    #[token("||")]
    DoubleBar,
    #[token("!")]
    Bang,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("<-")]
    LeftArrow,
    #[token(":=")]
    ColonEqual,
    #[token("let")]
    Let,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("match")]
    Match,
    #[token("with")]
    With,
    #[token("case")]
    Case,
    #[token("of")]
    Of,
    #[token("as")]
    As,
    #[token("begin")]
    Begin,
    #[token("end")]
    End,
    #[token("fresh")]
    Fresh,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("datatype")]
    Datatype,
    #[token("function")]
    Function,
    #[token("predicate")]
    Predicate,
    #[token("query")]
    Query,
    #[token("where")]
    Where,
    #[token("_")]
    Wild,
    #[regex(r"-?[0-9]([0-9])*")]
    Int,
    #[regex(r"-?[0-9]([0-9])*\.[0-9]([0-9])*")]
    Float,
    #[token("true")]
    #[token("false")]
    Bool,
    #[regex(r"'(.|\\.)'")]
    Char,
    #[token("Int")]
    TyInt,
    #[token("Float")]
    TyFloat,
    #[token("Bool")]
    TyBool,
    #[token("Char")]
    TyChar,
    #[token("Unit")]
    TyUnit,
    #[regex(r"[a-z]([a-zA-Z0-9]|_)*")]
    LowerIdent,
    #[regex(r"[A-Z]([a-zA-Z0-9]|_)*")]
    UpperIdent,
    #[regex(r"@[a-zA-Z]([a-zA-Z0-9]|_)*")]
    PrimOpr,
    #[token("//", line_comment)]
    LineComment,
    #[token("/*", block_comment)]
    BlockComment,
    #[token("\n")]
    NewLine,
    /// lexer failed, skip till next whitespace
    TokError,
    EndOfFile,
}

fn line_comment(lex: &mut logos::Lexer<Token>) -> bool {
    let mut rest = lex.remainder().chars();
    loop {
        if let Some(ch) = rest.next() {
            lex.bump(ch.len_utf8());
            if ch == '\n' {
                return true;
            }
        } else {
            return false;
        }
    }
}

fn block_comment(lex: &mut logos::Lexer<Token>) -> bool {
    let mut rest = lex.remainder().chars();
    let mut last_char = ' ';
    let mut nested_level: usize = 1;
    loop {
        if let Some(ch) = rest.next() {
            lex.bump(ch.len_utf8());
            match ch {
                '/' if last_char == '*' => {
                    nested_level -= 1;
                }
                '*' if last_char == '/' => {
                    nested_level += 1;
                }
                _ => {}
            }
            if nested_level == 0 {
                return true;
            }
            last_char = ch;
        } else {
            return false;
        }
    }
}

pub struct TokenSpan {
    pub token: Token,
    pub span: Span,
}

pub fn tokenize(source: &str) -> Vec<TokenSpan> {
    let mut lex = Token::lexer(source);
    let mut vec = Vec::new();
    while let Some(tok) = lex.next() {
        let span = lex.span();
        match tok {
            // we don't leak these three tokens to parser
            // but they will be useful in the future, if we want to write a formatter
            Ok(Token::NewLine) | Ok(Token::LineComment) | Ok(Token::BlockComment) => {}
            Ok(token) => {
                vec.push(TokenSpan { token, span });
            }
            Err(()) => {
                let token = Token::TokError;
                vec.push(TokenSpan { token, span });
            }
        }
    }
    let token = Token::EndOfFile;
    let span = lex.span();
    vec.push(TokenSpan { token, span });
    vec
}

#[test]
#[ignore = "just to see result"]
fn lexer_test() {
    let s = r#"
// test line comment
/*
    /*
        test block comment
    */
*/
datatype IntList where
| Cons(Int, IntList)
| Nil
end

function append(xs: IntList, x: Int) -> Int
begin
    match xs with
    | Cons(head, tail) => Cons(head, append(tail, x))
    | Nil => Cons(x, Nil)
    end
end
"#;

    let mut lex = Token::lexer(s);

    loop {
        if let Some(tok) = lex.next() {
            println!("{:?} {:?} {}", tok, lex.span(), lex.slice());
        } else {
            break;
        }
    }
}
