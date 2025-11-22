use super::lexer::*;
use super::*;
use crate::cli::diagnostic::Diagnostic;
use crate::syntax::ast::*;

pub struct Parser<'src> {
    source: &'src str,
    tokens: Vec<TokenSpan>,
    cursor: usize,
    errors: Vec<ParseError>,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    LexerError(Span),
    UnknownPrim(Span),
    FailedToMatch(Token, Token, Span),
    FailedToParse(&'static str, Token, Span),
}

impl From<ParseError> for Diagnostic {
    fn from(val: ParseError) -> Self {
        match val {
            ParseError::LexerError(span) => Diagnostic::error("cannot scan next token!")
                .line_span(span.clone(), "here is the bad token"),

            ParseError::UnknownPrim(span) => Diagnostic::error("unknown primitve!")
                .line_span(span.clone(), "here is the primitive"),
            ParseError::FailedToMatch(expect, found, span) => {
                Diagnostic::error("cannot match token!").line_span(
                    span.clone(),
                    format!("expect token {expect:?}, found token {found:?}"),
                )
            }
            ParseError::FailedToParse(name, found, span) => {
                Diagnostic::error(format!("cannot parse {name}!"))
                    .line_span(span.clone(), format!("found token {found:?}"))
            }
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Parser<'src> {
        let tokens = lexer::tokenize(src);
        Parser {
            source: src,
            tokens,
            cursor: 0,
            errors: Vec::new(),
        }
    }

    #[allow(dead_code)]
    fn peek_token_span(&self) -> &TokenSpan {
        &self.tokens[self.cursor]
    }

    fn peek_token(&self) -> Token {
        self.tokens[self.cursor].token
    }

    #[allow(dead_code)]
    fn peek_token_nth(&self, n: usize) -> Token {
        if self.cursor + n < self.tokens.len() {
            self.tokens[self.cursor + n].token
        } else {
            Token::EndOfFile
        }
    }

    fn peek_span(&self) -> &Span {
        &self.tokens[self.cursor].span
    }

    #[allow(dead_code)]
    fn peek_span_nth(&self, n: usize) -> &Span {
        if self.cursor + n < self.tokens.len() {
            &self.tokens[self.cursor + n].span
        } else {
            &self.tokens[self.tokens.len() - 1].span
        }
    }

    fn peek_slice(&self) -> &'src str {
        let span = &self.tokens[self.cursor].span;
        &self.source[span.start..span.end]
    }

    fn start_pos(&self) -> usize {
        self.tokens[self.cursor].span.start
    }

    fn end_pos(&self) -> usize {
        self.tokens[self.cursor - 1].span.end
    }

    fn next_token(&mut self) -> ParseResult<&TokenSpan> {
        let tok = &self.tokens[self.cursor];
        if tok.token == Token::TokError {
            Err(ParseError::LexerError(self.peek_span().clone()))
        } else {
            if self.cursor < self.tokens.len() - 1 {
                self.cursor += 1;
            }
            Ok(tok)
        }
    }

    fn match_token(&mut self, tok: Token) -> ParseResult<()> {
        if self.peek_token() == tok {
            self.next_token()?;
            Ok(())
        } else {
            Err(ParseError::FailedToMatch(
                tok,
                self.peek_token(),
                self.peek_span().clone(),
            ))
        }
    }

    #[allow(dead_code)]
    fn option<T, F>(&mut self, func: F) -> ParseResult<Option<T>>
    where
        F: Fn(&mut Parser) -> ParseResult<T>,
    {
        let last = self.cursor;
        match func(self) {
            Ok(res) => Ok(Some(res)),
            Err(err) => {
                // if it failed without consuming any token
                if self.cursor == last {
                    Ok(None) // return Err(ParseError::FailedToParse((), self.peek_token(), self.peek_span().clone()))
                } else {
                    Err(err) // otherwise fail
                }
            }
        }
    }

    #[allow(dead_code)]
    fn surround<T, F>(&mut self, left: Token, right: Token, func: F) -> ParseResult<T>
    where
        F: Fn(&mut Parser) -> ParseResult<T>,
    {
        self.match_token(left)?;
        let res = func(self)?;
        self.match_token(right)?;
        Ok(res)
    }

    fn delimited_list<T, F>(
        &mut self,
        left: Token,
        delim: Token,
        right: Token,
        func: F,
    ) -> ParseResult<Vec<T>>
    where
        F: Fn(&mut Parser) -> ParseResult<T>,
    {
        let mut vec: Vec<T> = Vec::new();
        self.match_token(left)?;
        // allow leading delimiter
        if self.peek_token() == delim {
            self.next_token()?;
        }
        // allow empty list
        if self.peek_token() == right {
            self.next_token()?;
            return Ok(vec);
        }
        vec.push(func(self)?);
        while self.peek_token() == delim {
            self.next_token()?;
            // allow trailing delimiter
            if self.peek_token() == right {
                break;
            }
            vec.push(func(self)?);
        }
        self.match_token(right)?;
        Ok(vec)
    }

    fn parse_lit_val(&mut self) -> ParseResult<LitVal> {
        match self.peek_token() {
            Token::Int => {
                let x = self.peek_slice().parse::<i64>().unwrap();
                self.next_token()?;
                Ok(LitVal::Int(x))
            }
            Token::Float => {
                let x = self.peek_slice().parse::<f64>().unwrap();
                self.next_token()?;
                Ok(LitVal::Float(x))
            }
            Token::Bool => {
                let x = self.peek_slice().parse::<bool>().unwrap();
                self.next_token()?;
                Ok(LitVal::Bool(x))
            }
            Token::Char => {
                let x = self.peek_slice().trim_matches('\'');
                // transform from 'c' to "c"
                let x: String = "\"".chars().chain(x.chars()).chain("\"".chars()).collect();
                if let Ok(s) = snailquote::unescape(&x) {
                    assert_eq!(s.len(), 1);
                    self.next_token()?;
                    Ok(LitVal::Char(s.chars().nth(0).unwrap()))
                } else {
                    Err(ParseError::LexerError(self.peek_span().clone()))
                }
            }
            _tok => Err(ParseError::FailedToParse(
                "literal value",
                self.peek_token(),
                self.peek_span().clone(),
            )),
        }
    }

    fn parse_lit_typ(&mut self) -> ParseResult<LitType> {
        match self.peek_token() {
            Token::TyInt => {
                self.next_token()?;
                Ok(LitType::TyInt)
            }
            Token::TyFloat => {
                self.next_token()?;
                Ok(LitType::TyFloat)
            }
            Token::TyBool => {
                self.next_token()?;
                Ok(LitType::TyBool)
            }
            Token::TyChar => {
                self.next_token()?;
                Ok(LitType::TyChar)
            }
            _tok => Err(ParseError::FailedToParse(
                "literal type",
                self.peek_token(),
                self.peek_span().clone(),
            )),
        }
    }

    fn parse_lower_var(&mut self) -> ParseResult<Var> {
        match self.peek_token() {
            Token::LowerIdent => {
                // handle wildcard identifier
                let ident = if self.peek_slice() == "_" {
                    Ident::fresh(&"_")
                } else {
                    Ident::dummy(&self.peek_slice())
                };
                let span = self.peek_span().clone();
                self.next_token()?;
                Ok(Var { ident, span })
            }
            _tok => Err(ParseError::FailedToParse(
                "lowercase varible",
                self.peek_token(),
                self.peek_span().clone(),
            )),
        }
    }

    fn parse_upper_var(&mut self) -> ParseResult<Var> {
        match self.peek_token() {
            Token::UpperIdent => {
                let ident = Ident::dummy(&self.peek_slice());
                let span = self.peek_span().clone();
                self.next_token()?;
                Ok(Var { ident, span })
            }
            _tok => Err(ParseError::FailedToParse(
                "uppercase varible",
                self.peek_token(),
                self.peek_span().clone(),
            )),
        }
    }

    fn parse_prim_opr(&mut self) -> ParseResult<Prim> {
        match self.peek_token() {
            Token::PrimOpr => {
                let s = self.peek_slice();
                self.next_token()?;
                let res = match s {
                    "@iadd" => Prim::IAdd,
                    "@isub" => Prim::ISub,
                    "@imul" => Prim::IMul,
                    "@idiv" => Prim::IDiv,
                    "@irem" => Prim::IRem,
                    "@ineg" => Prim::INeg,
                    "@icmplt" => Prim::ICmp(Compare::Lt),
                    "@icmple" => Prim::ICmp(Compare::Le),
                    "@icmpeq" => Prim::ICmp(Compare::Eq),
                    "@icmpgt" => Prim::ICmp(Compare::Gt),
                    "@icmpge" => Prim::ICmp(Compare::Ge),
                    "@icmpne" => Prim::ICmp(Compare::Ne),
                    "@band" => Prim::BAnd,
                    "@bor" => Prim::BOr,
                    "@bnot" => Prim::BNot,
                    _ => {
                        return Err(ParseError::UnknownPrim(self.peek_span().clone()));
                    }
                };
                Ok(res)
            }
            _tok => Err(ParseError::FailedToParse(
                "primitive operator",
                self.peek_token(),
                self.peek_span().clone(),
            )),
        }
    }

    fn parse_expr_args(&mut self) -> ParseResult<Vec<Expr>> {
        self.delimited_list(Token::LParen, Token::Comma, Token::RParen, |par| {
            par.parse_expr()
        })
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        let mut expr_stack: Vec<(Expr, Span)> = Vec::new();
        let mut opr_stack: Vec<Prim> = Vec::new();

        fn squash(expr_stack: &mut Vec<(Expr, Span)>, opr_stack: &mut Vec<Prim>) {
            let (rhs, span1) = expr_stack.pop().unwrap();
            let opr = opr_stack.pop().unwrap();
            let (lhs, span2) = expr_stack.pop().unwrap();
            let span = Span {
                start: span1.start,
                end: span2.end,
            };
            let new_expr = Expr::Prim {
                prim: opr,
                args: vec![lhs, rhs],
                span: span.clone(),
            };
            expr_stack.push((new_expr, span));
        }

        loop {
            let start = self.start_pos();
            let expr = self.parse_expr_factor()?;
            let end = self.end_pos();
            let span = Span { start, end };
            expr_stack.push((expr, span));
            // todo: ad-hoc polymorphism
            let opr = match self.peek_token() {
                Token::Plus => Prim::IAdd,
                Token::Minus => Prim::ISub,
                Token::Star => Prim::IMul,
                Token::Slash => Prim::IDiv,
                Token::Percent => Prim::IRem,
                Token::Less => Prim::ICmp(Compare::Lt),
                Token::LessEqual => Prim::ICmp(Compare::Le),
                Token::EqualEqual => Prim::ICmp(Compare::Eq),
                Token::GreaterEqual => Prim::ICmp(Compare::Ge),
                Token::Greater => Prim::ICmp(Compare::Gt),
                Token::BangEqual => Prim::ICmp(Compare::Ne),
                Token::DoubleAmpersand => Prim::BAnd,
                Token::DoubleBar => Prim::BOr,
                _ => {
                    while !opr_stack.is_empty() {
                        squash(&mut expr_stack, &mut opr_stack);
                    }
                    assert_eq!(expr_stack.len(), 1);
                    return Ok(expr_stack.pop().unwrap().0);
                }
            };
            self.next_token()?;

            while !opr_stack.is_empty() {
                let opr2 = opr_stack.last().unwrap();
                if opr2.get_prior() > opr.get_prior() {
                    squash(&mut expr_stack, &mut opr_stack);
                } else {
                    break;
                }
            }
            opr_stack.push(opr);
        }
    }

    fn parse_expr_factor(&mut self) -> ParseResult<Expr> {
        let start = self.start_pos();
        match self.peek_token() {
            Token::Int | Token::Float | Token::Bool | Token::Char => {
                let lit = self.parse_lit_val()?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Expr::Lit { lit, span })
            }
            Token::LowerIdent => {
                let var = self.parse_lower_var()?;
                if let Token::LParen = self.peek_token() {
                    let args = self.parse_expr_args()?;
                    let end = self.end_pos();
                    let span = Span { start, end };
                    Ok(Expr::App {
                        func: var,
                        args,
                        span,
                    })
                } else {
                    let end = self.end_pos();
                    let span = Span { start, end };
                    Ok(Expr::Var { var, span })
                }
            }
            Token::UpperIdent => {
                let cons = self.parse_upper_var()?;
                let flds = if let Token::LParen = self.peek_token() {
                    self.delimited_list(Token::LParen, Token::Comma, Token::RParen, |par| {
                        par.parse_expr()
                    })?
                } else {
                    Vec::new()
                };
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Expr::Cons { cons, flds, span })
            }
            Token::PrimOpr => {
                let prim = self.parse_prim_opr()?;
                let args = self.parse_expr_args()?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Expr::Prim { prim, args, span })
            }
            Token::Let => {
                self.match_token(Token::Let)?;
                let bind = self.parse_pattern()?;
                self.match_token(Token::Equal)?;
                let expr = Box::new(self.parse_expr()?);
                self.match_token(Token::Semi)?;
                let cont = Box::new(self.parse_expr()?);
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Expr::Let {
                    patn: bind,
                    expr,
                    cont,
                    span,
                })
            }
            Token::If => {
                self.match_token(Token::If)?;
                let cond = Box::new(self.parse_expr()?);
                self.match_token(Token::Then)?;
                let then = Box::new(self.parse_expr()?);
                self.match_token(Token::Else)?;
                let els = Box::new(self.parse_expr()?);
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Expr::Ifte {
                    cond,
                    then,
                    els,
                    span,
                })
            }
            Token::Match => {
                self.match_token(Token::Match)?;
                let expr = Box::new(self.parse_expr()?);
                let brchs = self.delimited_list(Token::With, Token::Bar, Token::End, |par| {
                    par.parse_match_brch()
                })?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Expr::Match { expr, brchs, span })
            }
            Token::Condition => {
                let brchs =
                    self.delimited_list(Token::Condition, Token::Bar, Token::End, |par| {
                        par.parse_cond_brch()
                    })?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Expr::Cond { brchs, span })
            }
            Token::Alternative => {
                let brchs =
                    self.delimited_list(Token::Alternative, Token::Bar, Token::End, |par| {
                        par.parse_expr()
                    })?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Expr::Alter { brchs, span })
            }
            Token::Fresh => {
                let vars = self.delimited_list(Token::Fresh, Token::Comma, Token::Semi, |par| {
                    par.parse_lower_var()
                })?;
                let cont = Box::new(self.parse_expr()?);
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Expr::Fresh { vars, cont, span })
            }
            Token::Guard => {
                self.match_token(Token::Guard)?;
                let lhs = Box::new(self.parse_expr()?);

                let rhs = match self.peek_token() {
                    Token::Equal => {
                        // guard e1 = e2; e3, in which e1 and e2 have the same type
                        self.match_token(Token::Equal)?;
                        Some(Box::new(self.parse_expr()?))
                    }
                    Token::Semi => {
                        // guard e1; e2, if e1 has Bool type
                        None
                    }
                    _ => {
                        return Err(ParseError::FailedToParse(
                            "right-hand-side of guard expression",
                            self.peek_token(),
                            self.peek_span().clone(),
                        ));
                    }
                };

                self.match_token(Token::Semi)?;
                let cont = Box::new(self.parse_expr()?);
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Expr::Guard {
                    lhs,
                    rhs,
                    cont,
                    span,
                })
            }
            Token::Bang => {
                self.match_token(Token::Bang)?;
                let arg = self.parse_expr_factor()?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Expr::Prim {
                    prim: Prim::BNot,
                    args: vec![arg],
                    span,
                })
            }
            Token::Undefined => {
                self.match_token(Token::Undefined)?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Expr::Undefined { span })
            }
            Token::LParen => {
                let exprs =
                    self.delimited_list(Token::LParen, Token::Comma, Token::RParen, |par| {
                        par.parse_expr()
                    })?;
                let end = self.end_pos();
                let span = Span { start, end };
                match exprs.len() {
                    0 => Err(ParseError::FailedToParse(
                        "tuple of expressions",
                        self.peek_token(),
                        self.peek_span().clone(),
                    )),
                    1 => Ok(exprs.into_iter().next().unwrap()),
                    _ => Ok(Expr::Tuple { flds: exprs, span }),
                }
            }
            Token::Unit => {
                self.next_token()?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Expr::Tuple {
                    flds: Vec::new(),
                    span,
                })
            }
            Token::End | Token::Bar | Token::Else => {
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Expr::Tuple {
                    flds: Vec::new(),
                    span,
                })
            }
            _tok => Err(ParseError::FailedToParse(
                "expression",
                self.peek_token(),
                self.peek_span().clone(),
            )),
        }
    }

    fn parse_match_brch(&mut self) -> ParseResult<(Pattern, Expr)> {
        let start = self.start_pos();
        let patn = self.parse_pattern()?;
        self.match_token(Token::FatArrow)?;
        let body = self.parse_expr()?;
        let end = self.end_pos();
        let _span = Span { start, end };
        Ok((patn, body))
    }

    fn parse_cond_brch(&mut self) -> ParseResult<(Expr, Expr)> {
        let start = self.start_pos();
        let cond = self.parse_expr()?;
        self.match_token(Token::FatArrow)?;
        let body = self.parse_expr()?;
        let end = self.end_pos();
        let _span = Span { start, end };
        Ok((cond, body))
    }

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        let start = self.start_pos();
        match self.peek_token() {
            Token::Int | Token::Float | Token::Bool | Token::Char | Token::Unit => {
                let lit = self.parse_lit_val()?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Pattern::Lit { lit, span })
            }
            Token::LowerIdent => {
                let var = self.parse_lower_var()?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Pattern::Var { var, span })
            }
            Token::UpperIdent => {
                let cons = self.parse_upper_var()?;
                let flds = if let Token::LParen = self.peek_token() {
                    self.delimited_list(Token::LParen, Token::Comma, Token::RParen, |par| {
                        par.parse_pattern()
                    })?
                } else {
                    Vec::new()
                };
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Pattern::Cons { cons, flds, span })
            }
            Token::LParen => {
                let patns =
                    self.delimited_list(Token::LParen, Token::Comma, Token::RParen, |par| {
                        par.parse_pattern()
                    })?;
                let end = self.end_pos();
                let span = Span { start, end };
                match patns.len() {
                    0 => Err(ParseError::FailedToParse(
                        "tuple of patterns",
                        self.peek_token(),
                        self.peek_span().clone(),
                    )),
                    1 => Ok(patns.into_iter().next().unwrap()),
                    _ => Ok(Pattern::Tuple { flds: patns, span }),
                }
            }
            _tok => Err(ParseError::FailedToParse(
                "pattern",
                self.peek_token(),
                self.peek_span().clone(),
            )),
        }
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        let start = self.start_pos();
        match self.peek_token() {
            Token::TyInt | Token::TyFloat | Token::TyBool | Token::TyChar => {
                let lit = self.parse_lit_typ()?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Type::Lit { lit, span })
            }
            Token::UpperIdent => {
                let end = self.end_pos();
                let span = Span { start, end };
                let cons = self.parse_upper_var()?;
                let flds = Vec::new();
                Ok(Type::Cons { cons, flds, span })
            }
            Token::LParen => {
                let typs =
                    self.delimited_list(Token::LParen, Token::Comma, Token::RParen, |par| {
                        par.parse_type()
                    })?;
                let end = self.end_pos();
                let span = Span { start, end };
                match typs.len() {
                    0 => Err(ParseError::FailedToParse(
                        "tuple of types",
                        self.peek_token(),
                        self.peek_span().clone(),
                    )),
                    1 => Ok(typs.into_iter().next().unwrap()),
                    _ => Ok(Type::Tuple { flds: typs, span }),
                }
            }
            Token::Unit => {
                self.next_token()?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok(Type::Tuple {
                    flds: Vec::new(),
                    span,
                })
            }
            _tok => Err(ParseError::FailedToParse(
                "type",
                self.peek_token(),
                self.peek_span().clone(),
            )),
        }
    }

    fn parse_varient(&mut self) -> ParseResult<Constructor> {
        let start = self.start_pos();
        let name = self.parse_upper_var()?;
        let flds = if let Token::LParen = self.peek_token() {
            self.delimited_list(Token::LParen, Token::Comma, Token::RParen, |par| {
                par.parse_type()
            })?
        } else {
            Vec::new()
        };
        let end = self.end_pos();
        let span = Span { start, end };
        Ok(Constructor { name, flds, span })
    }

    fn parse_data_decl(&mut self) -> ParseResult<DataDecl> {
        let start = self.start_pos();
        self.match_token(Token::Datatype)?;
        let name = self.parse_upper_var()?;
        let vars = self.delimited_list(Token::Where, Token::Bar, Token::End, |par| {
            par.parse_varient()
        })?;
        let end = self.end_pos();
        let span = Span { start, end };
        Ok(DataDecl {
            name,
            cons: vars,
            span,
        })
    }

    fn parse_func_decl(&mut self) -> ParseResult<FuncDecl> {
        let start = self.start_pos();
        self.match_token(Token::Function)?;
        let name = self.parse_lower_var()?;
        let pars = self.delimited_list(Token::LParen, Token::Comma, Token::RParen, |par| {
            let ident = par.parse_lower_var()?;
            par.match_token(Token::Colon)?;
            let typ = par.parse_type()?;
            Ok((ident, typ))
        })?;

        let res: Type = self
            .option(|par| {
                par.match_token(Token::Arrow)?;
                par.parse_type()
            })?
            .unwrap_or_else(|| {
                let start = self.start_pos();
                let end = self.end_pos();
                let span = Span { start, end };
                Type::Tuple {
                    flds: Vec::new(),
                    span,
                }
            });

        self.match_token(Token::Begin)?;
        let body = self.parse_expr()?;
        self.match_token(Token::End)?;
        let end = self.end_pos();
        let span = Span { start, end };
        Ok(FuncDecl {
            name,
            pars,
            res,
            body,
            span,
        })
    }

    fn parse_pos_int(&mut self) -> ParseResult<usize> {
        match self.peek_token() {
            Token::Int => {
                let x = self.peek_slice().parse::<i64>().unwrap();
                if x >= 0 {
                    self.next_token()?;
                    Ok(x as usize)
                } else {
                    Err(ParseError::FailedToParse(
                        "positive integer",
                        self.peek_token(),
                        self.peek_span().clone(),
                    ))
                }
            }
            _ => Err(ParseError::FailedToParse(
                "positive integer",
                self.peek_token(),
                self.peek_span().clone(),
            )),
        }
    }

    fn parse_bool(&mut self) -> ParseResult<bool> {
        match self.peek_token() {
            Token::Bool => {
                let x = self.peek_slice().parse::<bool>().unwrap();
                self.next_token()?;
                Ok(x)
            }
            _ => Err(ParseError::FailedToParse(
                "boolean literal",
                self.peek_token(),
                self.peek_span().clone(),
            )),
        }
    }

    fn parse_query_decl(&mut self) -> ParseResult<QueryDecl> {
        let start = self.start_pos();
        self.match_token(Token::Query)?;
        let entry = self.parse_lower_var()?;
        let params = self.delimited_list(Token::LParen, Token::Comma, Token::RParen, |par| {
            par.parse_query_param()
        })?;
        let end = self.end_pos();
        let span = Span { start, end };
        Ok(QueryDecl {
            entry,
            params,
            span,
        })
    }

    fn parse_query_param(&mut self) -> ParseResult<(QueryParam, Span)> {
        let start = self.start_pos();
        let name = self.parse_lower_var()?;

        match name.ident.as_str() {
            "depth_step" => {
                self.match_token(Token::Equal)?;
                let val = self.parse_pos_int()?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok((QueryParam::DepthStep(val), span))
            }

            "depth_limit" => {
                self.match_token(Token::Equal)?;
                let val = self.parse_pos_int()?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok((QueryParam::DepthLimit(val), span))
            }

            "answer_limit" => {
                self.match_token(Token::Equal)?;
                let val = self.parse_pos_int()?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok((QueryParam::AnswerLimit(val), span))
            }
            "answer_pause" => {
                self.match_token(Token::Equal)?;
                let val = self.parse_bool()?;
                let end = self.end_pos();
                let span = Span { start, end };
                Ok((QueryParam::AnswerPause(val), span))
            }
            _ => {
                let end = self.end_pos();
                let span = Span { start, end };
                Err(ParseError::FailedToParse(
                    "query parameter",
                    Token::LowerIdent,
                    span,
                ))
            }
        }
    }

    fn parse_decl(&mut self) -> ParseResult<Declaration> {
        match self.peek_token() {
            Token::Datatype => {
                let decl = self.parse_data_decl()?;
                Ok(Declaration::Data(decl))
            }
            Token::Function => {
                let decl = self.parse_func_decl()?;
                Ok(Declaration::Func(decl))
            }
            Token::Query => {
                let decl = self.parse_query_decl()?;
                Ok(Declaration::Query(decl))
            }
            _tok => Err(ParseError::FailedToParse(
                "declaration",
                self.peek_token(),
                self.peek_span().clone(),
            )),
        }
    }

    fn parse_program(&mut self) -> Program {
        let mut decls: Vec<Declaration> = Vec::new();
        loop {
            match self.peek_token() {
                Token::Datatype | Token::Function | Token::Query => {
                    // toplevel error recovering
                    match self.parse_decl() {
                        Ok(res) => decls.push(res),
                        Err(err) => self.errors.push(err),
                    }
                }
                Token::TokError => {
                    self.errors
                        .push(ParseError::LexerError(self.peek_span().clone()));
                    self.cursor += 1;
                }
                Token::EndOfFile => break,
                tok => {
                    self.errors.push(ParseError::FailedToParse(
                        "declaration",
                        tok,
                        self.peek_span().clone(),
                    ));
                    // skip all tokens before the next "header" token
                    loop {
                        if let Token::Datatype | Token::Function | Token::Query | Token::EndOfFile =
                            self.peek_token()
                        {
                            break;
                        }
                        self.next_token().unwrap();
                    }
                }
            }
        }
        self.match_token(Token::EndOfFile).unwrap();

        let mut datas = Vec::new();
        let mut funcs = Vec::new();
        let mut querys = Vec::new();

        for decl in decls.into_iter() {
            match decl {
                Declaration::Data(data) => datas.push(data),
                Declaration::Func(func) => funcs.push(func),
                Declaration::Query(query) => querys.push(query),
            }
        }

        Program {
            datas,
            funcs,
            querys,
        }
    }
}

pub fn parse_program(src: &str) -> (Program, Vec<ParseError>) {
    let mut pass = Parser::new(src);
    let prog = pass.parse_program();
    (prog, pass.errors)
}

#[test]
fn parser_test() {
    let src = r#"
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

function append(xs: IntList, x: Int) -> IntList
begin
    match xs with
    | Cons(head, tail) => Cons(head, append(tail, x))
    | Nil => Cons(x, Nil)
    end
end

function is_elem(xs: IntList, x: Int) -> Bool
begin
    match xs with
    | Cons(head, tail) => if head == x then true else is_elem(tail, x) 
    | Nil => false
    end
end

function is_elem_after_append(xs: IntList, x: Int) -> Bool
begin
    guard !is_elem(append(xs, x), x);
    true
end

query is_elem_after_append(depth_step=5, depth_limit=50, answer_limit=1)
"#;
    let (_prog, errs) = parse_program(&src);
    // println!("{:#?}", prog);

    assert!(errs.is_empty());

    // for diag in diags {
    //     println!("{}", diag.report(s, 10));
    // }
}
