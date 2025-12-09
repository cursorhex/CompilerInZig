const std = @import("std");

const TokenArrayList = @import("custom_array_list.zig").CustomArrayList(Token);

pub const TokenType = enum {
    Identifier,
    Number,
    String,
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Colon,
    Comma,
    Var,
    Const,
    VarGlobal,
    ConstGlobal,
    Dot,
    DotDot,
    PlusPlus,
    Section,
    Program,
    Run,
    Order,
    Mode,
    Optimize,
    Repeat,
    Parallel,
    Timeout,
    OnError,
    Trace,
    Debug,
    Release,
    Speed,
    Size,
    Continue,
    Stop,
    True,
    False,
    Bytecode,
    As,
    If,
    ElseIf,
    Else,
    EqualEqual, // ==
    NotEqual, // !=
    Greater, // >
    Less, // <
    GreaterEq, // >=
    LessEq, // <=
    Arrow, // =>
    Choose,
    Eof,
};

pub const Token = struct {
    t: TokenType,
    text: []const u8,
};

pub fn lex(src: []const u8, allocator: std.mem.Allocator) ![]Token {
    var tokens = TokenArrayList.init(allocator);
    defer tokens.deinit();

    var i: usize = 0;
    while (i < src.len) : (i += 1) {
        switch (src[i]) {
            ' ', '\n', '\t', '\r' => continue,
            '(' => try tokens.append(.{ .t = .LParen, .text = src[i .. i + 1] }),
            ')' => try tokens.append(.{ .t = .RParen, .text = src[i .. i + 1] }),
            '{' => try tokens.append(.{ .t = .LBrace, .text = src[i .. i + 1] }),
            '}' => try tokens.append(.{ .t = .RBrace, .text = src[i .. i + 1] }),
            '[' => try tokens.append(.{ .t = .LBracket, .text = src[i .. i + 1] }),
            ']' => try tokens.append(.{ .t = .RBracket, .text = src[i .. i + 1] }),
            ':' => try tokens.append(.{ .t = .Colon, .text = src[i .. i + 1] }),
            ',' => try tokens.append(.{ .t = .Comma, .text = src[i .. i + 1] }),
            '+' => {
                if (i + 1 < src.len and src[i + 1] == '+') {
                    try tokens.append(.{ .t = .PlusPlus, .text = src[i .. i + 2] });
                    i += 1;
                } else {
                    try tokens.append(.{ .t = .Plus, .text = src[i .. i + 1] });
                }
            },
            '-' => try tokens.append(.{ .t = .Minus, .text = src[i .. i + 1] }),
            '*' => try tokens.append(.{ .t = .Star, .text = src[i .. i + 1] }),
            '/' => {
                if (i + 1 < src.len and src[i + 1] == '/') {
                    i += 2;
                    while (i < src.len and src[i] != '\n') : (i += 1) {}
                    continue;
                } else {
                    try tokens.append(.{ .t = .Slash, .text = src[i .. i + 1] });
                }
            },
            '.' => {
                if (i + 1 < src.len and src[i + 1] == '.') {
                    try tokens.append(.{ .t = .DotDot, .text = src[i .. i + 2] });
                    i += 1;
                } else {
                    try tokens.append(.{ .t = .Dot, .text = src[i .. i + 1] });
                }
            },
            '"' => {
                const start = i + 1;
                i += 1;
                while (i < src.len and src[i] != '"') : (i += 1) {}
                if (i >= src.len) return error.UnterminatedString;
                try tokens.append(.{
                    .t = .String,
                    .text = src[start..i],
                });
            },
            '=' => {
                if (i + 1 < src.len and src[i + 1] == '=') {
                    try tokens.append(.{ .t = .EqualEqual, .text = src[i .. i + 2] });
                    i += 1;
                } else if (i + 1 < src.len and src[i + 1] == '>') {
                    try tokens.append(.{ .t = .Arrow, .text = src[i .. i + 2] });
                    i += 1;
                } else {
                    return error.UnexpectedCharacter;
                }
            },
            '!' => {
                if (i + 1 < src.len and src[i + 1] == '=') {
                    try tokens.append(.{ .t = .NotEqual, .text = src[i .. i + 2] });
                    i += 1;
                } else {
                    return error.UnexpectedCharacter;
                }
            },
            '>' => {
                if (i + 1 < src.len and src[i + 1] == '=') {
                    try tokens.append(.{ .t = .GreaterEq, .text = src[i .. i + 2] });
                    i += 1;
                } else {
                    try tokens.append(.{ .t = .Greater, .text = src[i .. i + 1] });
                }
            },
            '<' => {
                if (i + 1 < src.len and src[i + 1] == '=') {
                    try tokens.append(.{ .t = .LessEq, .text = src[i .. i + 2] });
                    i += 1;
                } else {
                    try tokens.append(.{ .t = .Less, .text = src[i .. i + 1] });
                }
            },
            else => |c| {
                if (std.ascii.isDigit(c)) {
                    const start = i;
                    var is_hex = false;
                    var j = i;
                    while (j < src.len and std.ascii.isAlphanumeric(src[j])) {
                        if ((src[j] >= 'a' and src[j] <= 'f') or (src[j] >= 'A' and src[j] <= 'F')) is_hex = true;
                        j += 1;
                    }

                    if (is_hex) {
                        i = j;
                        try tokens.append(.{
                            .t = .Identifier,
                            .text = src[start..i],
                        });
                        i -= 1;
                    } else {
                        while (i < src.len and std.ascii.isDigit(src[i])) : (i += 1) {}
                        try tokens.append(.{
                            .t = .Number,
                            .text = src[start..i],
                        });
                        i -= 1;
                    }
                } else if (std.ascii.isAlphabetic(c)) {
                    const start = i;
                    while (i < src.len and (std.ascii.isAlphanumeric(src[i]) or src[i] == '_')) : (i += 1) {}
                    const word = src[start..i];
                    const tok = if (std.mem.eql(u8, word, "var"))
                        TokenType.Var
                    else if (std.mem.eql(u8, word, "const"))
                        TokenType.Const
                    else if (std.mem.eql(u8, word, "VAR"))
                        TokenType.VarGlobal
                    else if (std.mem.eql(u8, word, "CONST"))
                        TokenType.ConstGlobal
                    else if (std.mem.eql(u8, word, "section"))
                        TokenType.Section
                    else if (std.mem.eql(u8, word, "program"))
                        TokenType.Program
                    else if (std.mem.eql(u8, word, "run"))
                        TokenType.Run
                    else if (std.mem.eql(u8, word, "order"))
                        TokenType.Order
                    else if (std.mem.eql(u8, word, "mode"))
                        TokenType.Mode
                    else if (std.mem.eql(u8, word, "optimize"))
                        TokenType.Optimize
                    else if (std.mem.eql(u8, word, "repeat"))
                        TokenType.Repeat
                    else if (std.mem.eql(u8, word, "parallel"))
                        TokenType.Parallel
                    else if (std.mem.eql(u8, word, "timeout"))
                        TokenType.Timeout
                    else if (std.mem.eql(u8, word, "onerror"))
                        TokenType.OnError
                    else if (std.mem.eql(u8, word, "trace"))
                        TokenType.Trace
                    else if (std.mem.eql(u8, word, "debug"))
                        TokenType.Debug
                    else if (std.mem.eql(u8, word, "release"))
                        TokenType.Release
                    else if (std.mem.eql(u8, word, "speed"))
                        TokenType.Speed
                    else if (std.mem.eql(u8, word, "size"))
                        TokenType.Size
                    else if (std.mem.eql(u8, word, "continue"))
                        TokenType.Continue
                    else if (std.mem.eql(u8, word, "stop"))
                        TokenType.Stop
                    else if (std.mem.eql(u8, word, "true"))
                        TokenType.True
                    else if (std.mem.eql(u8, word, "false"))
                        TokenType.False
                    else if (std.mem.eql(u8, word, "bytecode"))
                        TokenType.Bytecode
                    else if (std.mem.eql(u8, word, "as"))
                        TokenType.As
                    else if (std.mem.eql(u8, word, "if"))
                        TokenType.If
                    else if (std.mem.eql(u8, word, "elseif"))
                        TokenType.ElseIf
                    else if (std.mem.eql(u8, word, "else"))
                        TokenType.Else
                    else if (std.mem.eql(u8, word, "choose"))
                        TokenType.Choose
                    else
                        TokenType.Identifier;
                    try tokens.append(.{ .t = tok, .text = word });
                    i -= 1;
                }
            },
        }
    }

    try tokens.append(.{ .t = .Eof, .text = "" });
    return tokens.toOwnedSlice();
}
