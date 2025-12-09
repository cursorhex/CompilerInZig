const std = @import("std");
const TokenType = @import("lexer.zig").TokenType;
const Token = @import("lexer.zig").Token;
const Ast = @import("ast.zig");
const StmtArrayList = @import("custom_array_list.zig").CustomArrayList(*Ast.Stmt);

const ParseError = error{
    UnsupportedStatement,
    ExpectedNumber,
    ExpectedExpression,
    SyntaxError,
    InvalidVarDeclaration,
    InvalidConstDeclaration,
    InvalidCallExpression,
    InvalidBytecodeBlock,
    OutOfMemory,
    InvalidCharacter,
    Overflow,
};

pub fn parse(tokens_slice: []const Token, allocator: std.mem.Allocator) ParseError![]*Ast.Stmt {
    var p = Parser.init(tokens_slice, allocator);
    return try p.parse();
}

pub fn parseProgram(tokens_slice: []const Token, allocator: std.mem.Allocator, src: []const u8) ParseError!Ast.Program {
    var p = Parser.init(tokens_slice, allocator, src);
    return try p.parseFullProgram();
}
pub fn freeAst(stmts: []*Ast.Stmt, allocator: std.mem.Allocator) void {
    for (stmts) |stmt| {
        freeStmt(stmt, allocator);
    }
    allocator.free(stmts);
}

pub fn freeStmt(stmt: *Ast.Stmt, allocator: std.mem.Allocator) void {
    switch (stmt.*) {
        .VarDecl => |var_decl| {
            freeExpr(var_decl.value, allocator);
        },
        .Mutation => |mutation| {
            freeExpr(mutation.value, allocator);
        },
        .ExprStmt => |expr| freeExpr(expr, allocator),
        .BytecodeExec => |block| {
            allocator.free(block.data);
        },
    }
    allocator.destroy(stmt);
}

fn freeExpr(expr: *Ast.Expr, allocator: std.mem.Allocator) void {
    switch (expr.*) {
        .Number, .String, .Var => {},
        .Add => |bin_op| {
            freeExpr(bin_op.left, allocator);
            freeExpr(bin_op.right, allocator);
        },
        .Sub => |bin_op| {
            freeExpr(bin_op.left, allocator);
            freeExpr(bin_op.right, allocator);
        },
        .Mul => |bin_op| {
            freeExpr(bin_op.left, allocator);
            freeExpr(bin_op.right, allocator);
        },
        .Div => |bin_op| {
            freeExpr(bin_op.left, allocator);
            freeExpr(bin_op.right, allocator);
        },
        .Increment => |inner| {
            freeExpr(inner, allocator);
        },
        .Call => |call_expr| {
            freeExpr(call_expr.argument, allocator);
        },
    }
    allocator.destroy(expr);
}

const Parser = struct {
    tokens: []const Token,
    allocator: std.mem.Allocator,
    current: usize,
    src: []const u8, // NUOVO

    pub fn init(tokens: []const Token, allocator: std.mem.Allocator, src: []const u8) Parser {
        return Parser{
            .tokens = tokens,
            .allocator = allocator,
            .current = 0,
            .src = src,
        };
    }

    fn parseSection(self: *Parser) ParseError!*Ast.Section {
        try self.consume(.Section);
        try self.consume(.Identifier);
        const section_name = self.previous().text;

        try self.consume(.LBrace);
        try self.consume(.LBrace);

        var statements = StmtArrayList.init(self.allocator);
        defer statements.deinit();

        while (!self.check(.RBrace)) {
            try statements.append(try self.parseStatement());
        }

        try self.consume(.RBrace);
        try self.consume(.RBrace);

        const section = try self.allocator.create(Ast.Section);
        section.* = .{
            .name = section_name,
            .statements = try statements.toOwnedSlice(),
        };
        return section;
    }

    fn parseProgramRun(self: *Parser) ParseError!*Ast.ProgramRun {
        try self.consume(.Program);
        try self.consume(.Dot);
        try self.consume(.Run);
        try self.consume(.LBracket);

        const StringArrayList = @import("custom_array_list.zig").CustomArrayList([]const u8);
        var order_names = StringArrayList.init(self.allocator);
        defer order_names.deinit();

        var config = Ast.ProgramConfig{};

        while (!self.check(.RBracket)) {
            if (self.check(.Order)) {
                _ = self.advance();
                try self.consume(.Colon);
                try self.consume(.LBrace);

                while (!self.check(.RBrace)) {
                    if (self.check(.Identifier)) {
                        _ = self.advance();
                        try order_names.append(self.previous().text);

                        if (self.match(.{.Comma})) {
                            continue;
                        }
                    } else {
                        return error.SyntaxError;
                    }
                }

                try self.consume(.RBrace);
            } else if (self.check(.Mode)) {
                _ = self.advance();
                try self.consume(.Colon);

                if (self.match(.{.Debug})) {
                    config.mode = .Debug;
                } else if (self.match(.{.Release})) {
                    config.mode = .Release;
                } else {
                    return error.SyntaxError;
                }
            } else if (self.check(.Optimize)) {
                _ = self.advance();
                try self.consume(.Colon);

                if (self.match(.{.Speed})) {
                    config.optimize = .Speed;
                } else if (self.match(.{.Size})) {
                    config.optimize = .Size;
                } else {
                    return error.SyntaxError;
                }
            } else if (self.check(.Repeat)) {
                _ = self.advance();
                try self.consume(.Colon);
                try self.consume(.Number);
                const repeat_val = try std.fmt.parseInt(i64, self.previous().text, 10);
                config.repeat = repeat_val;
            } else if (self.check(.Parallel)) {
                _ = self.advance();
                try self.consume(.Colon);

                if (self.match(.{.True})) {
                    config.parallel = true;
                } else if (self.match(.{.False})) {
                    config.parallel = false;
                } else {
                    return error.SyntaxError;
                }
            } else if (self.check(.Timeout)) {
                _ = self.advance();
                try self.consume(.Colon);
                try self.consume(.Number);
                const timeout_val = try std.fmt.parseInt(i64, self.previous().text, 10);
                config.timeout = timeout_val;
            } else if (self.check(.OnError)) {
                _ = self.advance();
                try self.consume(.Colon);

                if (self.match(.{.Continue})) {
                    config.on_error = .Continue;
                } else if (self.match(.{.Stop})) {
                    config.on_error = .Stop;
                } else {
                    return error.SyntaxError;
                }
            } else if (self.check(.Trace)) {
                _ = self.advance();
                try self.consume(.Colon);

                if (self.match(.{.True})) {
                    config.trace = true;
                } else if (self.match(.{.False})) {
                    config.trace = false;
                } else {
                    return error.SyntaxError;
                }
            } else {
                return error.SyntaxError;
            }

            _ = self.match(.{.Comma});
        }

        try self.consume(.RBracket);

        const program_run = try self.allocator.create(Ast.ProgramRun);
        program_run.* = .{
            .order = try order_names.toOwnedSlice(),
            .config = config,
        };
        return program_run;
    }

    fn parseFullProgram(self: *Parser) ParseError!Ast.Program {
        const SectionArrayList = @import("custom_array_list.zig").CustomArrayList(*Ast.Section);
        var sections = SectionArrayList.init(self.allocator);
        defer sections.deinit();

        var loose_statements = StmtArrayList.init(self.allocator);
        defer loose_statements.deinit();

        var program_run: ?*Ast.ProgramRun = null;

        while (!self.isAtEnd()) {
            if (self.check(.Section)) {
                try sections.append(try self.parseSection());
            } else if (self.check(.Program)) {
                program_run = try self.parseProgramRun();
            } else {
                try loose_statements.append(try self.parseStatement());
            }
        }

        return Ast.Program{
            .sections = try sections.toOwnedSlice(),
            .loose_statements = try loose_statements.toOwnedSlice(),
            .program_run = program_run,
        };
    }

    pub fn parse(self: *Parser) ParseError![]*Ast.Stmt {
        var statements = StmtArrayList.init(self.allocator);
        defer statements.deinit();

        while (!self.isAtEnd()) {
            try statements.append(try self.parseStatement());
        }
        return statements.toOwnedSlice();
    }

    fn parseStatement(self: *Parser) ParseError!*Ast.Stmt {
        // NUOVO: bytecode block
        if (self.check(.Bytecode)) {
            return self.parseBytecodeBlock();
        }

        // var(x): 10 o VAR(x): 10
        if (self.match(.{ .Var, .VarGlobal })) {
            const is_global = self.previous().t == .VarGlobal;
            return self.parseVarDeclaration(is_global);
        }

        // const(y): 20 o CONST(y): 20
        if (self.match(.{ .Const, .ConstGlobal })) {
            const is_global = self.previous().t == .ConstGlobal;
            return self.parseConstDeclaration(is_global);
        }

        // (y): expr - mutation
        if (self.check(.LParen)) {
            const saved_pos = self.current;
            _ = self.advance();
            if (self.check(.Identifier)) {
                _ = self.advance();
                if (self.check(.RParen)) {
                    _ = self.advance();
                    if (self.check(.Colon)) {
                        self.current = saved_pos;
                        return self.parseMutation();
                    }
                }
            }
            self.current = saved_pos;
        }

        const expr = try self.parseExpression();
        const stmt = try self.allocator.create(Ast.Stmt);
        stmt.* = .{ .ExprStmt = expr };
        return stmt;
    }

    fn parseBytecodeBlock(self: *Parser) !*Ast.Stmt {
        try self.consume(.Bytecode);
        try self.consume(.LBracket);

        while (self.peek().t != .Eof and !self.check(.RBracket)) {
            if (self.peek().t == .Identifier) {
                const key = self.peek().text;
                if (std.mem.eql(u8, key, "code")) {
                    self.current += 1;
                    try self.consume(.Colon);
                    try self.consume(.LBrace);

                    // MODIFICATO: Accetta sia Identifier che Number per hex string
                    var hex_str: []const u8 = undefined;
                    if (self.check(.Identifier)) {
                        hex_str = self.peek().text;
                        self.current += 1;
                    } else if (self.check(.Number)) {
                        hex_str = self.peek().text;
                        self.current += 1;
                    } else {
                        return error.InvalidBytecodeBlock;
                    }

                    try self.consume(.RBrace);

                    const data = try hexToBytes(hex_str, self.allocator);

                    try self.consume(.RBracket);

                    const stmt = try self.allocator.create(Ast.Stmt);
                    stmt.* = .{ .BytecodeExec = .{ .data = data } };
                    return stmt;
                }
            }
            self.current += 1;
        }

        return error.InvalidBytecodeBlock;
    }

    fn hexToBytes(hex: []const u8, allocator: std.mem.Allocator) ![]u8 {
        if (hex.len % 2 != 0) return error.InvalidCharacter;

        const len = hex.len / 2;
        var result = try allocator.alloc(u8, len);

        for (0..len) |i| {
            const hi = try std.fmt.charToDigit(hex[i * 2], 16);
            const lo = try std.fmt.charToDigit(hex[i * 2 + 1], 16);
            result[i] = (hi << 4) | lo;
        }

        return result;
    }

    fn parseVarDeclaration(self: *Parser, is_global: bool) ParseError!*Ast.Stmt {
        try self.consume(.LParen);
        try self.consume(.Identifier);
        const name_token = self.previous();
        try self.consume(.RParen);
        try self.consume(.Colon);
        const value = try self.parseExpression();

        const stmt = try self.allocator.create(Ast.Stmt);
        stmt.* = .{ .VarDecl = .{ .name = name_token.text, .value = value, .is_const = false, .is_global = is_global } };
        return stmt;
    }

    fn parseConstDeclaration(self: *Parser, is_global: bool) ParseError!*Ast.Stmt {
        try self.consume(.LParen);
        try self.consume(.Identifier);
        const name_token = self.previous();
        try self.consume(.RParen);
        try self.consume(.Colon);
        const value = try self.parseExpression();

        const stmt = try self.allocator.create(Ast.Stmt);
        stmt.* = .{ .VarDecl = .{ .name = name_token.text, .value = value, .is_const = true, .is_global = is_global } };
        return stmt;
    }

    fn parseMutation(self: *Parser) ParseError!*Ast.Stmt {
        try self.consume(.LParen);
        try self.consume(.Identifier);
        const name_token = self.previous();
        try self.consume(.RParen);
        try self.consume(.Colon);
        const value = try self.parseExpression();

        const stmt = try self.allocator.create(Ast.Stmt);
        stmt.* = .{ .Mutation = .{ .name = name_token.text, .value = value } };
        return stmt;
    }

    fn parseExpression(self: *Parser) ParseError!*Ast.Expr {
        return self.parseAddition();
    }

    fn parseAddition(self: *Parser) ParseError!*Ast.Expr {
        var expr = try self.parseMultiplication();

        while (self.match(.{ .Plus, .Minus })) {
            const op_token = self.previous();
            const right = try self.parseMultiplication();
            const new_expr = try self.allocator.create(Ast.Expr);
            new_expr.* = switch (op_token.t) {
                .Plus => .{ .Add = .{ .left = expr, .right = right } },
                .Minus => .{ .Sub = .{ .left = expr, .right = right } },
                else => unreachable,
            };
            expr = new_expr;
        }
        return expr;
    }

    fn parseMultiplication(self: *Parser) ParseError!*Ast.Expr {
        var expr = try self.parsePrimary();

        while (self.match(.{ .Star, .Slash })) {
            const op_token = self.previous();
            const right = try self.parsePrimary();
            const new_expr = try self.allocator.create(Ast.Expr);
            new_expr.* = switch (op_token.t) {
                .Star => .{ .Mul = .{ .left = expr, .right = right } },
                .Slash => .{ .Div = .{ .left = expr, .right = right } },
                else => unreachable,
            };
            expr = new_expr;
        }
        return expr;
    }

    fn parsePrimary(self: *Parser) ParseError!*Ast.Expr {
        if (self.match(.{.Number})) {
            const token = self.previous();
            const value = try std.fmt.parseInt(i64, token.text, 10);
            const expr = try self.allocator.create(Ast.Expr);
            expr.* = .{ .Number = value };
            return expr;
        }

        if (self.check(.LParen)) {
            const saved_pos = self.current;
            _ = self.advance();

            if (self.check(.Identifier)) {
                const id_token = self.peek();
                _ = self.advance();

                if (self.match(.{.RParen})) {
                    if (self.match(.{.PlusPlus})) {
                        const var_expr = try self.allocator.create(Ast.Expr);
                        var_expr.* = .{ .Var = id_token.text };
                        const inc_expr = try self.allocator.create(Ast.Expr);
                        inc_expr.* = .{ .Increment = var_expr };
                        return inc_expr;
                    } else {
                        const expr = try self.allocator.create(Ast.Expr);
                        expr.* = .{ .Var = id_token.text };
                        return expr;
                    }
                }
            }

            self.current = saved_pos;
            _ = self.advance();
            const expr = try self.parseExpression();
            try self.consume(.RParen);
            return expr;
        }

        if (self.peek().t == .Identifier and std.mem.eql(u8, self.peek().text, "io")) {
            _ = self.advance(); // "io"
            try self.consume(.Dot);
            try self.consume(.Identifier);
            const func_name = self.previous().text;

            var argument: *Ast.Expr = undefined;
            var type_hint: []const u8 = "";

            // Caso 1: io.print (expr)
            if (self.match(.{.LParen})) {
                argument = try self.parseExpression();
                try self.consume(.RParen);
            }
            // Caso 2: io.print [Type](qualunque testo raw)
            else if (self.match(.{.LBracket})) {
                try self.consume(.Identifier); // tipo, es: txt, u8, i8
                type_hint = self.previous().text;
                try self.consume(.RBracket);

                // Qui NON chiamiamo parseExpression, ma prendiamo il testo raw fra ( e )
                try self.consume(.LParen);

                // Usa le posizioni dei token per estrarre dal sorgente originale
                const start_tok = self.peek(); // primo token dentro le parentesi
                // cerca fino alla RParen corrispondente
                var end_index = self.current;
                while (end_index < self.tokens.len and self.tokens[end_index].t != .RParen) {
                    end_index += 1;
                }
                if (end_index >= self.tokens.len) return error.SyntaxError;

                const end_tok = self.tokens[end_index - 1]; // ultimo token prima di ')'

                // Calcoliamo gli offset nel src: usiamo text slices come proxy
                const start_ptr = start_tok.text.ptr;
                const end_ptr = end_tok.text.ptr + end_tok.text.len;
                const start_off = @intFromPtr(start_ptr) - @intFromPtr(self.src.ptr);
                const end_off = @intFromPtr(end_ptr) - @intFromPtr(self.src.ptr);
                const raw_slice = self.src[start_off..end_off];

                self.current = end_index;
                try self.consume(.RParen);

                const str_expr = try self.allocator.create(Ast.Expr);
                str_expr.* = .{ .String = raw_slice };
                argument = str_expr;
            }
            // Caso 3: io.print literalIdentifier
            else if (self.check(.Identifier)) {
                _ = self.advance();
                const token = self.previous();
                const str_expr = try self.allocator.create(Ast.Expr);
                str_expr.* = .{ .String = token.text };
                argument = str_expr;
                type_hint = "txt";
            } else {
                return error.InvalidCallExpression;
            }

            const call_expr = try self.allocator.create(Ast.Expr);
            call_expr.* = .{
                .Call = .{
                    .library = "io",
                    .function = func_name,
                    .argument = argument,
                    .type_hint = type_hint,
                },
            };
            return call_expr;
        }

        if (self.match(.{.Identifier})) {
            const token = self.previous();
            const expr = try self.allocator.create(Ast.Expr);
            expr.* = .{ .Var = token.text };
            return expr;
        }

        return error.ExpectedExpression;
    }

    fn consume(self: *Parser, expected: TokenType) !void {
        if (self.peek().t != expected) {
            return error.SyntaxError;
        }
        self.current += 1;
    }

    fn match(self: *Parser, comptime types_tuple: anytype) bool {
        inline for (types_tuple) |t| {
            if (self.check(t)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().t == token_type;
    }

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn isAtEnd(self: *Parser) bool {
        return self.peek().t == .Eof;
    }

    fn peek(self: *Parser) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Parser) Token {
        return self.tokens[self.current - 1];
    }
};
