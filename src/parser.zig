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
    OutOfMemory,
    InvalidCharacter,
    Overflow,
};

pub fn parse(tokens_slice: []const Token, allocator: std.mem.Allocator) ParseError![]*Ast.Stmt {
    var p = Parser.init(tokens_slice, allocator);
    return try p.parse();
}
pub fn parseProgram(tokens_slice: []const Token, allocator: std.mem.Allocator) ParseError!Ast.Program {
    var p = Parser.init(tokens_slice, allocator);
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
        .ConstDecl => |const_decl| {
            freeExpr(const_decl.value, allocator);
        },
        .Mutation => |mutation| {
            freeExpr(mutation.value, allocator);
        },
        .Expression => |expr| freeExpr(expr, allocator),
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

    pub fn init(tokens: []const Token, allocator: std.mem.Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .allocator = allocator,
            .current = 0,
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

        // Parse multiple config options
        while (!self.check(.RBracket)) {
            // DEBUG: stampa il token corrente
            std.debug.print("Current token: {any}\n", .{self.peek()});

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
                        std.debug.print("ERROR: Expected Identifier, got {any}\n", .{self.peek()});
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
                    std.debug.print("ERROR: Expected Debug or Release\n", .{});
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
                    std.debug.print("ERROR: Expected Speed or Size\n", .{});
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
                    std.debug.print("ERROR: Expected True or False\n", .{});
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
                    std.debug.print("ERROR: Expected Continue or Stop\n", .{});
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
                    std.debug.print("ERROR: Expected True or False\n", .{});
                    return error.SyntaxError;
                }
            } else {
                std.debug.print("ERROR: Unexpected token in program.run: {any}\n", .{self.peek()});
                return error.SyntaxError;
            }

            // Consume optional comma between config options
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
                // NUOVO: permetti statement "loose"
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
            _ = self.advance(); // consume '('
            if (self.check(.Identifier)) {
                _ = self.advance(); // consume identifier
                if (self.check(.RParen)) {
                    _ = self.advance(); // consume ')'
                    if (self.check(.Colon)) {
                        // It's a mutation
                        self.current = saved_pos;
                        return self.parseMutation();
                    }
                }
            }
            self.current = saved_pos;
        }

        const expr = try self.parseExpression();
        const stmt = try self.allocator.create(Ast.Stmt);
        stmt.* = .{ .Expression = expr };
        return stmt;
    }

    fn parseVarDeclaration(self: *Parser, is_global: bool) ParseError!*Ast.Stmt {
        try self.consume(.LParen);
        try self.consume(.Identifier);
        const name_token = self.previous();
        try self.consume(.RParen);
        try self.consume(.Colon);
        const value = try self.parseExpression();

        const stmt = try self.allocator.create(Ast.Stmt);
        stmt.* = .{ .VarDecl = .{ .name = name_token.text, .value = value, .is_global = is_global } };
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
        stmt.* = .{ .ConstDecl = .{ .name = name_token.text, .value = value, .is_global = is_global } };
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

        // Handle (expr)++ for increment
        if (self.check(.LParen)) {
            const saved_pos = self.current;
            _ = self.advance(); // consume '('

            if (self.check(.Identifier)) {
                const id_token = self.peek();
                _ = self.advance(); // consume identifier

                if (self.match(.{.RParen})) {
                    if (self.match(.{.PlusPlus})) {
                        // It's an increment: (x)++
                        const var_expr = try self.allocator.create(Ast.Expr);
                        var_expr.* = .{ .Var = id_token.text };
                        const inc_expr = try self.allocator.create(Ast.Expr);
                        inc_expr.* = .{ .Increment = var_expr };
                        return inc_expr;
                    } else {
                        // Just (identifier) as a variable reference
                        const expr = try self.allocator.create(Ast.Expr);
                        expr.* = .{ .Var = id_token.text };
                        return expr;
                    }
                }
            }

            // Not a special case, parse as grouped expression
            self.current = saved_pos;
            _ = self.advance(); // consume '('
            const expr = try self.parseExpression();
            try self.consume(.RParen);
            return expr;
        }

        if (self.peek().t == .Identifier and std.mem.eql(u8, self.peek().text, "io")) {
            _ = self.advance();
            try self.consume(.Dot);
            try self.consume(.Identifier);
            const func_name = self.previous().text;

            var argument: *Ast.Expr = undefined;

            // Check if there's a space followed by LParen -> io.print (x) evaluates expression
            if (self.match(.{.LParen})) {
                // io.print (expression) - evaluate the expression
                argument = try self.parseExpression();
                try self.consume(.RParen);
            } else if (self.check(.Identifier)) {
                // io.print x - treat x as a string literal
                _ = self.advance();
                const token = self.previous();
                const str_expr = try self.allocator.create(Ast.Expr);
                str_expr.* = .{ .String = token.text };
                argument = str_expr;
            } else {
                return error.InvalidCallExpression;
            }

            const call_expr = try self.allocator.create(Ast.Expr);
            call_expr.* = .{ .Call = .{ .library = "io", .function = func_name, .argument = argument } };
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

    fn consume(self: *Parser, token_type: TokenType) ParseError!void {
        if (self.check(token_type)) {
            _ = self.advance();
            return;
        }
        return error.SyntaxError;
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
