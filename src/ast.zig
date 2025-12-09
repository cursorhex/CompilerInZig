const std = @import("std");

pub const BinaryOp = struct {
    left: *Expr,
    right: *Expr,
};

pub const Expr = union(enum) {
    Number: i64,
    String: []const u8,
    Var: []const u8,
    Add: BinaryOp,
    Sub: BinaryOp,
    Mul: BinaryOp,
    Div: BinaryOp,
    Increment: *Expr,
    Concat: BinaryOp,
    Call: struct {
        library: []const u8,
        function: []const u8,
        argument: *Expr,
        type_hint: []const u8,
    },
};

pub const Stmt = union(enum) {
    VarDecl: struct {
        name: []const u8,
        value: *Expr,
        is_const: bool,
        is_global: bool,
    },
    Mutation: struct {
        name: []const u8,
        value: *Expr,
    },
    ExprStmt: *Expr,
    BytecodeExec: struct {
        data: []const u8,
    },
};

pub const Section = struct {
    name: []const u8,
    statements: []*Stmt,
};

pub const ProgramConfig = struct {
    mode: enum { Debug, Release } = .Debug,
    optimize: enum { Speed, Size } = .Speed,
    repeat: i64 = 1,
    parallel: bool = false,
    timeout: i64 = 5000,
    on_error: enum { Continue, Stop } = .Stop,
    trace: bool = false,
};

pub const ProgramRun = struct {
    order: [][]const u8,
    config: ProgramConfig,
};

pub const Program = struct {
    sections: []*Section,
    loose_statements: []*Stmt,
    program_run: ?*ProgramRun,
};
