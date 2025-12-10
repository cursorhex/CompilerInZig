const std = @import("std");
const Bytecode = @import("bytecode.zig");
const Ast = @import("ast.zig");
const TempAllocList = @import("custom_array_list.zig").CustomArrayList([]const u8);
const RED = "\x1b[31m";
const YELLOW = "\x1b[33m";
const RESET = "\x1b[0m";

const InterpreterError = error{
    OutOfMemory,
    VariableNotFound,
    CannotMutateConstant,
    FunctionNotFound,
    StageNotFound,
    PipelineNotFound,
    WrongNumberOfArguments,
};

pub const Value = union(enum) {
    Int: i64,
    Str: []const u8,
};

const ValueArrayList = @import("custom_array_list.zig").CustomArrayList(Value);

pub const Var = struct {
    name: []const u8,
    value: Value,
    is_const: bool,
};

const VarArrayList = @import("custom_array_list.zig").CustomArrayList(Var);

pub const Environment = struct {
    global_vars: VarArrayList,
    local_vars: VarArrayList,
    functions: FunctionArrayList,
    stages: StageArrayList,
    pipelines: PipelineArrayList,
    allocator: std.mem.Allocator,
    temp_allocations: TempAllocList,

    pub fn init(allocator: std.mem.Allocator) Environment {
        return Environment{
            .global_vars = VarArrayList.init(allocator),
            .local_vars = VarArrayList.init(allocator),
            .functions = FunctionArrayList.init(allocator),
            .stages = StageArrayList.init(allocator),
            .pipelines = PipelineArrayList.init(allocator),
            .allocator = allocator,
            .temp_allocations = TempAllocList.init(allocator),
        };
    }

    pub fn deinit(self: *Environment) void {
        // Libera tutte le allocazioni temporanee
        for (self.temp_allocations.items) |alloc| {
            self.allocator.free(alloc);
        }
        self.temp_allocations.deinit();

        self.global_vars.deinit();
        self.local_vars.deinit();
        self.functions.deinit();
        self.stages.deinit();
        self.pipelines.deinit();
    }

    pub fn clearLocal(self: *Environment) void {
        self.local_vars.clear();
    }

    // 🔥 AGGIUNGI QUESTO
    pub fn trackAlloc(self: *Environment, buf: []const u8) !void {
        try self.temp_allocations.append(buf);
    }
};

pub const FunctionDef = struct {
    name: []const u8,
    params: [][]const u8,
    body: []*Ast.Stmt,
};

pub const StageDef = struct {
    name: []const u8,
    params: [][]const u8,
    body: []*Ast.Stmt,
};

pub const PipelineDef = struct {
    name: []const u8,
    stages: [][]const u8,
    parallel: bool,
    timeout: i64,
};

const FunctionArrayList = @import("custom_array_list.zig").CustomArrayList(FunctionDef);
const StageArrayList = @import("custom_array_list.zig").CustomArrayList(StageDef);
const PipelineArrayList = @import("custom_array_list.zig").CustomArrayList(PipelineDef);

pub fn run(bytecode: []Bytecode.Instr, env: *Environment) !void {
    var stack = ValueArrayList.init(env.allocator);
    defer stack.deinit();

    var ip: usize = 0; // instruction pointer

    while (ip < bytecode.len) {
        const instr = bytecode[ip];
        ip += 1;

        switch (instr.op) {
            .Const => {
                try stack.append(.{ .Int = instr.operand.Int });
            },
            .ConstStr => {
                try stack.append(.{ .Str = instr.operand.Str });
            },
            .Add => {
                const b = stack.pop();
                const a = stack.pop();
                const result = a.Int + b.Int;
                try stack.append(.{ .Int = result });
            },
            .Sub => {
                const b = stack.pop();
                const a = stack.pop();
                const result = a.Int - b.Int;
                try stack.append(.{ .Int = result });
            },
            .Mul => {
                const b = stack.pop();
                const a = stack.pop();
                const result = a.Int * b.Int;
                try stack.append(.{ .Int = result });
            },
            .Div => {
                const b = stack.pop();
                const a = stack.pop();
                const result = @divTrunc(a.Int, b.Int);
                try stack.append(.{ .Int = result });
            },

            .Concat => { // ✅ AGGIUNGI QUESTO
                const right = stack.pop();
                const left = stack.pop();

                var buf: [1024]u8 = undefined;
                var fba = std.heap.FixedBufferAllocator.init(&buf);
                const temp_alloc = fba.allocator();

                const left_str = switch (left) {
                    .Int => |i| try std.fmt.allocPrint(temp_alloc, "{d}", .{i}),
                    .Str => |s| s,
                };
                const right_str = switch (right) {
                    .Int => |i| try std.fmt.allocPrint(temp_alloc, "{d}", .{i}),
                    .Str => |s| s,
                };

                const result = try std.fmt.allocPrint(env.allocator, "{s}{s}", .{ left_str, right_str });
                try env.temp_allocations.append(result);
                try stack.append(.{ .Str = result });
            },

            .Get => {
                const var_name = instr.operand.Str;
                const val = try getVar(env, var_name);
                try stack.append(val);
            },
            .SetVar => {
                const var_name = instr.operand.Str;
                const val = stack.pop();
                try setVar(env, var_name, val, false, false);
            },
            .SetConst => {
                const var_name = instr.operand.Str;
                const val = stack.pop();
                try setVar(env, var_name, val, true, false);
            },
            .SetVarGlobal => {
                const var_name = instr.operand.Str;
                const val = stack.pop();
                try setVar(env, var_name, val, false, true);
            },
            .SetConstGlobal => {
                const var_name = instr.operand.Str;
                const val = stack.pop();
                try setVar(env, var_name, val, true, true);
            },
            .Mutate => {
                const var_name = instr.operand.Str;
                const new_val = stack.pop();
                try mutateVar(env, var_name, new_val);
            },
            .Increment => {
                const val = stack.pop();
                const inc_amount = instr.operand.Int;
                const result = val.Int + inc_amount;
                try stack.append(.{ .Int = result });
            },
            .Jump => {
                ip = @intCast(instr.operand.Int);
            },
            .Compare => {
                const right = stack.pop();
                const left = stack.pop();
                const op: Ast.ComparisonOp = @enumFromInt(@as(u8, @intCast(instr.operand.Int)));

                const result: i64 = switch (op) {
                    .Equal => if (left.Int == right.Int) 1 else 0,
                    .NotEqual => if (left.Int != right.Int) 1 else 0,
                    .Greater => if (left.Int > right.Int) 1 else 0,
                    .Less => if (left.Int < right.Int) 1 else 0,
                    .GreaterEq => if (left.Int >= right.Int) 1 else 0,
                    .LessEq => if (left.Int <= right.Int) 1 else 0,
                };

                //std.debug.print("DEBUG Compare: left={d} right={d} op={any} result={d}\n", .{ left.Int, right.Int, op, result });
                try stack.append(.{ .Int = result });
            },
            .JumpIfFalse => {
                const cond = stack.pop();
                //std.debug.print("DEBUG JumpIfFalse: cond={d} jump_to={d} ip={d}\n", .{ cond.Int, instr.operand.Int, ip });
                if (cond.Int == 0) {
                    //std.debug.print("  -> Jumping!\n", .{});
                    ip = @intCast(instr.operand.Int);
                } else {
                    //std.debug.print("  -> Not jumping\n", .{});
                }
            },
            .Choose => {
                const num_arms: usize = @intCast(instr.operand.Int);
                var total_weight: i64 = 0;

                var weights = try env.allocator.alloc(i64, num_arms);
                defer env.allocator.free(weights);

                var i: usize = num_arms;
                while (i > 0) {
                    i -= 1;
                    const weight = stack.pop();
                    weights[i] = weight.Int;
                    total_weight += weight.Int;
                }

                var prng = std.Random.DefaultPrng.init(@intCast(std.time.milliTimestamp()));
                const random = prng.random();
                const roll = random.intRangeAtMost(i64, 0, total_weight - 1);

                var current_sum: i64 = 0;
                var chosen_index: i64 = 0;
                for (weights, 0..) |weight, idx| {
                    current_sum += weight;
                    if (roll < current_sum) {
                        chosen_index = @intCast(idx);
                        break;
                    }
                }

                try stack.append(.{ .Int = chosen_index });
            },
            .Call => {
                const arg = stack.pop();
                const call_name = instr.operand.Str;

                var base_name = call_name;
                var type_hint: []const u8 = "";
                if (std.mem.indexOfScalar(u8, call_name, ':')) |idx| {
                    base_name = call_name[0..idx];
                    type_hint = call_name[idx + 1 ..];
                }

                const Printer = struct {
                    fn printTxt(v: Value) void {
                        switch (v) {
                            .Int => |val| std.debug.print("{d}", .{val}),
                            .Str => |s| std.debug.print("{s}", .{s}),
                        }
                    }

                    fn printAsU8(v: Value) void {
                        switch (v) {
                            .Int => |val| std.debug.print("{d}", .{@as(u8, @intCast(val))}),
                            .Str => |s| std.debug.print("{s}", .{s}),
                        }
                    }

                    fn printAsI8(v: Value) void {
                        switch (v) {
                            .Int => |val| std.debug.print("{d}", .{@as(i8, @intCast(val))}),
                            .Str => |s| std.debug.print("{s}", .{s}),
                        }
                    }

                    fn printAsHex(v: Value) void {
                        switch (v) {
                            .Int => |val| std.debug.print("{x}", .{val}),
                            .Str => |s| std.debug.print("{s}", .{s}),
                        }
                    }
                };

                if (std.mem.eql(u8, base_name, "io.print")) {
                    if (type_hint.len == 0 or std.mem.eql(u8, type_hint, "txt")) {
                        Printer.printTxt(arg);
                    } else if (std.mem.eql(u8, type_hint, "u8")) {
                        Printer.printAsU8(arg);
                    } else if (std.mem.eql(u8, type_hint, "i8")) {
                        Printer.printAsI8(arg);
                    } else if (std.mem.eql(u8, type_hint, "hex")) {
                        Printer.printAsHex(arg);
                    } else {
                        Printer.printTxt(arg);
                    }
                    std.debug.print("\n", .{});
                } else if (std.mem.eql(u8, base_name, "io.warn")) {
                    std.debug.print("{s}", .{YELLOW});
                    if (type_hint.len == 0 or std.mem.eql(u8, type_hint, "txt")) {
                        Printer.printTxt(arg);
                    } else if (std.mem.eql(u8, type_hint, "u8")) {
                        Printer.printAsU8(arg);
                    } else if (std.mem.eql(u8, type_hint, "i8")) {
                        Printer.printAsI8(arg);
                    } else if (std.mem.eql(u8, type_hint, "hex")) {
                        Printer.printAsHex(arg);
                    } else {
                        Printer.printTxt(arg);
                    }
                    std.debug.print("{s}\n", .{RESET});
                } else if (std.mem.eql(u8, base_name, "io.error")) {
                    std.debug.print("{s}", .{RED});
                    if (type_hint.len == 0 or std.mem.eql(u8, type_hint, "txt")) {
                        Printer.printTxt(arg);
                    } else if (std.mem.eql(u8, type_hint, "u8")) {
                        Printer.printAsU8(arg);
                    } else if (std.mem.eql(u8, type_hint, "i8")) {
                        Printer.printAsI8(arg);
                    } else if (std.mem.eql(u8, type_hint, "hex")) {
                        Printer.printAsHex(arg);
                    } else {
                        Printer.printTxt(arg);
                    }
                    std.debug.print("{s}\n", .{RESET});
                    std.process.exit(1);
                } else if (std.mem.eql(u8, base_name, "io.input")) {
                    var placeholder_buf: [256]u8 = undefined;
                    const placeholder: []const u8 = switch (arg) {
                        .Str => |s| s,
                        .Int => |val| std.fmt.bufPrint(&placeholder_buf, "{d}", .{val}) catch "input",
                    };

                    std.debug.print("{s}", .{placeholder});

                    var stdin_buffer: [512]u8 = undefined;
                    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
                    const stdin_interface = &stdin_reader.interface;

                    const line = stdin_interface.takeDelimiterExclusive('\n') catch null;

                    const len: usize = if (line) |l| l.len else 0;
                    const alloc_line = try env.allocator.alloc(u8, len);
                    if (line) |l| @memcpy(alloc_line, l);

                    // Traccia l'allocazione per liberarla in Environment.deinit()
                    try env.temp_allocations.append(alloc_line);

                    try stack.append(.{ .Str = alloc_line });
                }
            },
        }
    }
}

fn getVar(env: *Environment, name: []const u8) !Value {
    for (env.local_vars.items) |v| {
        if (std.mem.eql(u8, v.name, name)) {
            return v.value;
        }
    }
    for (env.global_vars.items) |v| {
        if (std.mem.eql(u8, v.name, name)) {
            return v.value;
        }
    }
    return error.VariableNotFound;
}

fn setVar(env: *Environment, name: []const u8, value: Value, is_const: bool, is_global: bool) !void {
    const vars = if (is_global) &env.global_vars else &env.local_vars;
    try vars.append(.{ .name = name, .value = value, .is_const = is_const });
}
fn findFunction(env: *Environment, name: []const u8) ?FunctionDef {
    for (env.functions.items) |func| {
        if (std.mem.eql(u8, func.name, name)) {
            return func;
        }
    }
    return null;
}

fn findStage(env: *Environment, name: []const u8) ?StageDef {
    for (env.stages.items) |stage| {
        if (std.mem.eql(u8, stage.name, name)) {
            return stage;
        }
    }
    return null;
}

fn findPipeline(env: *Environment, name: []const u8) ?PipelineDef {
    for (env.pipelines.items) |pipeline| {
        if (std.mem.eql(u8, pipeline.name, name)) {
            return pipeline;
        }
    }
    return null;
}
pub fn executeFunction(env: *Environment, func: FunctionDef, args: []Value) InterpreterError!Value {
    // Salva stato locale
    const saved_local_count = env.local_vars.items.len;
    defer {
        // Ripristina stato locale
        while (env.local_vars.items.len > saved_local_count) {
            _ = env.local_vars.pop();
        }
    }

    // Bind parametri
    if (func.params.len != args.len) {
        return error.WrongNumberOfArguments;
    }

    for (func.params, args) |param, arg| {
        try env.local_vars.append(.{
            .name = param,
            .value = arg,
            .is_const = false,
        });
    }

    // Esegui body (devi implementare executeStmt - vedi sotto)
    var return_value: ?Value = null;
    for (func.body) |stmt| {
        if (stmt.* == .Return) {
            return_value = try evaluateExpr(env, stmt.Return);
            break;
        }
        try executeStmtInterpreter(env, stmt);
    }

    return return_value orelse .{ .Int = 0 };
}

// Esecuzione statement in modalità interprete
pub fn executeStmtInterpreter(env: *Environment, stmt: *Ast.Stmt) InterpreterError!void {
    switch (stmt.*) {
        .VarDecl => |var_decl| {
            const val = try evaluateExpr(env, var_decl.value);
            try setVar(env, var_decl.name, val, var_decl.is_const, var_decl.is_global);
        },
        .Mutation => |mutation| {
            const val = try evaluateExpr(env, mutation.value);
            try mutateVar(env, mutation.name, val);
        },
        .ExprStmt => |expr| {
            _ = try evaluateExpr(env, expr);
        },
        .Return => {}, // Gestito in executeFunction
        .FunDecl => |fun| {
            try env.functions.append(.{
                .name = fun.name,
                .params = fun.params,
                .body = fun.body,
            });
        },
        .StageDecl => |stage| {
            try env.stages.append(.{
                .name = stage.name,
                .params = stage.params,
                .body = stage.body,
            });
        },
        .PipelineDecl => |pipeline| {
            try env.pipelines.append(.{
                .name = pipeline.name,
                .stages = pipeline.stages,
                .parallel = pipeline.parallel,
                .timeout = pipeline.timeout,
            });
        },
        else => {},
    }
}

// Valuta espressioni
pub fn evaluateExpr(env: *Environment, expr: *Ast.Expr) InterpreterError!Value {
    switch (expr.*) {
        .Number => |n| return .{ .Int = n },
        .String => |s| return .{ .Str = s },
        .Var => |name| return try getVar(env, name),
        .Add => |bin| {
            const left = try evaluateExpr(env, bin.left);
            const right = try evaluateExpr(env, bin.right);
            return .{ .Int = left.Int + right.Int };
        },
        .Sub => |bin| {
            const left = try evaluateExpr(env, bin.left);
            const right = try evaluateExpr(env, bin.right);
            return .{ .Int = left.Int - right.Int };
        },
        .Mul => |bin| {
            const left = try evaluateExpr(env, bin.left);
            const right = try evaluateExpr(env, bin.right);
            return .{ .Int = left.Int * right.Int };
        },
        .Div => |bin| {
            const left = try evaluateExpr(env, bin.left);
            const right = try evaluateExpr(env, bin.right);
            return .{ .Int = @divTrunc(left.Int, right.Int) };
        },

        // ✅ AGGIUNGI QUESTO CASE per gestire io.print, io.warn, io.input ecc.
        .Call => |call_expr| {
            const arg = try evaluateExpr(env, call_expr.argument);

            var buf: [256]u8 = undefined;
            const call_name = if (call_expr.type_hint.len == 0)
                std.fmt.bufPrint(buf[0..], "{s}.{s}", .{
                    call_expr.library,
                    call_expr.function,
                }) catch "unknown"
            else
                std.fmt.bufPrint(buf[0..], "{s}.{s}:{s}", .{
                    call_expr.library,
                    call_expr.function,
                    call_expr.type_hint,
                }) catch "unknown";

            var base_name = call_name;
            var type_hint: []const u8 = "";
            if (std.mem.indexOfScalar(u8, call_name, ':')) |idx| {
                base_name = call_name[0..idx];
                type_hint = call_name[idx + 1 ..];
            }

            if (std.mem.eql(u8, base_name, "io.print")) {
                switch (arg) {
                    .Int => |val| std.debug.print("{d}\n", .{val}),
                    .Str => |s| std.debug.print("{s}\n", .{s}),
                }
                return .{ .Int = 0 };
            } else if (std.mem.eql(u8, base_name, "io.warn")) {
                std.debug.print("{s}", .{YELLOW});
                switch (arg) {
                    .Int => |val| std.debug.print("{d}", .{val}),
                    .Str => |s| std.debug.print("{s}", .{s}),
                }
                std.debug.print("{s}\n", .{RESET});
                return .{ .Int = 0 };
            } else if (std.mem.eql(u8, base_name, "io.input")) {
                var placeholder_buf: [256]u8 = undefined;
                const placeholder: []const u8 = switch (arg) {
                    .Str => |s| s,
                    .Int => |val| std.fmt.bufPrint(&placeholder_buf, "{d}", .{val}) catch "input",
                };
                std.debug.print("{s}", .{placeholder});

                var stdin_buffer: [512]u8 = undefined;
                var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
                const stdin_interface = &stdin_reader.interface;

                const line = stdin_interface.takeDelimiterExclusive('\n') catch null;

                const len: usize = if (line) |l| l.len else 0;
                const alloc_line = try env.allocator.alloc(u8, len);
                if (line) |l| @memcpy(alloc_line, l);

                try env.temp_allocations.append(alloc_line);

                return .{ .Str = alloc_line };
            }

            return .{ .Int = 0 };
        },

        .FunctionCall => |call| {
            const func = findFunction(env, call.name) orelse return error.FunctionNotFound;

            var args = try env.allocator.alloc(Value, call.args.len);
            defer env.allocator.free(args);

            for (call.args, 0..) |arg_expr, i| {
                args[i] = try evaluateExpr(env, arg_expr);
            }

            return try executeFunction(env, func, args);
        },
        .PipelineCall => |pcall| {
            const pipeline = findPipeline(env, pcall.pipeline_name) orelse return error.PipelineNotFound;
            const input = try evaluateExpr(env, pcall.input);
            return try executePipeline(env, pipeline, input);
        },
        .Concat => |bin| {
            const left = try evaluateExpr(env, bin.left);
            const right = try evaluateExpr(env, bin.right);

            var buf: [1024]u8 = undefined;
            var fba = std.heap.FixedBufferAllocator.init(&buf);
            const temp_alloc = fba.allocator();

            const left_str = switch (left) {
                .Int => |i| try std.fmt.allocPrint(temp_alloc, "{d}", .{i}),
                .Str => |s| s,
            };
            const right_str = switch (right) {
                .Int => |i| try std.fmt.allocPrint(temp_alloc, "{d}", .{i}),
                .Str => |s| s,
            };

            const result = try std.fmt.allocPrint(env.allocator, "{s}{s}", .{ left_str, right_str });
            try env.temp_allocations.append(result);

            return .{ .Str = result };
        },
        else => return .{ .Int = 0 },
    }
}

// Esecuzione pipeline
fn executePipeline(env: *Environment, pipeline: PipelineDef, input: Value) InterpreterError!Value {
    var result = input;

    std.debug.print("DEBUG Pipeline: starting with input={any}\n", .{result});
    std.debug.print("DEBUG Pipeline: number of stages={d}\n", .{pipeline.stages.len});

    for (pipeline.stages) |stage_name| {
        std.debug.print("DEBUG Pipeline: looking for stage '{s}'\n", .{stage_name});

        const stage = findStage(env, stage_name) orelse {
            std.debug.print("DEBUG Pipeline: stage '{s}' NOT FOUND!\n", .{stage_name});
            return error.StageNotFound;
        };

        std.debug.print("DEBUG Pipeline: found stage '{s}', executing with input={any}\n", .{ stage_name, result });

        var args = [_]Value{result};
        result = try executeFunction(env, .{
            .name = stage.name,
            .params = stage.params,
            .body = stage.body,
        }, args[0..]);
    }

    return result;
}

fn mutateVar(env: *Environment, name: []const u8, new_value: Value) InterpreterError!void {
    for (env.local_vars.items) |*v| {
        if (std.mem.eql(u8, v.name, name)) {
            if (v.is_const) {
                return error.CannotMutateConstant;
            }
            v.value = new_value;
            return;
        }
    }
    for (env.global_vars.items) |*v| {
        if (std.mem.eql(u8, v.name, name)) {
            if (v.is_const) {
                return error.CannotMutateConstant;
            }
            v.value = new_value;
            return;
        }
    }
    return error.VariableNotFound;
}
