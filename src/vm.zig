const std = @import("std");
const Bytecode = @import("bytecode.zig");
const Ast = @import("ast.zig");

const RED = "\x1b[31m";
const YELLOW = "\x1b[33m";
const RESET = "\x1b[0m";

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
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Environment {
        return Environment{
            .global_vars = VarArrayList.init(allocator),
            .local_vars = VarArrayList.init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Environment) void {
        self.global_vars.deinit();
        self.local_vars.deinit();
    }

    pub fn clearLocal(self: *Environment) void {
        self.local_vars.clear();
    }
};

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
            .Concat => {
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

fn mutateVar(env: *Environment, name: []const u8, new_value: Value) !void {
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
