const std = @import("std");
const Bytecode = @import("bytecode.zig");
const CustomArrayList = @import("custom_array_list.zig").CustomArrayList;

const Value = union(enum) {
    Int: i64,
    Str: []const u8,
};

const Variable = struct {
    value: Value,
    is_const: bool,
    is_global: bool,
};

const ValueArrayList = CustomArrayList(Value);

// ANSI color codes
const RED = "\x1b[31m";
const RESET = "\x1b[0m";
const BOLD = "\x1b[1m";

pub const Environment = struct {
    variables: std.StringHashMap(Variable),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Environment {
        return .{
            .variables = std.StringHashMap(Variable).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Environment) void {
        self.variables.deinit();
    }

    // NUOVO: Pulisce tutte le variabili NON globali
    pub fn clearLocalVariables(self: *Environment) !void {
        const StringList = CustomArrayList([]const u8);
        var keys_list = StringList.init(self.allocator);
        defer keys_list.deinit();

        // Trova tutte le chiavi da rimuovere
        var it = self.variables.iterator();
        while (it.next()) |entry| {
            if (!entry.value_ptr.is_global) {
                try keys_list.append(entry.key_ptr.*);
            }
        }

        // Rimuovi le chiavi
        for (keys_list.items) |key| {
            _ = self.variables.remove(key);
        }
    }
};

pub fn run(bytecode: []const Bytecode.Instr, allocator: std.mem.Allocator) !void {
    var env = Environment.init(allocator);
    defer env.deinit();
    try runWithEnv(bytecode, &env);
}

pub fn runWithEnv(bytecode: []const Bytecode.Instr, env: *Environment) !void {
    var stack = ValueArrayList.init(env.allocator);
    defer stack.deinit();

    for (bytecode) |instr| {
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
            .Get => {
                const name = instr.operand.Str;
                if (env.variables.get(name)) |variable| {
                    try stack.append(variable.value);
                } else {
                    std.debug.print("{s}{s}Error:{s} Variable '{s}' not found\n", .{ BOLD, RED, RESET, name });
                    return error.VariableNotFound;
                }
            },
            .SetVar => {
                const value = stack.pop();
                const name = instr.operand.Str;
                try env.variables.put(name, .{ .value = value, .is_const = false, .is_global = false });
            },
            .SetConst => {
                const value = stack.pop();
                const name = instr.operand.Str;
                try env.variables.put(name, .{ .value = value, .is_const = true, .is_global = false });
            },
            .SetVarGlobal => {
                const value = stack.pop();
                const name = instr.operand.Str;
                try env.variables.put(name, .{ .value = value, .is_const = false, .is_global = true });
            },
            .SetConstGlobal => {
                const value = stack.pop();
                const name = instr.operand.Str;
                try env.variables.put(name, .{ .value = value, .is_const = true, .is_global = true });
            },
            .Mutate => {
                const value = stack.pop();
                const name = instr.operand.Str;

                if (env.variables.get(name)) |variable| {
                    if (variable.is_const) {
                        std.debug.print("   {s}{s}Error: Cannot mutate constant '{s}' {s}\n", .{ BOLD, RED, name, RESET });
                        return error.CannotMutateConstant;
                    }
                    try env.variables.put(name, .{ .value = value, .is_const = false, .is_global = variable.is_global });
                } else {
                    std.debug.print("{s}{s}Error:{s} Variable '{s}' not found\n", .{ BOLD, RED, RESET, name });
                    return error.VariableNotFound;
                }
            },
            .Increment => {
                const val = stack.pop();
                const result = val.Int + instr.operand.Int;
                try stack.append(.{ .Int = result });
            },
            .Call => {
                const arg = stack.pop();
                const call_name = instr.operand.Str;

                // Controlla suffix :tipo
                var base_name = call_name;
                var type_hint: []const u8 = "";
                if (std.mem.indexOfScalar(u8, call_name, ':')) |idx| {
                    base_name = call_name[0..idx];
                    type_hint = call_name[idx + 1 ..];
                }

                if (std.mem.eql(u8, base_name, "io.print")) {
                    // type_hint decide come stampare
                    if (type_hint.len == 0 or std.mem.eql(u8, type_hint, "txt")) {
                        // comportamento attuale
                        switch (arg) {
                            .Int => |i| std.debug.print("{d}\n", .{i}),
                            .Str => |s| std.debug.print("{s}\n", .{s}),
                        }
                    } else if (std.mem.eql(u8, type_hint, "u8")) {
                        switch (arg) {
                            .Int => |i| std.debug.print("{d}\n", .{@as(u8, @intCast(i))}),
                            .Str => |s| std.debug.print("{s}\n", .{s}),
                        }
                    } else if (std.mem.eql(u8, type_hint, "i8")) {
                        switch (arg) {
                            .Int => |i| std.debug.print("{d}\n", .{@as(i8, @intCast(i))}),
                            .Str => |s| std.debug.print("{s}\n", .{s}),
                        }
                    } else {
                        // fallback: stampa come prima
                        switch (arg) {
                            .Int => |i| std.debug.print("{d}\n", .{i}),
                            .Str => |s| std.debug.print("{s}\n", .{s}),
                        }
                    }
                }
            },
        }
    }
}
