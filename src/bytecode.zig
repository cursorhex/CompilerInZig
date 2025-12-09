const std = @import("std");
const CustomArrayList = @import("custom_array_list.zig").CustomArrayList;

pub const OpCode = enum(u8) {
    Const = 0x01,
    ConstStr = 0x02,
    Add = 0x03,
    Sub = 0x04,
    Mul = 0x05,
    Div = 0x06,
    Get = 0x07,
    SetVar = 0x08,
    SetConst = 0x09,
    SetVarGlobal = 0x0A,
    SetConstGlobal = 0x0B,
    Mutate = 0x0C,
    Increment = 0x0D,
    Call = 0x0E,
    Concat = 0x0F,
    Jump = 0x10, // salta a offset
    JumpIfFalse = 0x11, // salta se falso
    Compare = 0x12, // confronto
    Choose = 0x13, // scelta random pesata
};

pub const Operand = union(enum) {
    Int: i64,
    Str: []const u8,
};

pub const Instr = struct {
    op: OpCode,
    operand: Operand,
};

pub fn encodeCompact(instructions: []const Instr, allocator: std.mem.Allocator) ![]u8 {
    var buffer = CustomArrayList(u8).init(allocator);
    defer buffer.deinit();

    for (instructions) |instr| {
        try buffer.append(@intFromEnum(instr.op));

        switch (instr.operand) {
            .Int => |val| {
                const bytes = std.mem.toBytes(val);
                for (bytes) |b| {
                    try buffer.append(b);
                }
            },
            .Str => |str| {
                const len: u16 = @intCast(str.len);
                const len_bytes = std.mem.toBytes(len);
                for (len_bytes) |b| {
                    try buffer.append(b);
                }
                for (str) |c| {
                    try buffer.append(c);
                }
            },
        }
    }

    return buffer.toOwnedSlice();
}

pub fn decodeCompact(data: []const u8, allocator: std.mem.Allocator) ![]Instr {
    var result = CustomArrayList(Instr).init(allocator);
    defer result.deinit();

    var i: usize = 0;
    while (i < data.len) {
        const op: OpCode = @enumFromInt(data[i]);
        i += 1;

        const operand: Operand = switch (op) {
            .Const, .Increment => blk: {
                var val: i64 = undefined;
                const bytes = data[i .. i + 8];
                @memcpy(std.mem.asBytes(&val), bytes);
                i += 8;
                break :blk .{ .Int = val };
            },
            .ConstStr, .Get, .SetVar, .SetConst, .SetVarGlobal, .SetConstGlobal, .Mutate, .Call => blk: {
                var len: u16 = undefined;
                const len_bytes = data[i .. i + 2];
                @memcpy(std.mem.asBytes(&len), len_bytes);
                i += 2;
                const str = data[i .. i + len];
                i += len;
                break :blk .{ .Str = str };
            },
            .Add, .Sub, .Mul, .Div, .Concat => .{ .Int = 0 },
            .Jump, .JumpIfFalse, .Compare, .Choose => blk: {
                var val: i64 = undefined;
                const bytes = data[i .. i + 8];
                @memcpy(std.mem.asBytes(&val), bytes);
                i += 8;
                break :blk .{ .Int = val };
            },
        };

        try result.append(.{ .op = op, .operand = operand });
    }

    return result.toOwnedSlice();
}
