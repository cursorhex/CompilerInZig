const std = @import("std");
const Bytecode = @import("bytecode.zig");
const Ast = @import("ast.zig");
const InstrArrayList = @import("custom_array_list.zig").CustomArrayList(Bytecode.Instr);
const UsizeArrayList = @import("custom_array_list.zig").CustomArrayList(usize);

pub fn codegen(stmts: []*Ast.Stmt, allocator: std.mem.Allocator) ![]Bytecode.Instr {
    var instructions = InstrArrayList.init(allocator);
    defer instructions.deinit();

    for (stmts) |stmt| {
        try genStmt(stmt, &instructions);
    }

    return instructions.toOwnedSlice();
}

pub fn genExpr(expr: *Ast.Expr, out: *InstrArrayList) !void {
    switch (expr.*) {
        .Number => |n| try out.append(.{ .op = .Const, .operand = .{ .Int = n } }),
        .String => |s| try out.append(.{ .op = .ConstStr, .operand = .{ .Str = s } }),
        .Var => |v| try out.append(.{ .op = .Get, .operand = .{ .Str = v } }),
        .Add => |a| {
            try genExpr(a.left, out);
            try genExpr(a.right, out);
            try out.append(.{ .op = .Add, .operand = .{ .Int = 0 } });
        },
        .Sub => |s| {
            try genExpr(s.left, out);
            try genExpr(s.right, out);
            try out.append(.{ .op = .Sub, .operand = .{ .Int = 0 } });
        },
        .Mul => |m| {
            try genExpr(m.left, out);
            try genExpr(m.right, out);
            try out.append(.{ .op = .Mul, .operand = .{ .Int = 0 } });
        },
        .Div => |d| {
            try genExpr(d.left, out);
            try genExpr(d.right, out);
            try out.append(.{ .op = .Div, .operand = .{ .Int = 0 } });
        },
        .Concat => |c| {
            try genExpr(c.left, out);
            try genExpr(c.right, out);
            try out.append(.{ .op = .Concat, .operand = .{ .Int = 0 } });
        },
        .Increment => |inner| {
            try genExpr(inner, out);
            try out.append(.{ .op = .Increment, .operand = .{ .Int = 1 } });
        },
        .Call => |call_expr| {
            try genExpr(call_expr.argument, out);

            const call_name = if (call_expr.type_hint.len == 0)
                try std.fmt.allocPrint(out.allocator, "{s}.{s}", .{
                    call_expr.library,
                    call_expr.function,
                })
            else
                try std.fmt.allocPrint(out.allocator, "{s}.{s}:{s}", .{
                    call_expr.library,
                    call_expr.function,
                    call_expr.type_hint,
                });

            try out.append(.{ .op = .Call, .operand = .{ .Str = call_name } });
        },
        .Comparison => |comp| {
            try genExpr(comp.left, out);
            try genExpr(comp.right, out);
            const op_code: u8 = @intFromEnum(comp.op);
            try out.append(.{ .op = .Compare, .operand = .{ .Int = op_code } });
        },

        .Choose => |arms| {
            const num_arms = arms.len;

            // Push i pesi
            for (arms) |arm| {
                try out.append(.{ .op = .Const, .operand = .{ .Int = arm.weight } });
            }

            // Choose ritorna indice
            try out.append(.{ .op = .Choose, .operand = .{ .Int = @intCast(num_arms) } });

            // Salva indice in temp var
            try out.append(.{ .op = .SetVar, .operand = .{ .Str = "__choose_idx__" } });

            // Jump table
            var jump_positions = UsizeArrayList.init(out.allocator);
            defer jump_positions.deinit();

            for (arms, 0..) |arm, i| {
                try out.append(.{ .op = .Get, .operand = .{ .Str = "__choose_idx__" } });
                try out.append(.{ .op = .Const, .operand = .{ .Int = @intCast(i) } });
                try out.append(.{ .op = .Compare, .operand = .{ .Int = @intFromEnum(Ast.ComparisonOp.Equal) } });

                const jump_pos = out.items.len;
                try out.append(.{ .op = .JumpIfFalse, .operand = .{ .Int = 0 } });

                try genExpr(arm.value, out);

                try jump_positions.append(out.items.len);
                try out.append(.{ .op = .Jump, .operand = .{ .Int = 0 } });

                out.items[jump_pos].operand = .{ .Int = @intCast(out.items.len) };
            }

            const end_pos: i64 = @intCast(out.items.len);
            for (jump_positions.items) |pos| {
                out.items[pos].operand = .{ .Int = end_pos };
            }
        },
    }
}

pub fn genStmt(stmt: *Ast.Stmt, out: *InstrArrayList) !void {
    switch (stmt.*) {
        .VarDecl => |var_decl| {
            try genExpr(var_decl.value, out);

            if (var_decl.is_const) {
                if (var_decl.is_global) {
                    try out.append(.{ .op = .SetConstGlobal, .operand = .{ .Str = var_decl.name } });
                } else {
                    try out.append(.{ .op = .SetConst, .operand = .{ .Str = var_decl.name } });
                }
            } else {
                if (var_decl.is_global) {
                    try out.append(.{ .op = .SetVarGlobal, .operand = .{ .Str = var_decl.name } });
                } else {
                    try out.append(.{ .op = .SetVar, .operand = .{ .Str = var_decl.name } });
                }
            }
        },
        .Mutation => |mutation| {
            try genExpr(mutation.value, out);
            try out.append(.{ .op = .Mutate, .operand = .{ .Str = mutation.name } });
        },

        .ExprStmt => |expr| {
            try genExpr(expr, out);
        },
        .BytecodeExec => |block| {
            const decoded = try Bytecode.decodeCompact(block.data, out.allocator);
            defer out.allocator.free(decoded);

            for (decoded) |instr| {
                try out.append(instr);
            }
        },
        .If => |if_stmt| {
            // Genera condizione
            try genExpr(if_stmt.condition, out);

            // Salva posizione per JumpIfFalse
            const jump_if_false_pos = out.items.len;
            try out.append(.{ .op = .JumpIfFalse, .operand = .{ .Int = 0 } }); // placeholder

            // Then block
            for (if_stmt.then_block) |s| {
                try genStmt(s, out);
            }

            // Jump alla fine dopo then
            const jump_end_pos = out.items.len;
            try out.append(.{ .op = .Jump, .operand = .{ .Int = 0 } }); // placeholder

            // Aggiorna JumpIfFalse per saltare al prossimo branch
            out.items[jump_if_false_pos].operand = .{ .Int = @intCast(out.items.len) };

            // Elseif branches
            var previous_jump_positions = UsizeArrayList.init(out.allocator);
            defer previous_jump_positions.deinit();

            for (if_stmt.elseif_branches) |branch| {
                try genExpr(branch.condition, out);
                const elif_jump_pos = out.items.len;
                try out.append(.{ .op = .JumpIfFalse, .operand = .{ .Int = 0 } });

                for (branch.block) |s| {
                    try genStmt(s, out);
                }

                try previous_jump_positions.append(out.items.len);
                try out.append(.{ .op = .Jump, .operand = .{ .Int = 0 } });

                out.items[elif_jump_pos].operand = .{ .Int = @intCast(out.items.len) };
            }

            // Else block
            for (if_stmt.else_block) |s| {
                try genStmt(s, out);
            }

            // Patch tutti i jump alla fine
            const end_pos: i64 = @intCast(out.items.len);
            out.items[jump_end_pos].operand = .{ .Int = end_pos };
            for (previous_jump_positions.items) |pos| {
                out.items[pos].operand = .{ .Int = end_pos };
            }
        },
    }
}
fn genChooseStmt(arms: []Ast.ChoiceArm, out: *InstrArrayList) !void {
    const num_arms = arms.len;

    // Push i pesi
    for (arms) |arm| {
        try out.append(.{ .op = .Const, .operand = .{ .Int = arm.weight } });
    }

    // Choose ritorna indice
    try out.append(.{ .op = .Choose, .operand = .{ .Int = @intCast(num_arms) } });

    // Salva indice in temp var
    try out.append(.{ .op = .SetVar, .operand = .{ .Str = "__choose_idx__" } });

    // Jump table per ogni branch
    var jump_positions = UsizeArrayList.init(out.allocator);
    defer jump_positions.deinit();

    for (arms, 0..) |arm, i| {
        // Leggi indice e confronta
        try out.append(.{ .op = .Get, .operand = .{ .Str = "__choose_idx__" } });
        try out.append(.{ .op = .Const, .operand = .{ .Int = @intCast(i) } });
        try out.append(.{ .op = .Compare, .operand = .{ .Int = @intFromEnum(Ast.ComparisonOp.Equal) } });

        const jump_pos = out.items.len;
        try out.append(.{ .op = .JumpIfFalse, .operand = .{ .Int = 0 } });

        // Branch code
        try genExpr(arm.value, out);

        try jump_positions.append(out.items.len);
        try out.append(.{ .op = .Jump, .operand = .{ .Int = 0 } });

        // Patch JumpIfFalse
        out.items[jump_pos].operand = .{ .Int = @intCast(out.items.len) };
    }

    // Patch tutti i Jump alla fine
    const end_pos: i64 = @intCast(out.items.len);
    for (jump_positions.items) |pos| {
        out.items[pos].operand = .{ .Int = end_pos };
    }
}
pub fn freeBytecode(bytecode: []Bytecode.Instr, allocator: std.mem.Allocator) void {
    allocator.free(bytecode);
}
