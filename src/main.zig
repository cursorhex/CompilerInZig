const std = @import("std");

const lex = @import("lexer.zig").lex;
const parser = @import("parser.zig");
const codegen = @import("codegen.zig");
const run = @import("vm.zig").run;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var input_file: ?[]const u8 = null;
    var bytecode_file: ?[]const u8 = null;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--i")) {
            i += 1;
            if (i < args.len) {
                input_file = args[i];
            }
        } else if (std.mem.eql(u8, args[i], "--bytecode")) {
            i += 1;
            if (i < args.len) {
                bytecode_file = args[i];
            }
        }
    }

    const default_src: []const u8 =
        \\var(x): 10
        \\io.print (x)
    ;

    var src: []u8 = undefined;
    if (input_file) |file_path| {
        var file = try std.fs.cwd().openFile(file_path, .{});
        defer file.close();
        const file_size = try file.getEndPos();
        src = try allocator.alloc(u8, file_size);
        _ = try file.read(src);
    } else {
        src = try allocator.alloc(u8, default_src.len);
        @memcpy(src, default_src);
    }
    defer allocator.free(src);

    const tokens = try lex(src, allocator);
    defer allocator.free(tokens);

    const ast = try parser.parse(tokens, allocator);
    defer parser.freeAst(ast, allocator);

    const bytecode = try codegen.codegen(ast, allocator);
    defer codegen.freeBytecode(bytecode, allocator);

    if (bytecode_file) |file_path| {
        var file = try std.fs.cwd().createFile(file_path, .{});
        defer file.close();
        for (bytecode) |instr| {
            const str = try std.fmt.allocPrint(allocator, "{any}\n", .{instr});
            defer allocator.free(str);
            try file.writeAll(str);
        }
    }

    // Catch runtime errors and exit gracefully without stack trace
    run(bytecode, allocator) catch |err| {
        // Error message already printed by vm.zig, just exit
        switch (err) {
            error.CannotMutateConstant,
            error.VariableNotFound => std.process.exit(1),
            else => return err, // Propagate unexpected errors
        }
    };
}
