const std = @import("std");

pub fn CustomArrayList(comptime T: type) type {
    return struct {
        items: []T,
        capacity: usize,
        allocator: std.mem.Allocator,

        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) Self {
            return Self{
                .items = &[_]T{},
                .capacity = 0,
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            if (self.capacity > 0) {
                self.allocator.free(self.items.ptr[0..self.capacity]);
            }
        }

        pub fn clear(self: *Self) void {
            self.items.len = 0;
        }

        pub fn append(self: *Self, item: T) !void {
            if (self.items.len >= self.capacity) {
                const new_capacity = if (self.capacity == 0) 8 else self.capacity * 2;
                const new_memory = try self.allocator.alloc(T, new_capacity);
                if (self.items.len > 0) {
                    @memcpy(new_memory[0..self.items.len], self.items);
                }
                if (self.capacity > 0) {
                    self.allocator.free(self.items.ptr[0..self.capacity]);
                }
                self.items.ptr = new_memory.ptr;
                self.capacity = new_capacity;
            }
            self.items.len += 1;
            self.items[self.items.len - 1] = item;
        }

        pub fn pop(self: *Self) T {
            const item = self.items[self.items.len - 1];
            self.items.len -= 1;
            return item;
        }

        pub fn toOwnedSlice(self: *Self) ![]T {
            const result = try self.allocator.alloc(T, self.items.len);
            @memcpy(result, self.items);
            return result;
        }
    };
}
