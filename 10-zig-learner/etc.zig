/// slice pointer example
///
var x = [_]u32{ 1, 2, 3, 4, 5 };
var x2: []u32 = &x;
var ptr = &x2;
std.log.warn("{}", .{@typeName(@TypeOf(ptr))});
std.log.warn("len = {}", .{ptr.len});

var ptr2 = @ptrCast(*c_void, ptr);
std.log.warn("{}", .{@typeName(@TypeOf(ptr2))});

var ptr3 = @ptrCast(*[]u32, ptr2);
std.log.warn("{}", .{@typeName(@TypeOf(ptr3))});
std.log.warn("len = {}", .{ptr3.len});

/// Generics example
const std = @import("std");
const math = std.math;

const Rect = struct {
    const Self = @This();
    width: f64,
    height: f64,

    fn area(self: Self) f64 {
        return self.width * self.height;
    }

    fn perim(self: Self) f64 {
        return 2 * self.width + 2 * self.height;
    }
};

const Circle = struct {
    const Self = @This();
    radius: f64,

    fn area(self: Self) f64 {
        return math.pi * self.radius * self.radius;
    }

    fn perim(self: Self) f64 {
        return 2 * math.pi * self.radius;
    }
};

fn measure(comptime T: type, geometry: T) void {
    std.debug.print("{}\n{}\n{}\n", .{
        geometry,
        geometry.area(),
        geometry.perim(),
    });
}

pub fn main() anyerror!void {
    var r = Rect{ .width = 3, .height = 4 };
    var c = Circle{ .radius = 5 };

    measure(Rect, r);
    measure(Circle, c);
}

// from https://www.nmichaels.org/zig/interfaces.html
/// Generic iterator interface. Call next to get the next element or
/// null if the iterator's exhausted. Call reset to start iteration
/// over from the beginning.
pub fn Iterator(comptime T: type) type {
    return struct {
        const Self = @This();
        nextFn: fn (self: *Self) ?T,
        resetFn: fn (self: *Self) void,
        pub fn next(self: *Self) ?T {
            return self.nextFn(self);
        }
        pub fn reset(self: *Self) void {
            return self.resetFn(self);
        }
    };
}

/// Half-open range type. Call next() on its iterator to get values
/// out. Example usage:
///
/// var range = try Range(u32).init(0, 10, 1);
/// var iter = &range.iterator;
/// while (iter.next()) |n| {
///     std.debug.warn("{}\n", .{n});
/// }
pub fn Range(comptime T: type) type {
    return struct {
        iterator: Iterator(T),
        next_val: T,
        start: T,
        step: T,
        end: T,
        const Self = @This();

        /// Return the next element in the range, or null if the range
        /// has been exhausted.
        pub fn next(iterator: *Iterator(T)) ?T {
            const self = @fieldParentPtr(Self, "iterator", iterator);
            const rv = self.next_val;
            if (self.step < 0) {
                if (rv <= self.end) {
                    return null;
                }
            } else {
                if (rv >= self.end) {
                    return null;
                }
            }

            self.next_val += self.step;
            return rv;
        }

        /// Reset the range back to its start.
        pub fn reset(iterator: *Iterator(T)) void {
            const self = @fieldParentPtr(Self, "iterator", iterator);
            self.next_val = self.start;
        }

        /// Initialize. Returns error if step size is invalid.
        pub fn init(start: T, end: T, step: T) !Self {
            if (step == 0) {
                return error.ZeroStepSize;
            }
            return Self{
                .next_val = start,
                .start = start,
                .end = end,
                .step = step,
                .iterator = Iterator(T){
                    .nextFn = next,
                    .resetFn = reset,
                },
            };
        }
    };
}

const std = @import("std");

const testing = std.testing;
test "range ascend" {
    var range = try Range(u32).init(0, 10, 1);
    var iter = &range.iterator;
    var correct: u32 = 0;
    while (iter.next()) |n| {
        testing.expectEqual(correct, n);
        correct += 1;
    }
    testing.expectEqual(correct, 10);
    testing.expectEqual(iter.next(), null);
}

test "range descend" {
    var range = try Range(i32).init(10, 0, -1);
    var iter = &range.iterator;
    var correct: i32 = 10;
    while (iter.next()) |n| {
        testing.expectEqual(correct, n);
        correct -= 1;
    }
    testing.expectEqual(correct, 0);
    testing.expectEqual(iter.next(), null);
}

test "range skip" {
    var range = try Range(u32).init(0, 10, 2);
    var iter = &range.iterator;
    var correct: u32 = 0;
    while (iter.next()) |n| {
        testing.expectEqual(correct, n);
        correct += 2;
    }
    testing.expectEqual(correct, 10);
    testing.expectEqual(iter.next(), null);
}

test "range runtime" {
    var start: u32 = 0;
    while (start < 10) : (start += 1) {
        var range = try Range(u32).init(start, 10, 1);
        var iter = &range.iterator;
        var correct: u32 = start;
        while (iter.next()) |n| {
            testing.expectEqual(correct, n);
            correct += 1;
        }
        testing.expectEqual(correct, 10);
        testing.expectEqual(iter.next(), null);
    }
}

