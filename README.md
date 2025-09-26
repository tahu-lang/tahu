# Tahu Programming Language

## Quick Examples
### Hello World
```tahu
fn main() {
    println("Hello, World!")
}
```

### Variables and Types
```tahu
fn main() {
    // Immutable variable
    val name: string = "Tahu"

    // Mutable variable
    var age: int = 5
    age = 6 // Reassigning a mutable variable

    var port: int = 8080
    var is_running: bool = true
    
    // Type inference
    var message = "Server running on port ${port}"
    print(message)
}
```

## Data Types
### Primitive
```tahu
i8;
i16;
i32;
i64;
isize;

u8;
u16;
u32;
u64;
usize;

f32;
f64;

char;
bool;
unit;
```

### Composite
### array
```tahu
var arr = [1, 2, 3];

arr[0];
```

### struct
```tahu
struct Person {
    name: string
    age: int
}

var person = Person {
    name: "Tahu",
    age: 5
}
```

## Control Flow
### If-Else
```tahu
fn main() {
    var condition = true;
    if condition {
        println("Condition is true")
    } else {
        println("Condition is false")
    }
}
```
### While Loop
```tahu
fn main() {
    var i = 0
    while i < 5 {
        println(i)
        i += 1
    }
}
```

## Functions
### Function Definition
```tahu
fn add(a: int, b: int) int {
    return a + b
}

// fn with public visibility can access cross file
// math.tahu
pub fn sub(a: int, b: int) int {
    return a - b
}

// main.tahu
import math

fn main() {
    var result = sub(3, 4)
    print(result)
}
```

### Function Calling
```tahu
fn main() {
    var result = add(3, 4)
    print(result)
}
```

### Extern Function
```tahu
extern fn prints(msg: string, len: usize) unit;

fn print(msg: string) unit {
    prints(msg, msg.len)
}
```

## Installation
`TODO`


## Getting Started
`TODO`