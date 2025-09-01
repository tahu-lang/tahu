#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOp {
    /// The `=` operator (basic assignment)
    ///
    /// Example: `x = 5`
    Assign,
    
    /// The `+=` operator (addition assignment)
    ///
    /// Example: `x += 3` (equivalent to `x = x + 3`)
    AddAssign,
    
    /// The `-=` operator (subtraction assignment)
    ///
    /// Example: `x -= 2` (equivalent to `x = x - 2`)
    SubAssign,
    
    /// The `*=` operator (multiplication assignment)
    ///
    /// Example: `x *= 4` (equivalent to `x = x * 4`)
    MulAssign,
    
    /// The `/=` operator (division assignment)
    ///
    /// Example: `x /= 2` (equivalent to `x = x / 2`)
    DivAssign,
    
    /// The `%=` operator (modulus/remainder assignment)
    ///
    /// Example: `x %= 3` (equivalent to `x = x % 3`)
    RemAssign,
    
    /// The `&=` operator (bitwise AND assignment)
    ///
    /// Example: `x &= 0b1010` (equivalent to `x = x & 0b1010`)
    BitAndAssign,
    
    /// The `|=` operator (bitwise OR assignment)
    ///
    /// Example: `x |= 0b0101` (equivalent to `x = x | 0b0101`)
    BitOrAssign,
    
    /// The `^=` operator (bitwise XOR assignment)
    ///
    /// Example: `x ^= 0b1100` (equivalent to `x = x ^ 0b1100`)
    BitXorAssign,
    
    /// The `<<=` operator (left shift assignment)
    ///
    /// Example: `x <<= 2` (equivalent to `x = x << 2`)
    ShlAssign,
    
    /// The `>>=` operator (right shift assignment)
    ///
    /// Example: `x >>= 1` (equivalent to `x = x >> 1`)
    ShrAssign,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    /// The `+` operator (addition)
    Add,
    /// The `-` operator (subtraction)
    Sub,
    /// The `*` operator (multiplication)
    Mul,
    /// The `/` operator (division)
    Div,
    /// The `%` operator (modulus)
    Rem,
    /// The `&&` operator (logical and)
    And,
    /// The `||` operator (logical or)
    Or,
    /// The `^` operator (bitwise xor)
    BitXor,
    /// The `&` operator (bitwise and)
    BitAnd,
    /// The `|` operator (bitwise or)
    BitOr,
    /// The `<<` operator (shift left)
    Shl,
    /// The `>>` operator (shift right)
    Shr,
    /// The `==` operator (equality)
    Eq,
    /// The `<` operator (less than)
    Lt,
    /// The `<=` operator (less than or equal to)
    Le,
    /// The `!=` operator (not equal to)
    Ne,
    /// The `>=` operator (greater than or equal to)
    Ge,
    /// The `>` operator (greater than)
    Gt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    /// the `-` operator for decrement
    Minus,
    /// the `+` operator for increment
    Plus,
    /// The `!` operator for logical inversion
    Not,
    /// The `~` operator for bitwise inversion
    BitNot,
    /// the `i++` opeartor for increment
    PostIncrement,
    /// the `i--` opeartor for decrement
    PostDecrement,
    /// The `++i` increment
    PreIncrement,
    /// The `--i` decrement
    PreDecrement,
    /// *x
    Deref,
    /// &x
    AddressOf,
}