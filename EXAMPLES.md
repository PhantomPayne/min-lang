# Example Min Programs

This file contains example programs in the Min language to validate the syntax and demonstrate features.

## Example 1: Simple Arithmetic

```min
fn add:
  """Adds two integers."""
  a: int
  b: int
-> int {
  return a + b
}

fn multiply:
  """Multiplies two integers."""
  x: int
  y: int
-> int {
  return x * y
}

fn main:
  """Main entry point."""
-> int {
  let sum = add(a: 5, b: 3);
  let product = multiply(x: sum, y: 2);
  return product
}
```

## Example 2: Structs and Type Aliases

```min
type Point = {
  x: int,
  y: int
}

type Rectangle = {
  top_left: Point,
  width: int,
  height: int
}

fn make_point:
  """Creates a point at the given coordinates."""
  x: int
  y: int
-> Point {
  return { x: x, y: y }
}

fn area:
  """Calculates the area of a rectangle."""
  rect: Rectangle
-> int {
  return rect.width * rect.height
}

fn main:
  """Demonstrates struct usage."""
-> int {
  let origin = make_point(x: 0, y: 0);
  let rect = {
    top_left: origin,
    width: 10,
    height: 20
  };
  return area(rect: rect)
}
```

## Example 3: Conditionals

```min
fn abs:
  """Returns the absolute value of a number."""
  x: int
-> int {
  return if x < 0 { -x } else { x }
}

fn sign:
  """Returns the sign of a number."""
  x: int
-> string {
  if x > 0 {
    return "positive"
  } else {
    if x < 0 {
      return "negative"
    } else {
      return "zero"
    }
  }
}

fn clamp:
  """Clamps a value between min and max."""
  value: int
  min: int
  max: int
-> int {
  if value < min {
    return min
  } else {
    if value > max {
      return max
    } else {
      return value
    }
  }
}
```

## Example 4: String Interpolation

```min
fn greet:
  """Greets a person with a formatted message."""
  name: string
  age: int
-> string {
  return "Hello, {name}! You are {age} years old."
}

fn format_point:
  """Formats a point as a string."""
  x: int
  y: int
-> string {
  return "Point({x}, {y})"
}

fn describe_rectangle:
  """Describes a rectangle."""
  width: int
  height: int
-> string {
  let area = width * height;
  return "Rectangle {width}x{height} with area {area}"
}
```

## Example 5: Blocks and Scoping

```min
fn compute:
  """Demonstrates block expressions and scoping."""
  x: int
-> int {
  let doubled = {
    let temp = x * 2;
    temp + 1
  };
  
  let result = {
    let a = doubled;
    let b = x;
    a + b
  };
  
  return result
}

fn complex_calculation:
  """Shows nested blocks."""
  input: int
-> int {
  let step1 = {
    let a = input * 2;
    let b = a + 10;
    b
  };
  
  let step2 = {
    let c = step1 - 5;
    c * 3
  };
  
  return step1 + step2
}
```

## Example 6: Attributes and Documentation

```min
fn legacy_add:
  @deprecated: "Use add_v2 instead"
  @since: v1.0
  """
  Legacy addition function.
  
  This function is deprecated and will be removed in v2.0.
  Use add_v2 for better performance.
  """
  a: int
  b: int
-> int {
  return a + b
}

fn add_v2:
  @pure
  @since: v1.5
  """
  Improved addition function.
  
  ## Performance
  This version is 10x faster than legacy_add.
  
  ## Examples
  ```min
  add_v2(a: 5, b: 3)  // => 8
  ```
  """
  a: int
    @range: 0-1000
    """First operand (must be positive)."""
  
  b: int
    @range: 0-1000
    """Second operand (must be positive)."""
-> int {
  return a + b
}
```

## Example 7: Structural Typing

```min
type Point2D = { x: int, y: int }
type Vector2D = { x: int, y: int }
type Coordinate = { x: int, y: int }

fn distance_from_origin:
  """
  Calculates distance from origin.
  Works with any struct with x and y fields.
  """
  p: Point2D
-> float {
  // Can pass Vector2D or Coordinate - same structure
  let x_squared = p.x * p.x;
  let y_squared = p.y * p.y;
  return sqrt(x_squared + y_squared)
}

fn add_vectors:
  """Adds two vectors."""
  a: Vector2D
  b: Vector2D
-> Vector2D {
  return {
    x: a.x + b.x,
    y: a.y + b.y
  }
}

fn main:
  """Demonstrates structural typing."""
-> int {
  let point: Point2D = { x: 3, y: 4 };
  let vector: Vector2D = { x: 1, y: 2 };
  
  // Can use point where Vector2D is expected
  let result = add_vectors(a: point, b: vector);
  
  return result.x + result.y
}
```

## Example 8: Default Parameters

```min
fn greet_person:
  """Greets a person with optional title and formality."""
  name: string
    """The person's name."""
  
  title: string = "Mr."
    """Optional title (default: "Mr.")."""
  
  formal: bool = true
    """Whether to use formal greeting."""
-> string {
  let greeting = if formal { "Good day" } else { "Hey" };
  return "{greeting}, {title} {name}!"
}

fn create_user:
  """Creates a user with optional fields."""
  name: string
  email: string
  age: int = 0
  verified: bool = false
-> string {
  return "User: {name} ({email}), age {age}, verified: {verified}"
}
```

## Example 9: Nested Structs

```min
type Address = {
  street: string,
  city: string,
  zip: int
}

type Person = {
  name: string,
  age: int,
  address: Address
}

fn format_address:
  """Formats an address as a string."""
  addr: Address
-> string {
  return "{addr.street}, {addr.city} {addr.zip}"
}

fn describe_person:
  """Describes a person with their address."""
  person: Person
-> string {
  let addr_str = format_address(addr: person.address);
  return "{person.name}, age {person.age}, lives at {addr_str}"
}

fn main:
  """Creates and describes a person."""
-> string {
  let person = {
    name: "Alice",
    age: 30,
    address: {
      street: "123 Main St",
      city: "Springfield",
      zip: 12345
    }
  };
  
  return describe_person(person: person)
}
```

## Example 10: Complex Computation

```min
type Circle = {
  radius: float
}

type Rectangle = {
  width: float,
  height: float
}

fn circle_area:
  """Calculates the area of a circle."""
  @pure
  circle: Circle
-> float {
  let pi = 3.14159;
  return pi * circle.radius * circle.radius
}

fn rectangle_area:
  """Calculates the area of a rectangle."""
  @pure
  rect: Rectangle
-> float {
  return rect.width * rect.height
}

fn max:
  """Returns the maximum of two floats."""
  @pure
  a: float
  b: float
-> float {
  return if a > b { a } else { b }
}

fn larger_area:
  """Returns the larger area."""
  circle_radius: float
  rect_width: float
  rect_height: float
-> float {
  let circle = { radius: circle_radius };
  let rect = { width: rect_width, height: rect_height };
  
  let c_area = circle_area(circle: circle);
  let r_area = rectangle_area(rect: rect);
  
  return max(a: c_area, b: r_area)
}
```

## Example 11: Early Returns

```min
fn divide:
  """Divides two numbers, returns 0 if divisor is zero."""
  numerator: int
  denominator: int
-> int {
  if denominator == 0 {
    return 0;
  }
  
  return numerator / denominator
}

fn validate_and_process:
  """Validates input and processes it."""
  value: int
-> int {
  if value < 0 {
    return 0;
  }
  
  if value > 100 {
    return 100;
  }
  
  let processed = value * 2;
  let adjusted = processed + 10;
  return adjusted
}
```

## Example 12: Multiple Attributes

```min
fn critical_function:
  @pure
  @since: v1.0
  @deprecated: "Use critical_function_v2"
  @complexity: O(n^2)
  @author: "Alice"
  """
  A critical function with multiple attributes.
  
  ## Warning
  This function is deprecated and will be removed.
  
  ## Migration Guide
  Replace with critical_function_v2 which has O(n) complexity.
  """
  
  data: string
    @validate: non-empty
    @sensitive: true
    """Sensitive input data."""
  
  limit: int
    @range: 1-1000
    """Processing limit."""
    
-> string {
  return "Processed: {data} with limit {limit}"
}
```

## Example 13: Semicolon Usage

```min
fn demonstrate_semicolons:
  """Shows explicit semicolon usage."""
  x: int
-> int {
  // Statements end with semicolons
  let a = x * 2;
  let b = a + 10;
  let c = b - 5;
  
  // Last expression (no semicolon) is returned
  c * 3
}

fn with_explicit_return:
  """Shows explicit return."""
  x: int
-> int {
  let doubled = x * 2;
  return doubled + 1;  // Explicit return with semicolon
}

fn mixed_style:
  """Mixes implicit and explicit returns."""
  x: int
-> int {
  if x < 0 {
    return 0;  // Early return
  }
  
  let processed = x * 2;
  processed + 1  // Implicit return
}
```
