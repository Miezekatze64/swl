<span color="red">
<b>WARNING: the compiler currently only supports LINUX x86_64 systems, for other compilation targets please add your own equivalent of [codegen_x86_64_linux.rs](https://github.com/Miezekatze64/swl/blob/main/src/codegen_x86_64_linux.rs) and maybe even create a pull request</b>
<br><br>
<b>WARNING: this compiler is currently at a very, very early state of development, do not expect anything to work as you want it to....</b>
</span>

# Getting started

- install the following dependencies:
  
  - nasm (netwide assembler)

- clone this repo

```shell
  git clone https://github.com/Miezekatze64/swl
```

- run the compiler

```shell
  ./swlc main.swl
```

# Primitive Types:

- int     (64 bit signed integer)

- float  (not fully implemented)

- bool (8 bit, 0 or 1)

- char (8 bit)

- void (0 bit, should not be used as type)

- unchecked (will be removed, if type variables are fully implemented)

# Basic Syntax

- Comments:
  
  ```c
  // single-line comment
  /*
  multi
  line
  comment
  */
  ```

- Operators: 
  
  ```csharp
  int a = 0;
  a += 42 / 132;
  bool b = a == 42;
  bool c = !(b != false);
  ...
  ```

- Variables:
  
  ```go
  string test = "Hello, World!";
  ```

- Conditional statements:
  
  - if:
    
    ```c
    if (1 == 1) {
        println("is true");
    } else {
        // else is currently not implemented..., but will be soon
        println("THIS IS NOT POSSIBLE");
    }
    ```
  
  - while:
    
    ```java
    while (true) {
        println("HERE");
    }
    ```

- Functions: 
  
  ```go
  func name(int arg0, string arg1) -> bool {
      // function content
  }
  ```

- Return:
  
  ```go
  func main() -> int {
      // return 42
      <- 42;
  }
  ```

- Type-Aliases:
  
  ```bash
    alias string = [char];
  ```

- Structures:
  
  - Definition:
    
    ```c
    struct LinedList {
        LinkedList*  next;
        LinkdexList* prev;
        int len;
        int index;
    }
    ```
  
  - Initialization:
    
    ```java
    LinkedList list = LinkedList {
        next: next_list;
        prev: prev_list;
        len: 42;
        index: 0;
    };
    ```
  
  - Reading values:.
    
    ```java
    LinkedList list = /*  something */;
    int a = list.len;
    ```

- Member functions:
  
  ```go
  include "./std.swl"
  
  func to_string(bool self) -> string from bool {
      if (self) {
          return "true";
      }
      return "false";
  }
  
  func main() -> int {
      println(0.to_string();
      <- 0;
  }
  ```

- Type variable (currently under development):
  
  ```go
  // type variabled have to start with an underscore ('_') and are inferred by the compiler
  func id(_a a) -> _a {
      <- a;
  }
  ```

- Loading of compiler intrinsics (should only be used in stdlib..):
  
  ```c
  intrinsic convert as __unsafe_convert(unchecked) -> unchecked;
  ```

- For future syntax / semantic changes look at [TODO.md](./TODO.md)

# Hello World

```go
// include standard library
include "std.swl"

// declaration of main entrypoint
func main() -> int {
    // call `println` function
    println("Hello, World!");
    // return success
    <- 0;
}
```

- more examples are located in the [examples](./examples) directory
