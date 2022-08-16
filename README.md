---
title: 
---

<span color="red"> <b>WARNING: the compiler currently only supports
LINUX x86_64 systems, for other compilation targets please add your own
equivalent of
<a href="https://github.com/Miezekatze64/swl/blob/main/src/codegen_x86_64_linux.rs" target="_blank">codegen_x86_64_linux.rs</a>
and maybe even create a pull request.<br> WARNING: beacause of syscall
incompatibility, new targets also have to come with their own stdlib,
replacing <a href="./swl/linux.swl">linux.swl</a> (currently in
<a href="swl/std.swl">std.swl</a>)\` <br><br> <b>WARNING: this compiler
is currently at a very, very early state of development, do not expect
anything to work as you want it to….</b> </span>

# Getting started

-   install the following dependencies:

    -   nasm (netwide assembler)

-   clone this repo

``` shell
  git clone https://github.com/Miezekatze64/swl
```

-   run the compiler

``` shell
  ./swlc main.swl
```

# Primitive Types:

-   int     (64 bit signed integer)

-   float (not fully implemented)

-   bool (8 bit, 0 or 1)

-   char (8 bit)

-   void (0 bit, should not be used as type)

-   unchecked (will be removed, if type variables are fully implemented)

# Basic Syntax

-   Comments:

    ``` swl
    // single-line comment
    /*
    multi
    line
    comment
    */
    ```

-   Operators:

    ``` swl
    int a = 0;
    a += 42 / 132;
    bool b = a == 42;
    bool c = !(b != false);
    ...
    ```

-   Variables:

    ``` swl
    string test = "Hello, World!";
    ```

-   Conditional statements:

    -   if:

        ``` swl
        if (1 == 1) {
            println("is true");
        } else {
            // else is currently not implemented..., but will be soon
            println("THIS IS NOT POSSIBLE");
        }
        ```

    -   while:

        ``` swl
        while (true) {
            println("HERE");
        }
        ```

-   Functions:

    ``` swl
    func name(int arg0, string arg1) -> bool {
        // function content
    }
    ```

-   Return:

    ``` swl
    func main() -> int {
        // return 42
        <- 42;
    }
    ```

-   Type-Aliases:

    ``` swl
      alias string = [char];
    ```

-   Structures:

    -   Definition:

        ``` swl
        struct LinedList {
            LinkedList*  next;
            LinkdexList* prev;
            int len;
            int index;
        }
        ```

    -   Initialization:

        ``` swl
        LinkedList list = LinkedList {
            next: next_list;
            prev: prev_list;
            len: 42;
            index: 0;
        };
        ```

    -   Reading values:.

        ``` swl
        LinkedList list = /*  something */;
        int a = list.len;
        ```

-   Member functions:

    ``` swl
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

-   Type variable (currently under development):

    ``` swl
    // type variabled have to start with an underscore ('_') and are inferred by the compiler
    func id(_a a) -> _a {
        <- a;
    }
    ```

-   Type classes (currently under development):

    ``` swl
    typeclass Showable a {
        func to_string(a val) -> string;
    }
    ```

-   Type class instances:

    ``` swl
    instance Showable bool {
        func to_string(bool b) -> string {
            if (b) {
                <- "true";
            } else {
                <- "false";
            }
        }
    }
    ```

-   Loading of compiler intrinsics (should only be used in stdlib..):

    ``` swl
    intrinsic convert as __unsafe_convert(unchecked) -> unchecked;
    ```

-   For future syntax / semantic changes look at [TODO.md](./TODO.md)

# Hello World

``` swl
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

-   more examples are located in the [swl/examples](./swl/examples)
    directory
