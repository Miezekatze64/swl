---
generator: pandoc
viewport: width=device-width, initial-scale=1.0, user-scalable=yes
---

::: {#title-block-header}
#  {#section .title}
:::

WARNING: the compiler currently only supports LINUX x86_64 systems, for
other compilation targets please add your own equivalent of
[codegen_x86_64_linux.rs](https://github.com/Miezekatze64/swl/blob/main/src/codegen_x86_64_linux.rs){target="_blank"}
and maybe even create a pull request.\
WARNING: beacause of syscall incompatibility, new targets also have to
come with their own stdlib, replacing [linux.swl](./swl/linux.swl)
(currently in [std.swl](swl/std.swl))\`\
\
**WARNING: this compiler is currently at a very, very early state of
development, do not expect anything to work as you want it to....**

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

    ::: {#cb3 .sourceCode}
    ``` {.sourceCode .swl}
    // single-line comment
    /*
    multi
    line
    comment
    */
    ```
    :::

-   Operators:

    ::: {#cb4 .sourceCode}
    ``` {.sourceCode .swl}
    int a = 0;
    a += 42 / 132;
    bool b = a == 42;
    bool c = !(b != false);
    ...
    ```
    :::

-   Variables:

    ::: {#cb5 .sourceCode}
    ``` {.sourceCode .swl}
    string test = "Hello, World!";
    ```
    :::

-   Conditional statements:

    -   if:

        ::: {#cb6 .sourceCode}
        ``` {.sourceCode .swl}
        if (1 == 1) {
            println("is true");
        } else {
            // else is currently not implemented..., but will be soon
            println("THIS IS NOT POSSIBLE");
        }
        ```
        :::

    -   while:

        ::: {#cb7 .sourceCode}
        ``` {.sourceCode .swl}
        while (true) {
            println("HERE");
        }
        ```
        :::

-   Functions:

    ::: {#cb8 .sourceCode}
    ``` {.sourceCode .swl}
    func name(int arg0, string arg1) -> bool {
        // function content
    }
    ```
    :::

-   Return:

    ::: {#cb9 .sourceCode}
    ``` {.sourceCode .swl}
    func main() -> int {
        // return 42
        <- 42;
    }
    ```
    :::

-   Type-Aliases:

    ::: {#cb10 .sourceCode}
    ``` {.sourceCode .swl}
      alias string = [char];
    ```
    :::

-   Structures:

    -   Definition:

        ::: {#cb11 .sourceCode}
        ``` {.sourceCode .swl}
        struct LinedList {
            LinkedList*  next;
            LinkdexList* prev;
            int len;
            int index;
        }
        ```
        :::

    -   Initialization:

        ::: {#cb12 .sourceCode}
        ``` {.sourceCode .swl}
        LinkedList list = LinkedList {
            next: next_list;
            prev: prev_list;
            len: 42;
            index: 0;
        };
        ```
        :::

    -   Reading values:.

        ::: {#cb13 .sourceCode}
        ``` {.sourceCode .swl}
        LinkedList list = /*  something */;
        int a = list.len;
        ```
        :::

-   Member functions:

    ::: {#cb14 .sourceCode}
    ``` {.sourceCode .swl}
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
    :::

-   Type variable (currently under development):

    ::: {#cb15 .sourceCode}
    ``` {.sourceCode .swl}
    // type variabled have to start with an underscore ('_') and are inferred by the compiler
    func id(_a a) -> _a {
        <- a;
    }
    ```
    :::

-   Type classes (currently under development):

    ::: {#cb16 .sourceCode}
    ``` {.sourceCode .swl}
    typeclass Showable a {
        func to_string(a val) -> string;
    }
    ```
    :::

-   Type class instances:

    ::: {#cb17 .sourceCode}
    ``` {.sourceCode .swl}
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
    :::

-   Loading of compiler intrinsics (should only be used in stdlib..):

    ::: {#cb18 .sourceCode}
    ``` {.sourceCode .swl}
    intrinsic convert as __unsafe_convert(unchecked) -> unchecked;
    ```
    :::

-   For future syntax / semantic changes look at [TODO.md](./TODO.md)

# Hello World

::: {#cb19 .sourceCode}
``` {.sourceCode .swl}
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
:::

-   more examples are located in the [swl/examples](./swl/examples)
    directory
