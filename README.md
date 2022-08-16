---
generator: pandoc
viewport: width=device-width, initial-scale=1.0, user-scalable=yes
---

<div id="title-block-header">

# 

</div>

WARNING: the compiler currently only supports LINUX x86_64 systems, for
other compilation targets please add your own equivalent of <a
href="https://github.com/Miezekatze64/swl/blob/main/src/codegen_x86_64_linux.rs"
target="_blank">codegen_x86_64_linux.rs</a> and maybe even create a pull
request.  
WARNING: beacause of syscall incompatibility, new targets also have to
come with their own stdlib, replacing [linux.swl](./swl/linux.swl)
(currently in [std.swl](swl/std.swl))\`  
  
**WARNING: this compiler is currently at a very, very early state of
development, do not expect anything to work as you want it to….**

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

    <div id="cb3" class="sourceCode">

    ``` sourceCode
    // single-line comment
    /*
    multi
    line
    comment
    */
    ```

    </div>

-   Operators:

    <div id="cb4" class="sourceCode">

    ``` sourceCode
    int a = 0;
    a += 42 / 132;
    bool b = a == 42;
    bool c = !(b != false);
    ...
    ```

    </div>

-   Variables:

    <div id="cb5" class="sourceCode">

    ``` sourceCode
    string test = "Hello, World!";
    ```

    </div>

-   Conditional statements:

    -   if:

        <div id="cb6" class="sourceCode">

        ``` sourceCode
        if (1 == 1) {
            println("is true");
        } else {
            // else is currently not implemented..., but will be soon
            println("THIS IS NOT POSSIBLE");
        }
        ```

        </div>

    -   while:

        <div id="cb7" class="sourceCode">

        ``` sourceCode
        while (true) {
            println("HERE");
        }
        ```

        </div>

-   Functions:

    <div id="cb8" class="sourceCode">

    ``` sourceCode
    func name(int arg0, string arg1) -> bool {
        // function content
    }
    ```

    </div>

-   Return:

    <div id="cb9" class="sourceCode">

    ``` sourceCode
    func main() -> int {
        // return 42
        <- 42;
    }
    ```

    </div>

-   Type-Aliases:

    <div id="cb10" class="sourceCode">

    ``` sourceCode
      alias string = [char];
    ```

    </div>

-   Structures:

    -   Definition:

        <div id="cb11" class="sourceCode">

        ``` sourceCode
        struct LinedList {
            LinkedList*  next;
            LinkdexList* prev;
            int len;
            int index;
        }
        ```

        </div>

    -   Initialization:

        <div id="cb12" class="sourceCode">

        ``` sourceCode
        LinkedList list = LinkedList {
            next: next_list;
            prev: prev_list;
            len: 42;
            index: 0;
        };
        ```

        </div>

    -   Reading values:.

        <div id="cb13" class="sourceCode">

        ``` sourceCode
        LinkedList list = /*  something */;
        int a = list.len;
        ```

        </div>

-   Member functions:

    <div id="cb14" class="sourceCode">

    ``` sourceCode
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

    </div>

-   Type variable (currently under development):

    <div id="cb15" class="sourceCode">

    ``` sourceCode
    // type variabled have to start with an underscore ('_') and are inferred by the compiler
    func id(_a a) -> _a {
        <- a;
    }
    ```

    </div>

-   Type classes (currently under development):

    <div id="cb16" class="sourceCode">

    ``` sourceCode
    typeclass Showable a {
        func to_string(a val) -> string;
    }
    ```

    </div>

-   Type class instances:

    <div id="cb17" class="sourceCode">

    ``` sourceCode
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

    </div>

-   Loading of compiler intrinsics (should only be used in stdlib..):

    <div id="cb18" class="sourceCode">

    ``` sourceCode
    intrinsic convert as __unsafe_convert(unchecked) -> unchecked;
    ```

    </div>

-   For future syntax / semantic changes look at [TODO.md](./TODO.md)

# Hello World

<div id="cb19" class="sourceCode">

``` sourceCode
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

</div>

-   more examples are located in the [swl/examples](./swl/examples)
    directory
