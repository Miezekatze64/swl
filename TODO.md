# TODO

## Compiler

- [ ] finish `ref` and `deref` implementation + implement `is_ref` for all possibilities + `type**`  syntax fix

- [ ] fix `unreachable!()` at line 1292 (struct literals as statement), should add error

- [ ] implement lambda functions + make all functions first-class

- [ ] check for redefined functions

- [ ] disallow redefinition of vars in same scope

- [ ] ensure that innermost var definition is used

- [ ] add some kind of generic types:
  
  ```rust
  struct List<T> {T head; List<T> tail}
  ```

- [ ] type inference -> multiple function generation 

- [ ] add support for operator overloading and create of own operators:
  
  ```cpp
  operator +(other_num a, other_num b) -> other_num) {}
  ```

- [ ] support floats

- [ ] add support for member functions
  
  ```swl
  func as_cstr() -> char* from string {}
  ```

- [ ] check for bugs...

- [ ] maybe add enums sometimes...

- [ ] add tuples

- [ ] add some unique features to differ more from c and rust...

- [ ] add support for functional concepts

- [ ] create code optimizer

- [ ] add macro system / preprocessor

- [ ] add useful comments
  
  ## SWL

- [ ] complete malloc implementation + wrapper functions

- [ ] implement some kind of dynamic array / list / vector

- [ ] create stdlib

- [ ] create stdmath library

- [ ] test sample programs (e.g. fizzbuzz)

- [ ] check turing completeness (maybe implement RULE 110)

- [ ] add lazy evaluated list (infinite data structures)

- [ ] add useful comments

- [ ] add some functional operators (requires custom operators)
