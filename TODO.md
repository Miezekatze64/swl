# TODO

## Compiler

<!-- - [ ] IMPORTANT -- FIX ORDER OF STRUCT FIELDS!!! -- IMPORTANT -->

- [x] add casting

- [x] finish `ref` and `deref` implementation + implement `is_ref` for all possibilities

- [x] allow `deref a = val;` instead of `set_ptr(__unsafe_convert(a), val);`

- [x] implement else

- [x] implement for loop

- [x] fix `unreachable!()` at line 1292 (struct literals as statement), should add error

- [x] make all functions first-class

- [ ] implement lambda functions

- [x] check for redefined functions

- [x] ensure that innermost var definition is used

- [ ] add some kind of generic types:
  
  ```rust
  struct List<T> {T head; List<T> tail}
  ```

- [x] type inference -> multiple function generation 

- [ ] add support for operator overloading and creation of own operators:
  
  ```cpp
  operator +(other_num a, other_num b) -> other_num {}
  ```

- [ ] support floats

- [x] add support for member functions
  
  ```swl
  func as_cstr() -> char* from string {}
  ```

- [ ] check for bugs...

- [ ] maybe add enums sometimes...

- [ ] add tuples

- [ ] add some unique features to differ more from c and rust...

- [ ] add support for functional concepts

- [x] create code optimizer

- [ ] add macro system / preprocessor

- [ ] add useful comments

- [ ] add REPL to interpreter
  
  ## SWL

- [x] complete malloc implementation + wrapper functions

- [ ] fix free() / malloc() implementation, to not return "double free or corruption"

- [ ] implement some kind of dynamic array / list / vector

- [x] create stdlib

- [ ] create math library (important functions in `std.swl`, extras in `math.swl`)

- [x] test sample programs (e.g. fizzbuzz)

- [x] check turing completeness (maybe implement RULE 110)

- [ ] add lazy evaluated list (infinite data structures)

- [x] add useful comments

- [ ] add some functional operators (requires custom operators ↑)
