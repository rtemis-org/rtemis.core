# quoted defaults are validated at declaration

    Code
      suppressMessages(prop_array(prop_number(max = 1), default = quote(c(0, 2))))
    Condition
      Error in `with_handlers()`:
      ! `default` must be <= 1.

---

    Code
      suppressMessages(prop_integer(default = quote(as.numeric(1))))
    Condition
      Error in `with_handlers()`:
      ! `default` must be of type integer.

---

    Code
      suppressMessages(prop_map(prop_any(), default = quote(list(a = 1, a = 2))))
    Condition
      Error in `with_handlers()`:
      ! `default` must have distinct names ('a' is repeated).

---

    Code
      suppressMessages(prop_string(default = quote({
        NULL
      })))
    Condition
      Error in `with_handlers()`:
      ! `default` must be of type <character>, not <NULL>.

