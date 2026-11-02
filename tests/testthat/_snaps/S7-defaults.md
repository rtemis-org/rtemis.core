# quoted defaults are validated at declaration

    Code
      suppressMessages(prop_float(default = quote(c(0, 2)), vector = TRUE, max = 1))
    Condition
      Error in `with_handlers()`:
      ! `default` must be <= 1.

---

    Code
      suppressMessages(prop_integer(default = quote(as.numeric(1))))
    Condition
      Error in `with_handlers()`:
      ! `default` must be of type <integer>, not <double>.

---

    Code
      suppressMessages(prop_bag(default = quote(list(a = 1, a = 2))))
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
