# error for unknown options

    Code
      r_process_options(func = function() { }, foo = "bar")
    Condition
      Error:
      ! Unknown optioncharacter(0):'foo'

---

    Code
      r_process_options(func = function() { }, foo = "bar", bar = "foo")
    Condition
      Error:
      ! Unknown options:'foo' and 'bar'

