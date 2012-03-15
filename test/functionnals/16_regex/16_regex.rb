
assert_matches "abc", "b"
assert_matches "bc", "b"
assert_matches "bc", "^bc$"
assert_matches "12", "\\d+"
assert_matches "12", "\\d{2}"
assert_matches "123", "\\d{2,3}"

assert_matches "123", "abc"
