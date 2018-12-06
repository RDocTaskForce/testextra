
# Test to make sure an object is not of a class.
\dontrun{
# will return an error.
expect_is_not(1L, "numeric")
}

# but this is fine.
expect_is_not('a', "numeric")

expect_is_exactly('a', "character")
