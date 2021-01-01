test_that("rref", {
    A <- matrix(1:4, 2, 2)
    expect_equal(rref(A), diag(2))
    expect_error(rref(matrix("A", 2, 2)))
    expect_error(rref(matrix(NA, 2, 2)))
    A <- matrix(1:6, 3, 2)
    expect_equal(rref(A), matrix(c(1, 0, 0, 0, 1, 0), 3, 2))
    A <- rbind(A, 0)
    expect_equal(rref(A), matrix(c(1, 0, 0, 0, 0, 1, 0, 0), 4, 2))
})

test_that("eps", {

    expect_equal(eps(3), 4.44089209850063e-16)
    expect_equal(eps(1:3), 4.44089209850063e-16)
    expect_equal(eps(-1), 2.22044604925031e-16)
    expect_equal(eps(matrix(0:3, 2, 2)), 2.22044604925031e-16)
    expect_error(eps(NA))
    expect_error(eps("NA"))

})
