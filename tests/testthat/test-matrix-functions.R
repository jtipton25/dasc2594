test_that("rref", {
    expect_error(rref(matrix("A", 2, 2)))
    expect_error(rref(matrix(NA, 2, 2)))
    expect_error(rref(1:10))
    expect_error(rref(array(1:20, dim = c(2, 5, 2))))
    expect_error(rref(1))
    expect_error(rref(matrix(1, 1, 1)))

    A <- matrix(1:4, 2, 2)
    expect_equal(rref(A), diag(2))
    A <- matrix(1:6, 3, 2)
    expect_equal(rref(A), matrix(c(1, 0, 0, 0, 1, 0), 3, 2))
    A <- rbind(A, 0)
    expect_equal(rref(A), matrix(c(1, 0, 0, 0, 0, 1, 0, 0), 4, 2))

    A <- matrix(1:9 * 10^-200, 3, 3)
    A[2, 2] <- 2e210
    # expect_warning(rref(A))


})

test_that("eps", {

    expect_equal(eps(.Machine$double.xmin / 100), .Machine$double.xmin)
    expect_equal(eps(3), 4.44089209850063e-16)
    expect_equal(eps(1:3), 4.44089209850063e-16)
    expect_equal(eps(-1), 2.22044604925031e-16)
    expect_equal(eps(matrix(0:3, 2, 2)), 2.22044604925031e-16)
    expect_error(eps(NA))
    expect_error(eps("NA"))

})

test_that("array_to_latex", {
    expect_error(array_to_latex(1:10))
    expect_error(array_to_latex(array(1:20, dim = c(2, 5, 2))))
    expect_error(array_to_latex(1))

    expect_identical(array_to_latex(matrix("A", 2, 2)), "\\begin{pmatrix} A & A \\\\ A & A \\end{pmatrix}")
    expect_identical(array_to_latex(matrix(1:4, 2, 2)), "\\begin{pmatrix} 1 & 3 \\\\ 2 & 4 \\end{pmatrix}")
    expect_identical(array_to_latex(matrix(1:6, 3, 2)), "\\begin{pmatrix} 1 & 4 \\\\ 2 & 5 \\\\ 3 & 6 \\end{pmatrix}")

})
