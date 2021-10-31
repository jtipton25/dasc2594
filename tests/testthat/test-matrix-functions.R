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

    # A <- matrix(1:9 * 10^-200, 3, 3)
    # A[2, 2] <- 2e210
    # expect_warning(rref(A))
    A <- matrix(1:4, 2, 2)
    expect_message(rref(A, num_flops = TRUE), "The number of flops to get RREF was 9")


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

    expect_error(array_to_latex(array(1:20, dim = c(2, 5, 2))), "Input matrix A must be a vector or a matrix.")

    expect_error(array_to_latex(matrix("A", 2, 2), fraction = TRUE), "Input matrix A must be a matrix of numeric values when fraction = TRUE.")
    expect_error(array_to_latex(matrix(1:4, 2, 2), fraction = "a"), "fraction must be a logical input.")


    expect_identical(array_to_latex(1), "\\begin{pmatrix} 1 \\end{pmatrix}")
    expect_identical(array_to_latex(1:10), "\\begin{pmatrix} 1 \\\\ 2 \\\\ 3 \\\\ 4 \\\\ 5 \\\\ 6 \\\\ 7 \\\\ 8 \\\\ 9 \\\\ 10 \\end{pmatrix}")
    expect_identical(array_to_latex(matrix("A", 2, 2), fraction = FALSE), "\\begin{pmatrix} A & A \\\\ A & A \\end{pmatrix}")
    expect_identical(array_to_latex(matrix(1:4, 2, 2)), "\\begin{pmatrix} 1 & 3 \\\\ 2 & 4 \\end{pmatrix}")
    expect_identical(array_to_latex(matrix(1:6, 3, 2)), "\\begin{pmatrix} 1 & 4 \\\\ 2 & 5 \\\\ 3 & 6 \\end{pmatrix}")

})


test_that("is_basis", {
    expect_error(is_basis(rep(1, 4)), "B must be a square numeric matrix.")
    expect_error(is_basis(matrix(0, 3, 4)), "B must be a square numeric matrix.")
    expect_error(is_basis(array(0, dim = c(3, 4, 2))), "B must be a square numeric matrix.")
    expect_error(is_basis(matrix("a", 3, 3)), "B must be a square numeric matrix.")

    expect_true(is_basis(matrix(1:4, 2, 2)))
    expect_true(is_basis(matrix(c(6, 5, 1, 8, 7, 9, 4, 3, 2), 3, 3)))
    expect_true(is_basis(diag(3)))
    expect_true(is_basis(diag(5)))
    expect_true(is_basis(diag(10)))
    expect_false(is_basis(matrix(1, 3, 3)))
})

test_that("make_system_of_equations", {

    set.seed(101)
    expect_identical(make_system_of_equations(n_equations = 3, n_variables = 4, is_consistent = TRUE),
                     list(A = structure(c(-1, 4, 7, -1, 4, 0, -7, -7, -8, -7, -7, -1), .Dim = 3:4),
                          b = structure(c(-49, -14, 29), .Dim = c(3L, 1L)),
                          x = c(7, 0, 2, 4), is_consistent = TRUE))

    expect_identical(make_system_of_equations(n_equations = 5, n_variables = 2, is_consistent = FALSE),
                     list(A = structure(c(-2L, -4L, -3L, 0L, 0L, 6L, 0L, 1L, 4L, -2L), .Dim = c(5L, 2L)),
                          b = structure(c(4, 20, 21, -2, -5), .Dim = c(5L, 1L)),
                          is_consistent = FALSE))

    expect_identical(make_system_of_equations(n_equations = 2, n_variables = 2, dim_col = 1, dim_null = 1, is_consistent = TRUE),
                     list(A = structure(c(-7, 0, -7, 0), .Dim = c(2L, 2L)),
                          b = structure(c(56, 0), .Dim = 2:1),
                          x = c(-8, 0), is_consistent = TRUE))

    expect_identical(make_system_of_equations(n_equations = 2, n_variables = 2, dim_col = 1, dim_null = 1, is_consistent = FALSE),
                     list(A = structure(c(6, 4, 6, 4), .Dim = c(2L, 2L)),
                          b = structure(c(12, 16), .Dim = 2:1),
                          is_consistent = FALSE))

    # check that the systems of equations have the correct results
    eq <- make_system_of_equations(2, 2, is_consistent = TRUE)
    expect_true(is_consistent(eq$A, eq$b))
    expect_true(is_unique(eq$A, eq$b))
    expect_equal(eq$A %*% eq$x, eq$b)
    eq <- make_system_of_equations(5, 2, is_consistent = TRUE)
    expect_true(is_consistent(eq$A, eq$b))
    expect_true(is_unique(eq$A, eq$b))
    expect_equal(eq$A %*% eq$x, eq$b)
    eq <- make_system_of_equations(3, 5, is_consistent = TRUE)
    expect_true(is_consistent(eq$A, eq$b))
    expect_false(is_unique(eq$A, eq$b))
    expect_equal(eq$A %*% eq$x, eq$b)

    eq <- make_system_of_equations(2, 2, dim_col = 1, dim_null = 1, is_consistent = FALSE)
    expect_false(is_consistent(eq$A, eq$b))
    eq <- make_system_of_equations(5, 2, is_consistent = FALSE)
    expect_false(is_consistent(eq$A, eq$b))
    eq <- make_system_of_equations(3, 4, dim_col = 2, is_consistent = FALSE)
    expect_false(is_consistent(eq$A, eq$b))

    # check function inputs
    expect_error(make_system_of_equations(-1, 1), "n_equations must be a positive integer")
    expect_error(make_system_of_equations(NULL, 1), "n_equations must be a positive integer")
    expect_error(make_system_of_equations(NA, 1), "n_equations must be a positive integer")
    expect_error(make_system_of_equations("a", 1), "n_equations must be a positive integer")
    expect_error(make_system_of_equations(1:4, 1), "n_equations must be a positive integer")
    expect_error(make_system_of_equations(matrix(1:4, 2, 2), 1), "n_equations must be a positive integer")

    expect_error(make_system_of_equations(1, -1), "n_variables must be a positive integer")
    expect_error(make_system_of_equations(1, NULL), "n_variables must be a positive integer")
    expect_error(make_system_of_equations(1, NA), "n_variables must be a positive integer")
    expect_error(make_system_of_equations(1, "a"), "n_variables must be a positive integer")
    expect_error(make_system_of_equations(1, 1:4), "n_variables must be a positive integer")
    expect_error(make_system_of_equations(1, matrix(1:4, 2, 2)), "n_variables must be a positive integer")

    expect_error(make_system_of_equations(2, 2, dim_col = 3, dim_null = 1), "dim_col must be no larger than min\\(n_equations, n_variables\\)")
    expect_error(make_system_of_equations(2, 2, dim_col = -1), "dim_col must be a positive integer")
    expect_error(make_system_of_equations(2, 2, dim_col = NULL), "dim_col must be a positive integer")
    expect_error(make_system_of_equations(2, 2, dim_col = NA), "dim_col must be a positive integer")
    expect_error(make_system_of_equations(2, 2, dim_col = "aa"), "dim_col must be a positive integer")
    expect_error(make_system_of_equations(2, 2, dim_col = 1:4), "dim_col must be a positive integer")
    expect_error(make_system_of_equations(2, 2, dim_col = matrix(1:4, 2, 2)), "dim_col must be a positive integer")

    expect_error(make_system_of_equations(2, 2, dim_col = 1, dim_null = 2), "dim_col \\+ dim_null must equal n_variables")
    expect_error(make_system_of_equations(2, 2, dim_null = -1), "dim_null must be a positive integer")
    expect_error(make_system_of_equations(2, 2, dim_null = NULL), "dim_null must be a positive integer")
    expect_error(make_system_of_equations(2, 2, dim_null = NA), "dim_null must be a positive integer")
    expect_error(make_system_of_equations(2, 2, dim_null = "aa"), "dim_null must be a positive integer")
    expect_error(make_system_of_equations(2, 2, dim_null = 1:4), "dim_null must be a positive integer")
    expect_error(make_system_of_equations(2, 2, dim_null = matrix(1:4, 2, 2)), "dim_null must be a positive integer")

    expect_error(make_system_of_equations(2, 2, is_consistent = "a"), "is_consistent must be either TRUE or FALSE")
    expect_error(make_system_of_equations(2, 2, is_consistent = 1), "is_consistent must be either TRUE or FALSE")
    expect_error(make_system_of_equations(2, 2, is_consistent = NA), "is_consistent must be either TRUE or FALSE")
    expect_error(make_system_of_equations(2, 2, is_consistent = c(TRUE, FALSE)), "is_consistent must be either TRUE or FALSE")

    expect_error(make_system_of_equations(2, 2, is_homogeneous = "a"), "is_homogeneous must be either TRUE or FALSE")
    expect_error(make_system_of_equations(2, 2, is_homogeneous = 1), "is_homogeneous must be either TRUE or FALSE")
    expect_error(make_system_of_equations(2, 2, is_homogeneous = NA), "is_homogeneous must be either TRUE or FALSE")
    expect_error(make_system_of_equations(2, 2, is_homogeneous = c(TRUE, FALSE)), "is_homogeneous must be either TRUE or FALSE")

    # cannot have inconsistent solutions
    expect_error(make_system_of_equations(2, 2, is_consistent = FALSE), "is_consistent must be TRUE when dim_col = n_equations")
    expect_error(make_system_of_equations(3, 4, is_consistent = FALSE), "is_consistent must be TRUE when dim_col = n_equations")

    # dim_col + dim_null must equal n_variables
    expect_error(make_system_of_equations(2, 2, dim_col = 2, dim_null = 1, is_consistent = FALSE), "dim_col \\+ dim_null must equal n_variables")



})

test_that("is_consistent", {
    A <- diag(4)
    b <- 1:4
    expect_true(is_consistent(A, b))

    A <- matrix(c(1, 0, 0, 0, 1, 0), 3, 2)
    b <- c(1, 1, 0)
    expect_true(is_consistent(A, b))
    b <- c(0, 0, 1)
    expect_false(is_consistent(A, b))

    A <- matrix(c(1, 0, 0, 1, 1, 1), 2, 3)
    b <- 1:2
    expect_true(is_consistent(A, b))

    expect_error(is_consistent(matrix("A", 2, 2), 1:2), "A must be a numeric matrix.")
    expect_error(is_consistent(matrix(NA, 2, 2), 1:2), "A must be a numeric matrix.")
    expect_error(is_consistent(1:10, 1:10), "A must be a numeric matrix.")
    expect_error(is_consistent(array(1:20, dim = c(2, 5, 2)), 1:10), "A must be a numeric matrix.")
    expect_error(is_consistent(1, 1), "A must be a numeric matrix.")

    expect_error(is_consistent(matrix(1:4, 2, 2), c("aa", "a")), "b must be a numeric vector.")
    expect_error(is_consistent(matrix(1:4, 2, 2), rep(NA, 2)), "b must be a numeric vector.")
    expect_error(is_consistent(matrix(1:4, 2, 2), matrix(1:4, 2, 2)), "b must be a numeric vector.")
    expect_error(is_consistent(matrix(1:4, 2, 2), array(1:4, dim = c(2, 2, 4))), "b must be a numeric vector.")

    expect_error(is_consistent(matrix(1:9, 3, 3), 1:2), "A and b must have the same number of rows.")
    expect_error(is_consistent(matrix(1:4, 2, 2), 1:3), "A and b must have the same number of rows.")


})

test_that("is_unique", {
    A <- diag(4)
    b <- 1:4
    expect_true(is_unique(A, b))

    A <- matrix(c(1, 0, 0, 0, 1, 0), 3, 2)
    b <- c(1, 1, 0)
    expect_true(is_unique(A, b))
    b <- c(0, 0, 0)
    expect_true(is_unique(A, b))
    b <- c(0, 0, 1)
    expect_error(is_unique(A, b), "The system of equations Ax = b must be consistent.")

    A <- matrix(c(1, 0, 0, 1, 1, 1), 2, 3)
    b <- 1:2
    expect_false(is_unique(A, b))
    b <- c(0, 0)
    expect_false(is_unique(A, b))


    expect_error(is_unique(matrix("A", 2, 2), 1:2), "A must be a numeric matrix.")
    expect_error(is_unique(matrix(NA, 2, 2), 1:2), "A must be a numeric matrix.")
    expect_error(is_unique(1:10, 1:10), "A must be a numeric matrix.")
    expect_error(is_unique(array(1:20, dim = c(2, 5, 2)), 1:10), "A must be a numeric matrix.")
    expect_error(is_unique(1, 1), "A must be a numeric matrix.")

    expect_error(is_unique(matrix(1:4, 2, 2), c("aa", "a")), "b must be a numeric vector.")
    expect_error(is_unique(matrix(1:4, 2, 2), rep(NA, 2)), "b must be a numeric vector.")
    expect_error(is_unique(matrix(1:4, 2, 2), matrix(1:4, 2, 2)), "b must be a numeric vector.")
    expect_error(is_unique(matrix(1:4, 2, 2), array(1:4, dim = c(2, 2, 4))), "b must be a numeric vector.")

    expect_error(is_unique(matrix(1:9, 3, 3), 1:2), "A and b must have the same number of rows.")
    expect_error(is_unique(matrix(1:4, 2, 2), 1:3), "A and b must have the same number of rows.")

})

test_that("make_eigen", {

    set.seed(2021)
    expect_equal(make_eigen(3),
                     list(A = structure(c(-1.8, -1.2, -5.2, -9.2, 0.2, -14.8, 5.4, 0.6, 11.6), .Dim = c(3L, 3L)),
                          V = structure(c(2L, 1L, 2L, -1L, 1L, 1L, 1L, 0L, 2L), .Dim = c(3L, 3L)),
                          d = c(-1L, 2L, 9L)))

    expect_error(make_eigen("a"), "n must be a positive integer")
    expect_error(make_eigen(-1), "n must be a positive integer")
    expect_error(make_eigen(NA), "n must be a positive integer")
    expect_error(make_eigen(NULL), "n must be a positive integer")
    expect_error(make_eigen(TRUE), "n must be a positive integer")
    expect_error(make_eigen(rep(1, 4)), "n must be a positive integer")
    expect_error(make_eigen(matrix(1:4, 2, 2)), "n must be a positive integer")

})


test_that("is_invertible", {
    A <- diag(4)
    expect_true(is_invertible(A))
    A <- matrix(c(0, 1, -1, 0), 2, 2)
    expect_true(is_invertible(A))
    A <- matrix(1, 3, 3)
    expect_false(is_invertible(A))


    A <- matrix(c(1, 0, 0, 0, 1, 0), 3, 2)
    expect_error(is_invertible(A), "A must be a square numeric matrix")
    A <- matrix(c(1, 0, 0, 1, 1, 1), 2, 3)
    expect_error(is_invertible(A), "A must be a square numeric matrix")
    expect_error(is_invertible(matrix("A", 2, 2)), "A must be a square numeric matrix")
    expect_error(is_invertible(matrix(NA, 2, 2)), "A must be a square numeric matrix")
    expect_error(is_invertible(1:10), "A must be a square numeric matrix")
    expect_error(is_invertible(array(1:20, dim = c(2, 5, 2))), "A must be a square numeric matrix")
    expect_error(is_invertible(1), "A must be a square numeric matrix")

})

# test_that("elementary_matrix"{
#
# })

# test_that("elementary_matrix_sequence"{
#
# })
