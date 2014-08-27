
context("is_falsy")

test_that("falsy things are falsy", {

  expect_true(is_falsy(NULL))
  expect_true(is_falsy(FALSE))
  expect_true(is_falsy(0L))
  expect_true(is_falsy(0.0))
  expect_true(is_falsy(0+0i))
  expect_true(is_falsy(""))
  expect_true(is_falsy(as.raw(0)))
  expect_true(is_falsy(logical()))
  expect_true(is_falsy(integer()))
  expect_true(is_falsy(double()))
  expect_true(is_falsy(complex()))
  expect_true(is_falsy(character()))
  expect_true(is_falsy(raw()))
  expect_true(is_falsy(list()))
  expect_true(is_falsy(try(silent = TRUE, stop("hey!"))))
  
})

context("TRUTHY, FALSY")

test_that("TRUTHY, FALSY", {

  expect_true(is_truthy(TRUTHY))
  expect_true(is_falsy(FALSY))

})

context("is_truthy")

test_that("not falsy things are truthy", {

  expect_true(is_truthy(falsy::not(NULL)))
  expect_true(is_truthy(falsy::not(FALSE)))
  expect_true(is_truthy(falsy::not(0L)))
  expect_true(is_truthy(falsy::not(0.0)))
  expect_true(is_truthy(falsy::not(0+0i)))
  expect_true(is_truthy(falsy::not("")))
  expect_true(is_truthy(falsy::not(as.raw(0))))
  expect_true(is_truthy(falsy::not(logical())))
  expect_true(is_truthy(falsy::not(integer())))
  expect_true(is_truthy(falsy::not(double())))
  expect_true(is_truthy(falsy::not(complex())))
  expect_true(is_truthy(falsy::not(character())))
  expect_true(is_truthy(falsy::not(raw())))
  expect_true(is_truthy(falsy::not(list())))
  expect_true(is_truthy(falsy::not(try(silent = TRUE, stop("hey!")))))
  
})

context("%||%")

test_that("%||% works", {

  a <- FALSY %||% "foo"
  expect_equal(a, "foo")

  a <- "truthy" %||% stop("don't evaluate this, ever!")
  expect_equal(a, "truthy")
  
})

test_that("%||% returns the parent", {

  f <- function() {
    FALSE %||% return("return this")
    "instead of this"
  }
  expect_equal(f(), "return this")

  g <- function() {
    TRUE %||% return("but not this")
    "this instead"
  }
  expect_equal(g(), "this instead")
  
})

context("%&&%")

test_that("%&&% works", {

  a <- FALSY %&&% stop("don't evaluate this!")
  expect_equal(a, FALSY)

  a <- TRUTHY %&&% "foo"
  expect_equal(a, "foo")

})

test_that("%&&% returns the parent", {

  f <- function() {
    TRUE %&&% return("this")
    "not this"
  }
  expect_equal(f(), "this")
  
  g <- function() {
    FALSE %&&% return("nope")
    "yep"
  }
  expect_equal(g(), "yep")

})

context("not")

test_that("not works", {

  expect_equal(not(TRUTHY), FALSY)
  expect_equal(not(FALSY), TRUTHY)
  
})

context("try quietly")

test_that("try_quietly works", {

  expect_equal(try_quietly("foo"), "foo")
  expect_true(inherits(try_quietly(stop("foo!")), "try-error"))
  expect_output(try_quietly(stop("foo!")), "^$")

})

context("with magrittr")

test_that("or and and work well with magrittr chains", {

  if (require(magrittr, quietly = TRUE)) {

    res <- 1:10 %>% sum() %||% "foobaR" %>% paste("!!!")
    expect_equal(res, "55 !!!")

    res2 <- 1:10 %>% sum() %&&% "foo" %>% paste("!!!")
    expect_equal(res2, "foo !!!")
  }
})
