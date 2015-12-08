context("Checking the generateTransforms function")

test_that("Runs as expected", {
  generateText <- function(n) {
    n_words <- rpois(n, lambda = 4) + 1
    word_length <- lapply(n_words, function(x) rpois(x, lambda = 3) + 1)
    sapply(word_length, function(y) {
      paste(sapply(y, function(x)
        paste0(sample(c(LETTERS, letters), size = x, replace = TRUE), collapse = "")
      ), collapse = " ")
    }
    )
  }
  set.seed(1234)
  n_obs <- 50
  test_data <- data.frame(n1 = rnorm(n_obs),
                          n2 = rnorm(n_obs),
                          f1 = factor(sample(letters[1:3], size = n_obs, replace = TRUE)),
                          f2 = factor(sample(letters[4:7], size = n_obs, replace = TRUE)),
                          d1 = as.Date("2015-01-01") + 0:9,
                          d2 = as.Date("2012-01-01") + round(runif(n_obs, min = -20, max = 20)),
                          c1 = generateText(n_obs),
                          c2 = generateText(n_obs),
                          stringsAsFactors = FALSE
  )
  expect_named(test_data, c("n1", "n2", "f1", "f2", "d1", "d2", "c1", "c2"))

})



