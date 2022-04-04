abalone <- read.csv("data/abalone_raw.csv")
library(ggplot2)

lr_test <- function(reduced, full, alpha = .05) {
  statistic <- deviance(reduced) - deviance(full)
  df <- length(coef(full)) - length(coef(reduced))
  statistic <-
    cat("Statistic: ", statistic, "\n",
      "Degrees of freedom: ", df, "\n",
      "Threshhold: ", qchisq(1 - alpha, df = df), "\n",
      "p = ", pchisq(statistic, df = df, lower.tail = FALSE), "\n",
      sep = ""
    )
}

generate_rich_model <- function(model, ..., model_formula = model[["formula"]], model_data = model[["data"]], max_degree = 3) {
  stopifnot(max_degree > 0, max_degree %% 1 == 0)
  pred <- all.names(model_formula)[[2]]
  drop_cols <- c(...)
  model_data <- model_data[setdiff(names(model_data), c(drop_cols, pred))]
  syms <- colnames(model_data)
  names(syms) <- syms
  syms <- lapply(syms, as.symbol)
  interactions <- combn(syms, m = 2, FUN = \(x) bquote(.(x[[1]]) *
    .(x[[2]])), simplify = FALSE)
  poly_template <- lapply(seq(2, max_degree), \(x) substitute(temp^x, list(x = x))) |>
    Reduce(f = \(x, y) substitute(x + y, list(x = x, y = y)))
  print(poly_template)
  poly <- syms[sapply(model_data, is.numeric)] |>
    lapply(\(x) eval(substitute(substitute(
      poly_template,
      list(temp = x)
    ), list(poly_template = poly_template))))
  # poly <- syms[sapply(model_data, is.numeric)] |>
  # lapply(\(x) bquote(I(poly(.(x), degree = .(max_degree))[, -1])))
  terms <- Reduce(c(poly, interactions),
    f = \(x, y) substitute(x + y, list(x = x, y = y))
  )

  pred <- as.symbol(pred)
  # bquote(.(as.symbol(pred)) ~ .(terms))
  substitute(pred ~ terms)
}
# For a polytomous model, extract from a data frame with a column for each level of a response the value at each row by a vector of column indices
extract_by_level <- function(data, indices) {
  data[cbind(seq_len(nrow(data)), indices)]
}

confusion_matrix <- function(actual, predicted) {
  table(truth = actual, predicted = predicted)
}

analyze_cm <- function(cm) {
  diagonal <- diag(cm)
  trace <- sum(diagonal)
  row_sums <- rowSums(cm)
  rbind(
    sensitity = diagonal / row_sums,
    specificity = (trace - diagonal) / (trace - diagonal + row_sums - diagonal), # doesn't count negatives as true if they exclude the class correctly but don't predict the correct class, e.g. y = F and pred = M
    precision = diag(cm) / colSums(cm)
  )
}
#' The three separate weight variables don't quite sum to total weight, suggesting measurement Error.
#' Still, including all three would be a bad idea because it would make the model matrix nearly singular.
summary(with(abalone, Whole.weight - Shucked.weight - Viscera.weight - Shell.weight))

#' I fit an initial model using sex, dimensions,
#' and whole weight as predictors
library(nnet)
abalone[["Sex"]] <- factor(abalone[["Sex"]], levels = c("I", "F", "M"))
train_i <- sample(nrow(abalone), floor(nrow(abalone) * 0.8), replace = FALSE)
train <- abalone[train_i, ]
test <- abalone[-train_i, ]
initial_model <- multinom(Sex ~ Whole.weight + Length + Diameter + Height + Rings, data = train)

summary(initial_model)
anova(update(initial_model, . ~ . - .), initial_model)

#' Most of the estimated coefficients are
#' significant under Wald tests, even after applying
#' the Bonferroni correction to $p$-values.

summarized <- broom::tidy(initial_model)
summarized[["p.value"]] <- p.adjust(summarized[["p.value"]], method = "bonferroni")
summarized

rich <- generate_rich_model(initial_model,
  model_data = train[, colnames(coef(initial_model))[-1]],
  model_formula = Sex ~ Whole.weight + Length + Diameter + Height + Rings
)

#' Stepwise selection chooses a model with a few interactions
step_model <- suppressMessages(step(initial_model, scope = list(upper = rich, lower = Sex ~ Whole.weight + Length + Diameter + Height + Rings)))
step_residual <- extract_by_level(residuals(step_model, "std.res"), as.integer(train[["Sex"]]))
step_fitted <- extract_by_level(step_model[["fitted.values"]], as.integer(train[["Sex"]]))

#' Average standardized residuals are notably lower for infants than for adults
ggplot(
  data.frame(step_fitted, step_residual,
    Sex = train[["Sex"]],
    yintercept = ave(step_residual, train[["Sex"]], FUN = mean)
  ),
  aes(x = seq_len(nrow(train)), y = step_residual, color = Sex)
) +
  geom_point() +
  geom_hline(aes(yintercept = yintercept, color = Sex), show.legend = FALSE)

## Testing
#' A likelihood-ratio test of the step-selected model over the initial model is highly significant. A goodness-of-fit test for the step model has a p-value that is computationally 1, providing no evidence against the null of a good fit

lr_test(initial_model, step_model)
pchisq(sum(extract_by_level(
  residuals(step_model, "pearson"),
  as.integer(train[["Sex"]])
)^2),
df = nrow(train) - length(coef(step_model)), lower.tail = FALSE
)

#' A type II ANOVA test shows that all predictors aside from `diameter` significantly improve the fit when included. `Whole.weight` is by far the most important, followed by `Rings` and the interactions.
#'
car::Anova(step_model)

#' On both training and testing sets, sensitivity, specificity, and precision are much higher for the infant than the adult classes. However, test error
#' was only a little higher than train error.  Still,
#'
train_preds <- predict(step_model, newdata = train, type = "class")
mean(train_preds == train[["Sex"]])
train_cm <- confusion_matrix(train[["Sex"]], train_preds)
train_cm
analyze_cm(train_cm)

test_preds <- predict(step_model, newdata = test, type = "class")
mean(test_preds == test[["Sex"]])
test_cm <- confusion_matrix(test[["Sex"]], test_preds)
test_cm
analyze_cm(test_cm)
analyze_cm(train_cm) - analyze_cm(test_cm)
