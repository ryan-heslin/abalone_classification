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

  poly <- syms[sapply(model_data, is.numeric)] |>
    lapply(\(x) eval(substitute(substitute(
      poly_template,
      list(temp = x)
    ), list(poly_template = poly_template))))
  terms <- Reduce(c(poly, interactions),
    f = \(x, y) substitute(x + y, list(x = x, y = y))
  )

  pred <- as.symbol(pred)
  substitute(pred ~ terms)
}


# Update an object's formula by replacing all occurrences of the symbol `old` with `new`
update_formula <- function(object, old, new, indirect = FALSE) {
  old_formula <- formula(object)
  new <- substitute(new)
  # If the variable name is passed indirectly (i.e., this function is being called from another function), evaluate it in caller environment
  if (indirect) new <- eval(new, envir = parent.frame())
  old <- deparse(substitute(old))
  sub <- list(new)
  names(sub) <- old
  new_formula <- eval(substitute(substitute(old_formula, sub), list(old_formula = old_formula)))
  eval(substitute(update(object, new_formula)))
}

# For a polytomous model, extract from a data frame with a column for each level of a response the value at each row by a vector of column indices
extract_by_level <- function(data, indices) {
  data[cbind(seq_len(nrow(data)), indices)]
}

confusion_matrix <- function(actual, predicted) {
  table(truth = actual, predicted = predicted)
}

# Extract class-specific sensitivity, specificity, and precision from a confusion matrix
analyze_cm <- function(cm) {
  diagonal <- diag(cm)
  trace <- sum(diagonal)
  row_sums <- rowSums(cm)
  rbind(
    sensitivity = diagonal / row_sums,
    specificity = (trace - diagonal) / (trace - diagonal + row_sums - diagonal), # doesn't count negatives as true if they exclude the class correctly but don't predict the correct class, e.g. y = F and pred = M
    precision = diag(cm) / colSums(cm)
  )
}
