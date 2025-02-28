#' ---
#' title: "`r params$title`"
#' author: "Ryan Heslin, Chenxi Liao, Sebastian Zovko"
#' date: "`r format(Sys.Date(), '%B %e, %Y')`"
#' output:
#'   beamer_presentation:
#'     theme: "Ilmenau"
#'     colortheme: "beaver"
#'     highlight: "kate"
#'     includes:
#'       in_header: "../data/presentation_preamble.tex"
#'     df_print: "kable"
#' params:
#'   title: "Classifying Abalone Sex"
#' urlcolor: "blue"
#' ---
#'
#' ```{r setup, include=FALSE}
#'  knitr::opts_chunk$set(
#'  echo = FALSE,
#'  comment = "",
#'  fig.pos = "",
#'  message = FALSE,
#'  tidy = "styler",
#'  out.width = "70%",
#'  out.height = "70%",
#'  warning = FALSE,
#'  fig.align = "center",
#'  highlight = TRUE
#' )
#' options(digits = 3)
#' ```


#' # Introduction
#' The goal of this analysis was to create an inferential model of the sex of abalone, a type of saltwater mollusk. This is a classification problem with three classes: infant, (adult) male, and (adult) female. Avaliable features include:

#' \begin{itemize}
#' \item Measurements of abalone dimensions (length, diameter, height)
#' \item The weights of different parts of the abalone, as well as the entire animal
#' \item The number of rings of the abalone (abalone grow rings as they age)
#' \end{itemize}

#' # More on the Problem

#' Wild abalone populations have collapsed from overharvesting, but they are farmed commercially for their meat and pearls.
#' Predicting sex is of interest because the eggs of adult female abalone are useful to breeders and scientists (Bradley, 2010).
abalone <- read.csv(here::here("data", "abalone_raw.csv"))
source(here::here("R", "utils.R"))
library(ggplot2)
theme_set(theme_minimal())
invisible()

#' # Available Predictors
#' \begin{itemize}
#' \item Length
#' \item Diameter
#' \item Height
#' \item Weight
#' \begin{itemize}
#' \item Whole
#' \item Viscera
#' \item Shucked
#' \item Shell
#' \end{itemize}
#' \item Rings (roughly corresponds to abalone age)
#' \end{itemize}

#' # Abalone Weight by Sex
#' Infant abalone weigh substantially less than adults, but male and female adult abalone weigh about the same. This illustrates the main challenge of this classification task: infant abalone are easy to distinguish from adults, but male and female adults are hard to distinguish from each other.
invisible()

#' # Abalone Weight by Sex Visualized
abalone_long <- abalone |> tidyr::pivot_longer(ends_with("weight"), names_to = "Measure", values_to = "Value", names_transform = function(x) gsub("\\..*", "", x))
ggplot(abalone_long, aes(x = Sex, fill = Sex, y = Value)) +
  geom_violin(alpha = .5) +
  facet_wrap(~Measure, ncol = 1, scales = "free_y") +
  labs(title = "Abalone Weight by Sex", y = "Weight")

#' # The Data in Detail
#' The full data have `r nrow(abalone)` observations. The dataset (Kaggle, 2020) is standard in machine learning research. It may be obtained [here](https://www.kaggle.com/datasets/rodolfomendes/abalone-dataset).
#' The three separate weight variables summed almost exactly to total weight, suggesting measurement error.
#' Still, including all three would be a bad idea because it would make the model matrix nearly singular. We selected only one to use. This table summarizes the difference of whole weight and the sum of the other weight variables.
summary(with(abalone, Whole.weight - Shucked.weight - Viscera.weight - Shell.weight)) |>
  as.list() |>
  list2DF()

#+ results="hide"
library(nnet)
abalone[["Sex"]] <- factor(abalone[["Sex"]], levels = c("I", "F", "M"))
set.seed(12345)
train_i <- sample(nrow(abalone), floor(nrow(abalone) * 0.8), replace = FALSE)
train <- abalone[train_i, ]
test <- abalone[-train_i, ]
initial_model <- multinom(Sex ~
Whole.weight + Length + Diameter + Height + Rings, data = train)

#' # General Approach
#' First, we split the data, reserving 80% of observations for a training set and the remaining 20% for a test set, after setting a random seed to ensure reproducibility.
#' We fit an initial $J-1$  (here $J = 3$) logits baseline model using sex, dimensions,
#' and whole weight as predictors. This model is appropriate for nominal outcomes, which abalone sex is because it has no logical ordering.
#' This model consists of $J-1$ logit models, each predicting the log odds of a given class versus a baseline class.
#' There are ${J \choose 2}$ possible comparisons, but all can be derived algebraically from just $J-1$ logits.
#' We choose infants as the reference class because they differ in the same way from each adult class (i.e., are younger).
invisible()

#' # Initial Model Fitting and Data Partitioning
broom::tidy(initial_model)[, -4]

#+ results ="hide"
null_model <- update(initial_model, . ~ . - .)
#' # Overall Likelihood Ratio Test of Initial Model
#' The overall likelihood ratio test of the initial model was highly significant, so we rejected the null hypothesis of no linear relationship
out <- as.data.frame(anova(null_model, initial_model))
out[["Model"]] <- c("Null", "Full")
out

#' # Wald Tests of Coefficients
#' Most of the estimated coefficients were
#' significant under Wald tests, even after applying
#' the Bonferroni correction to $p$-values.

summarized <- broom::tidy(initial_model)
summarized[["p.value"]] <- p.adjust(summarized[["p.value"]],
  method = "bonferroni"
)
summarized[1:6, -4]

rich <- generate_rich_model(initial_model,
  model_data = train[, colnames(coef(initial_model))[-1]],
  model_formula = Sex ~ Whole.weight + Length + Diameter + Height + Rings
)

#' # Choosing a Model by Stepwise Selection
#' We used stepwise selection by AIC to choose a model with a few interactions. The upper scope included all
#' pairwise interactions and quadratic variable terms.
#+ results = "hide"
step_model <- suppressMessages(step(initial_model, scope = list(
  upper = rich,
  lower = Sex ~ Whole.weight + Length + Diameter + Height + Rings
)))
step_residual <- extract_by_level(
  residuals(step_model, "std.res"),
  as.integer(train[["Sex"]])
)
step_fitted <- extract_by_level(
  step_model[["fitted.values"]],
  as.integer(train[["Sex"]])
)

#' # The Stepwise-Selected Model
#' Only variables for the female class are shown.
broom::tidy(step_model)[1:9, -4]

#' # Residuals by Class
#' Average standardized residuals were notably lower for infants than for adults, consistent with infants being easier to classify.
ggplot(
  data.frame(step_fitted, step_residual,
    Sex = train[["Sex"]],
    yintercept = ave(step_residual, train[["Sex"]], FUN = mean)
  ),
  aes(x = seq_len(nrow(train)), y = step_residual, color = Sex)
) +
  geom_point(alpha = .1, size = 1.5) +
  geom_hline(aes(yintercept = yintercept, color = Sex),
    size = 2, show.legend = FALSE
  ) +
  theme_minimal() +
  labs(
    x = "Case",
    y = "Studentized Residual",
    title = "Studentized Residuals by True Class",
    subtitle = "Horizontal lines indicate average residuals by class"
  )
ggsave(here::here("figure", "student-resid.jpg"))

#' # Delta Chi-Square vs. Predicted Probability
step_delta_chisq <- extract_by_level(
  residuals(step_model, "pearson"),
  as.integer(train[["Sex"]])
)^2
ggplot(data.frame(step_fitted, step_delta_chisq, Class = train[["Sex"]]), aes(x = step_fitted, y = step_delta_chisq)) +
  geom_smooth(se = FALSE) +
  geom_point(aes(color = Class))

#' # Model Testing
#' A likelihood-ratio test of the step-selected model over the initial model is highly significant. A goodness-of-fit test for the step-selected model has a p-value that is computationally 1, providing no evidence against the null of a good fit

lr_test(initial_model, step_model)

#' # Type II ANOVA
#' A type II ANOVA test shows that all predictors aside from `diameter` significantly improve the fit when included. `Whole.weight` is by far the most important, followed by `Rings` and the interactions.
as.data.frame(car::Anova(step_model))

#' # Comparing Different Weight Variables
#' Next, we tried refitting the model with each of all four available weight variables (including replacing the interaction) and comparing AIC. (AIC has no inherent interpretation, but is useful when comparing variants of the same model). Whole weight has the lowest, but the differences are minor.
weight_vars <- c("Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight")
names(weight_vars) <- weight_vars
weight_vars <- lapply(weight_vars, as.symbol)

#+ results = "hide"
aics <- lapply(weight_vars, function(x) {
  update_formula(step_model, Whole.weight, x, indirect = TRUE)[["AIC"]]
}) |>
  list2DF()

aics

train_preds <- predict(step_model, newdata = train, type = "class")

#' # Class Separation by Height and Width
#' Plotting predicted class by height and width shows again that infants are well separated from adults.

ggplot(
  data.frame(train,
    Predicted = train_preds,
    Accuracy = ifelse(train_preds == train[["Sex"]],
      "Correct", "Incorrect"
    )
  ),
  aes(x = Length, y = Height, color = Predicted, shape = Accuracy)
) +
  lims(
    x = quantile(probs = c(0, .95), train[["Length"]]),
    y = quantile(probs = c(0, .95), train[["Height"]])
  ) +
  geom_jitter(alpha = .5) +
  labs(title = "Predicted Class by Height and Width")

#' # Comparison with Binary Classification

#' It was obvious that distinguising infants from adults was much easier than distinguising male from female adults.
#' For comparison, we combined the male and female classes into "adult" and fit a binomial model.
train[["Sex_binary"]] <- factor(ifelse(train[["Sex"]] == "I", "I", "A"),
  levels = c("I", "A")
)
new_formula <- eval(substitute(substitute(new_formula, list(Sex = quote(Sex_binary))), list(new_formula = formula(step_model))))
binary_model <- glm(
  formula = new_formula, data = train,
  family = binomial(link = logit)
)
broom::tidy(binary_model)[, -4]

#' # Deviance Comparison
#' While a direct likelihood ratio test is inappropriate because these models use different versions of
#' the response, it is worth noting this binary model
#' had less than half the deviance of the step-selected model.
c(
  "Binary Deviance" = deviance(binary_model),
  "Three-Class Deviance" = deviance(step_model)
) |>
  as.list() |>
  list2DF()

#' # Model Validation

#' On both training and testing sets, sensitivity, specificity, and precision were much higher for the infant than the adult classes. However, test error
#' was only a little higher than train error.  Still,
#' overall test accuracy was above 50%, much better than the 36% (the highest class proportion) achieved by the naive classifier.
#+ results = "asis"
train_cm <- confusion_matrix(train[["Sex"]], train_preds)
as.data.frame(train_cm)
cat("Overall training set accuracy was ", mean(train_preds == train[["Sex"]]), ".", sep = "\n")

#' # Training Set Class-Specific Metrics
test_preds <- predict(step_model, newdata = test, type = "class")
as.data.frame(analyze_cm(train_cm))

#' # Test Set Results
#' Results on the test set were very similar, suggesting minimal generalization error. Overall test accuracy was `r mean(test_preds == test[["Sex"]])`.
test_cm <- confusion_matrix(test[["Sex"]], test_preds)
as.data.frame(test_cm)

#' # Test Set Class-Specific Metrics
as.data.frame(analyze_cm(test_cm))

#' # Test-Train Comparison and Overall Accuracy
#' The difference of the two confusion matrices shows that test and train performance were similar.
as.data.frame(analyze_cm(train_cm) - analyze_cm(test_cm))

#' # Conclusion
#' Male and female adult abalone are difficult to distinguish using predictors avaliable in this dataset. Using a $J-1$ logits baseline model with interaction terms, approximately 50% classification accuracy was achieved with minimal generalization error.
#' However, infant abalone are smaller than adults of either sex, making them significantly easier to
#' classify accurately. Future research should focus on finding easily measured predictors with which the adult sexes are easily distinguished.
#' Published research tends to focus on classifying abalone age in years (e.g, Abdelbar [1998]). For the reason noted above, this is in some ways an easier problem than classifying sex.
invisible(NULL)

#' # References

#' Abalone Dataset. (n.d.). [2022]. Kaggle. Retrieved April 15, 2022, from https://www.kaggle.com/rodolfomendes/abalone-dataset
#'
#' Abdelbar, A. M. (1998). Achieving superior generalisation with a high order neural network. Neural Computing and Applications, 7, 141–147.
#'
#' Bradley, R. (2010, March 9). How to Sex an Abalone: A Sea Snail’s Story. The Atlantic. https://www.theatlantic.com/health/archive/2010/03/how-to-sex-an-abalone-a-sea-snails-story/37198/
