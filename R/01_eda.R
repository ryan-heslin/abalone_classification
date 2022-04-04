abalone <- read.csv(here::here("data", "abalone_raw.csv"))
library(ggplot2)

#' The data for this project relate to measurements
#' of abalone, a species of aquatic snail sometimes
#' eaten as a delicacy. It may be downloaded [here](https://www.kaggle.com/datasets/rodolfomendes/abalone-dataset). There are `r nrow(abalone)`
#' observations of `r ncol(abalone)` rows.
#' The response, variable, `sex`, has three levels: "M", "F", and "I", corresponding to male, female, and infant. Available predictors correspond to measurements of size and weight, in addition to
#' the number of rings. As with trees, counting rings
#' approximates age.


abalone_long <- abalone |> tidyr::pivot_longer(ends_with("weight"), names_to = "Measure", values_to = "Value", names_transform = function(x) gsub("\\..*", "", x))

#' The three classes of `Sex` are about evenly distributed.
table(abalone$Sex)

#' Distributions of dimension measures are quite similar across the classes of `sex`.
abalone |>
  tidyr::pivot_longer(c(Length, Diameter, Height), names_to = "Measure", values_to = "Value") |>
  ggplot(aes(x = Value, fill = Measure)) +
  geom_density(alpha = .5) +
  facet_grid(rows = vars(Measure), vars(Sex), scales = "free_y")

#' The three separate weight variables don't quite sum to total weight, suggesting measurement error.
summary(with(abalone, Whole.weight - Shucked.weight - Viscera.weight - Shell.weight))

#' It seems that weight increases nonlinearly with number of rings. This makes sense, as many organisms grow at a nonlinear rate.
ggplot(abalone_long, aes(x = Rings, y = Value, color = Measure)) +
  geom_point() +
  geom_smooth(se = FALSE)

#' Infants have substantially fewer rings than adults, but adults differ only a little
with(abalone, tapply(Rings, Sex, mean))

#' Adults of both sexes have very similar weight distributions, but infants
#' weigh notably less, as would be expected
ggplot(abalone_long, aes(x = Sex, fill = Sex, y = Value)) +
  geom_violin(alpha = .5) +
  facet_wrap(~Measure, ncol = 1, scales = "free_y")

#' Applying $K$-means clustering with $K = 3$ does
#' not result in well-separated assignments. This is
#' consistent with the major variation occurring between infants and adults,
#' of both sexes, not males and females.
set.seed(1)
clusters <- abalone[, -match("Sex", colnames(abalone))] |>
  scale() |>
  kmeans(centers = 3) |>
  getElement("cluster")

table(clusters, abalone[["Sex"]])
