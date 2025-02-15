## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----message = FALSE----------------------------------------------------------
library(patterncausality)
data(DJS)
head(DJS)

## ----echo=FALSE---------------------------------------------------------------
library(ggplot2)
library(ggthemes)
df <- data.frame(
  Date = as.Date(DJS$Date),
  Value = c(
    DJS$Apple,
    DJS$Microsoft
  ),
  Type = c(
    rep("Apple", dim(DJS)[1]),
    rep("Microsoft", dim(DJS)[1])
  )
)
ggplot(df) +
  geom_line(aes(Date, Value, group = Type, colour = Type), linewidth = 0.4) +
  theme_few(base_size = 12) +
  xlab("Time") +
  ylab("Stock Price") +
  theme(
    legend.position = "bottom", legend.box.background = element_rect(fill = NA, color = "black", linetype = 1), legend.key = element_blank(),
    legend.title = element_blank(), legend.background = element_blank(), axis.text = element_text(size = rel(0.8)),
    strip.text = element_text(size = rel(0.8))
  ) +
  scale_color_manual(values = c("#DC143C", "#191970"))

## ----eval=FALSE---------------------------------------------------------------
# dataset <- DJS[,-1]
# parameter <- optimalParametersSearch(Emax = 5, tauMax = 5, metric = "euclidean", dataset = dataset)

## -----------------------------------------------------------------------------
X <- DJS$Apple
Y <- DJS$Microsoft
pc <- pcLightweight(X, Y, E = 3, tau = 2, metric = "euclidean", h = 1, weighted = TRUE)
print(pc)

## -----------------------------------------------------------------------------
plot_total(pc)
plot_components(pc)

## ----eval=FALSE---------------------------------------------------------------
# X <- DJS$Apple
# Y <- DJS$Microsoft
# detail <- pcFullDetails(X, Y, E = 3, tau = 2, metric = "euclidean", h = 1, weighted = TRUE)
# # Access the causality components
# causality_real <- detail$causality_real
# causality_pred <- detail$causality_pred
# print(causality_pred)

## -----------------------------------------------------------------------------
# Example with both weighted and relative TRUE
pc_rel_weighted <- pcLightweight(X, Y, E = 3, tau = 2, metric = "euclidean", 
                               h = 1, weighted = TRUE, relative = TRUE)
print(pc_rel_weighted)

