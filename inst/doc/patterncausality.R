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
  theme_bw(base_size = 12, base_family = "Times New Roman") +
  xlab("Time") +
  ylab("Stock Price") +
  theme(
    legend.position = c(0.25, 0.85), legend.box.background = element_rect(fill = NA, color = "black", linetype = 1), legend.key = element_blank(),
    legend.title = element_blank(), legend.background = element_blank(), axis.text = element_text(size = rel(0.8)),
    strip.text = element_text(size = rel(0.8))
  ) +
  scale_color_manual(values = c("#DC143C", "#191970"))

## ----eval=FALSE---------------------------------------------------------------
#  dataset <- DJS[,-1]
#  parameter <- optimalParametersSearch(Emax = 5, tauMax = 5, metric = "euclidean", dataset = dataset)

## -----------------------------------------------------------------------------
X <- DJS$Apple
Y <- DJS$Microsoft
pc <- pcLightweight(X, Y, E = 3, tau = 2, metric = "euclidean", h = 1, weighted = TRUE, tpb=FALSE)
print(pc)

## -----------------------------------------------------------------------------
library(ggplot2)
df <- data.frame(
  name = stringr::str_to_title(c(colnames(pc))),
  val = as.vector(unlist(pc))
)

ggplot(df, aes(x = name, y = val, fill = name)) +
  geom_bar(stat = "identity", alpha = .6, width = .4) +
  scale_fill_grey(start = 0, end = 0.8) + # start and end define the range of grays
  labs(x = "Status", y = "Strength") +
  theme_bw(base_size = 12, base_family = "Times New Roman") +
  theme(
    legend.position = "none", axis.text = element_text(size = rel(0.8)),
    strip.text = element_text(size = rel(0.8))
  )

## ----eval=FALSE---------------------------------------------------------------
#  X <- DJS$Apple
#  Y <- DJS$Microsoft
#  detail <- pcFullDetails(X, Y, E = 3, tau = 2, metric = "euclidean", h = 1, weighted = TRUE)
#  predict_status <- detail$spectrumOfCausalityPredicted
#  real_status <- detail$spectrumOfCausalityReal
#  names(detail)

