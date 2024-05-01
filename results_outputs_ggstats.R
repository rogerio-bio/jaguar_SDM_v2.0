library (ggstatsplot)
library (ggplot2)
library (dplyr)
library (patchwork)
library (smplot2)
library (purrr)

#Import the data
data = read.csv(file.choose(), header=TRUE, sep=",")
head(data)

# Count the number of models for each distinct "dist"
model_counts <- data %>%
  group_by(DIST) %>%
  summarise(Count = n())

print(model_counts)

set.seed(7)  
# Use only when testing reg_group
data <- data %>%
  filter(reg_group != "5")

# Sample 14 models between groups
data  <- data  %>%
  group_by(reg_group) %>%
  sample_n(14)

# Shapiro test
data %>%
  group_by(Model) %>%
  nest() %>%
  mutate(shapiro_test = map(data, ~shapiro.test(.$DIST)))

# Suponha que 'final_results' seja a tabela que você deseja exportar
final_results <- resultados_shapiro %>%
  rowwise() %>%
  mutate(
    p_value = shapiro_test$p.value,  # Extrai o valor-p de cada teste
    statistic = shapiro_test$statistic  # Extrai a estatística de teste
  ) %>%
  select(Dist, p_value, statistic)  # Seleciona apenas as colunas relevantes

# Shapiro results
write.csv(final_results, "shapiro_dist_Omission.csv", row.names = FALSE)


#Use ggstatsplot to compare metrics

#AUC
ggbetweenstats(
  data = data,
  x    = reg_group, 
  y    = AUC,
  type = "nonparametric", 
  p.adjust.method = "holm",
  pairwise.display = "s",
  #violin.args = list(width = 0),
  xlab = "Distance",
  ylab = "AUC",
  package = "ggprism",
  palette = "colors", 
  ggplot.component = list(theme(plot.subtitle = element_text(size = 12, face = "bold")))
)

#TSS
ggbetweenstats(
  data = data,
  x    = reg_group, 
  y    = TSS,
  type = "nonparametric", 
  p.adjust.method = "holm",
  pairwise.display = "s",
  #violin.args = list(width = 0),
  xlab = "Distance",
  ylab = "TSS",
  package = "ggprism",
  palette = "colors")

#Omission
ggbetweenstats(
  data = data,
  x    = reg_group, 
  y    = Omission,
  type = "nonparametric", 
  p.adjust.method = "holm",
  pairwise.display = "s",
  #violin.args = list(width = 0),
  xlab = "Distance",
  ylab = "Omission",
  package = "ggprism",
  palette = "colors")

#PPI
ggbetweenstats(
  data = data,
  x    = reg_group, 
  y    = PPI,
  type = "nonparametric", 
  p.adjust.method = "holm",
  pairwise.display = "s",
  #violin.args = list(width = 0),
  xlab = "Distance",
  ylab = "PPI",
  package = "ggprism",
  palette = "colors")


#Create the union of the 2 plots
uni <- AUC/TSS/OM/PPI
uni

#Use patchwork to combine the plots
TSS + 
  theme(text = element_text(size = 14))
