library(dplyr)
library(tidyr)
library(broom)

# Criando os intervalos para os grupos
intervalos <- c(0.1, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, Inf)
labels <- c("0.1", "0.5", "1.0", "1.5", "2.0", "3.0", "4.0" , "5.0")

# Adicionando a nova coluna com os grupos
data$reg_group <- cut(data$reg, breaks = intervalos, labels = labels, include.lowest = TRUE, right = FALSE)

# Exibindo a planilha com a nova coluna
print(data)

# Escrevendo a planilha para um novo arquivo CSV
write.csv(data, file = "E:/jaguar v4.0/Resultados_rivendell/all_models_rivendell_reg_group.csv", row.names = FALSE)

# Contando o número de modelos em cada grupo
table(data$reg_group)

# Carregar os dados
data <- read.csv(file.choose(), header=TRUE, sep=",")

# Remover grupo 5 devido ao N baixo de modelos
data <- data %>%
  filter(reg_group != "5.0")

# Inicializar um vetor para armazenar os valores-p de cada variável
p_values <- data.frame(AUC = numeric(10000), TSS = numeric(10000),
                       Omission = numeric(10000), PPI = numeric(10000))

# Para reprodutibilidade
set.seed(123) 

for (i in 1:10000) {
  # Fazer a amostragem de bootstrap para cada grupo, mantendo N=14
  bootstrap_sample <- data %>%
    group_by(reg_group) %>%
    sample_n(14, replace = TRUE) %>%
    ungroup()
  
  # Aplicar o teste de Kruskal-Wallis para cada variável de interesse
  for (var in c("AUC", "TSS", "Omission", "PPI")) {
    test_result <- kruskal.test(reformulate("reg_group", response = var), data = bootstrap_sample)
    p_values[i, var] <- test_result$p.value
  }
}

write.csv(p_values, "p_values_reggroups.csv", row.names = T)

# Calcular a média dos valores-p para cada variável de interesse
mean_p_values <- colMeans(p_values)

# Exibir os valores-p médios
print(mean_p_values)
