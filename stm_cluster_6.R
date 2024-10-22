library(stm)
library(tm)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(openxlsx)
library(devtools)
library(topicmodels)
library(tidytext)
library(dplyr)
library(reshape2)
library(readxl)
library(quanteda)
library(stringr)

# 1) Importação dos dados ----
data <- read.xlsx("df3_filtered_cluster_louvain.xlsx")

# Seleção de colunas de interesse 
data <- data[, c("DE", "Cluster")] |>
  filter(!is.na(Cluster)) |>
  rename(documents = DE)

sum(is.na(data$documents))

# 2) Tratamento para text analysis ----
# Substituição de termos ---- 
data$documents <- gsub("internet of thing", "iot", data$documents)
data$documents <- gsub("internet of things", "iot", data$documents)
data$documents <- gsub("internet-of-thing", "iot", data$documents)
data$documents <- gsub("internet-of-things", "iot", data$documents)
data$documents <- gsub("supply chain", "supply_chain", data$documents)

#The function dfm from the quanteda package creates a document-term matrix that 
#can be supplied directly to the stm model fitting function.
data_tokens <- tokens(data$documents, what = "word",
                      remove_numbers = TRUE,
                      remove_punct = TRUE,
                      remove_symbols = TRUE, 
                      split_hyphens = TRUE)

# Lower case the tokens.
data_tokens <- tokens_tolower(data_tokens)

# Use quanteda's built-in stopword list for English.
data_tokens <- tokens_select(data_tokens, 
                             c(stopwords(), "of", "and"), 
                             selection = "remove"
)

# Perform stemming on the tokens.
data_tokens <- tokens_wordstem(data_tokens, language = "english")

# Create our first bag-of-words model.
data_tokens_dfm <- dfm(data_tokens, tolower = FALSE)

# convert to STM
data_stm <-    convert(
  data_tokens_dfm,
  to = c("stm"),
  docvars = data
)

# 3) Ingestão dos dados ----
# Preparação dos documentos, vocabularios distintos e a própria tabela ----

# remove termos infrequentes
out <- prepDocuments(
  data_stm$documents, # apenas coluna alvo do STM (desconsiderando o metadados)
  data_stm$vocab,# vocabularios distintos
  data_stm$meta) # tabela completa inicial



#Testando
storage <- searchK(out$documents,
                   out$vocab,
                   K = c(3:20),
                   N = 245,
                   init.type = 'Spectral',
                   heldout.seed = 042022)


storage$results %>%
  pivot_longer(
    cols = -K,
    names_to = "metric",
    values_to = "value") %>%
  filter(metric %in%
           c("lbound", "exclus", "residual", "semcoh")) %>%
  mutate(value = map_dbl(value, 1)) %>%
  mutate(K = map_dbl(K,1)) %>%
  ggplot(aes(x = K,
             y = value,
             color = metric)) +
  geom_point() +
  geom_line() +
  facet_wrap(~metric, scales = "free_y") +
  theme_minimal() +
  guides(color = "none")

#exclusivity represents the degree to which words are exclusive to a single topic rather than associated with multiple topics
#Exclusive words are more likely to carry topic-relevant content, thus assisting with the interpretation of topics

#O limite inferior variacional (lbound) é a métrica usada para determinar a convergência para uma solução específica.
#Em outras palavras, as funções de estimativa, searchK() e stm(), continuarão a avaliar os modelos até que a mudança no limite inferior
#variacional seja menor do que algum limite designado ou o número máximo de iterações permitidas seja atingido.

#Residual is the estimation of the dispersion of residuals for a particular solution
#Some have recommended looking for local minima, whereas others suggest that
#dispersion greater than one indicates more topics are needed.

#Coerência semântica é uma medida de quão comym as palavras mais prováveis emhttp://127.0.0.1:45065/graphics/plot_zoom_png?width=1228&height=709 um tópico coocorrem

#Ideal solutions yield fewer residuals and higher exclusivity, variational lower bound, and semantic coherence.
#6 tópicos parece ser o ideal


# 4) Modelo ----

## 4.1) Estimação do Modelo ----
stm <- stm(documents = out$documents, 
           vocab = out$vocab, 
           K = 6, 
           prevalence = ~`Cluster`, # criar um vetor de probabilidade para cada documento. Ir de um vetor 1xp com os metadados de cada documento para um vetor 1xk em que os valores somam 1, com cada valor correspondendo a quanto desse documento é pertencente a determinado topic. Para chegar a esse vetor numérico (que soma 1), multiplica-se a matriz de vetor de metadados (1xp) pela matriz de pesos (pxk).
           max.em.its = 30, 
           data = out$meta, 
           init.type = "Spectral" # para tornar reproduzível 
) 

plot(stm, n=10)

# Extract words for each topic
topic_words <- labelTopics(stm, n=10) 

# Convert the list of topic words to a dataframe
df_topic_words <- as.data.frame(do.call(rbind, topic_words), stringsAsFactors = FALSE) |>
  slice(1:6) |>
  tibble::rownames_to_column(var = "x") |>
  tibble::rownames_to_column(var = "topic") |>
  select(-x)

# 5) Tabela de proporção de cada tópico ----
proporcao_topicos_e_principais_termos <- as.data.frame(colSums(stm$theta/nrow(stm$theta))) |>
  tibble::rownames_to_column(var = "topic") |>
  rename(proportion = 2) |>
  mutate(proportion = round(proportion*100,1)) |>
  left_join(df_topic_words, by = 'topic')

# 6) Principais documentos de cada tópico ----

# Indice e documento
documento_indice <- as.data.frame(out$meta$documents, sep=';') |>
  tibble::rownames_to_column(var = "indice") |>
  mutate(indice = as.character(indice)) |>
  rename(documento = 2)

# Indice e cluster
cluster_indice <- as.data.frame(out$meta$Cluster, sep=';') |>
  tibble::rownames_to_column(var = "indice") |>
  mutate(indice = as.character(indice)) |>
  rename(cluster = 2)

# Join
porcentagem_por_doc_e_topico <- make.dt(stm, meta = NULL) |>
  rename(indice = 1) |>
  mutate(indice = as.character(indice)) |>
  left_join(documento_indice, by = 'indice') |>
  left_join(cluster_indice, by = 'indice')

rm(documento_indice)
rm(cluster_indice)

# 7) Número de vezes de um termo ----

termo_num_vezes <- as.data.frame(stm$settings$dim$wcounts[2]) |>
  tibble::rownames_to_column(var = "indice")

termos_analise_descritiva <- as.data.frame(stm$vocab) |>
  tibble::rownames_to_column(var = "indice") |>
  left_join(termo_num_vezes, by ='indice')

rm(termo_num_vezes)

# 8) Visualização 
stm_topics <- tidy(stm)

# The palette with black:
top_terms <- stm_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)%>% 
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder(term, beta)) %>%
  mutate(topic = paste("Topic ", topic)) %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme( strip.background = element_blank())+
  scale_fill_brewer(palette = "Pastel2") +
  coord_flip()

#other options for colours are scale_fill_brewer(palette = "Blues") ou 
#scale_fill_brewer(palette = "Pastel1")

plot(top_terms)
