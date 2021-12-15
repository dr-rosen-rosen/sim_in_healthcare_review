library(here)
library(config)
library(tidyverse)
library(revtools)
library(stm)
library(furrr)
debuggingState(on = FALSE)
Sys.setenv(R_CONFIG_ACTIVE = "default")
config <- config::get()
source(here("1_funcs.R"), echo = TRUE)

ref_df <- get_references()
revtools::screen_topics(ref_df)

#### 
ref_df$cmb <- paste(ref_df$title, ref_df$abstract)

# process the data
Processed_ref_data<-textProcessor(ref_df$cmb, metadata=ref_df)

# structure the data for usage in further analyses and verify formatting is correct (and no missing values, etc.)
out<-prepDocuments(Processed_ref_data$documents, Processed_ref_data$vocab, Processed_ref_data$meta)
docs<-out$documents
vocab<-out$vocab
meta<-out$meta

#inspect how many words/documents are removed by adjusting threshold
plotRemoved(Processed_ref_data$documents, lower.thresh=seq(1,200, by=100))

k_range = c(5,10,15,20,25,30,35,40)
kResult <- searchK(out$documents, out$vocab, K=k_range, #prevalence=~role,
                   data=meta)
plot(kResult)

k_30_title <- stm(
  documents = out$documents,
  vocab = out$vocab,
  K = 30,
  # prevalence = ,
  max.em.its = 75,
  data = out$meta,
  init.type = 'Spectral'
)
labelTopics(k_30_title)
plot(k_30_title)
findThoughts(k_30_title)

### Implementing from here: https://juliasilge.com/blog/evaluating-stm/

many_models <- data.frame(
  K = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) %>%
  mutate(
    topic_model = future_map(K, ~stm(
      documents = out$documents, 
      vocab = out$vocab,
      K = ., 
      verbose = FALSE)))

#NOT EDITED....
heldout <- stm::make.heldout(
  documents = out$documents,
  vocab = out$vocab)
k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, out$documents),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, out$documents),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL#,
       #title = "Model diagnostics by number of topics",
       #subtitle = "These diagnostics indicate that a good number of topics would be around 60"
       )

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(40, 50, 60)) %>%
  unnest(cols = c(exclusivity, semantic_coherence)) %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")

topic_model <- many_models %>% 
  filter(K == 40) %>% 
  pull(topic_model) %>% 
  .[[1]]
stm::plot.STM(topic_model)
stm::plotQuote(topic_model)
labelTopics(topic_model)

td_gamma <- broom::tidy(topic_model, matrix = "beta",
                       document_names = rownames(out$documents))

td_gamma