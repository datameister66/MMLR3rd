install.packages("ggplot2")
install.packages("ggraph")
install.packages("ggthemes")
install.packages("igraph")
install.packages("quanteda")
install.packages("qdap")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("SnowballC")
install.packages("sotu")
install.packages("tm")
install.packages("topicmodels")

library(magrittr)
library(sotu)

txt <- c("The regular early morning yell of horror was the sound of Authur", 
          "Dent waking up and suddenly remembering where he was. It wasnt", 
          "just that the cave was cold, it wasnt just that it was damp and", 
          "smelly. It was the fact that the cave was in the middle of",
         "Islington and there wasnt a bus due for two million years.") 
writeLines(txt, tf <- tempfile()) 
install.packages("text2vec")
install.packages("reticulate")
install.packages("cleanNLP")
library(cleanNLP) 
cleanNLP::cnlp_init_tokenizers() 
anno <- cleanNLP::cnlp_annotate(tf) 
names(anno)
cleanNLP::cnlp_get_token(anno) 
# python -m spacy download en # run as administrator
cleanNLP::cnlp_init_spacy() 
anno <- cleanNLP::cnlp_annotate(tf) 
cleanNLP::cnlp_get_token(anno) 

data(obama)
cleanNLP::cnlp_get_document(obama)
cleanNLP::cnlp_get_token(obama)
cnlp_get_dependency(obama, get_token = TRUE) 
cnlp_get_entity(obama)
# cnlp_get_coreference(obama) not supported
cnlp_get_sentence(obama) # nothing
dim(cnlp_get_token(obama))
dim(cnlp_get_vector(obama)) # nothing

library(sotu)
data(sotu_text) 
data(sotu_meta) 
sotu <- cleanNLP::cnlp_annotate(sotu_text, as_strings = TRUE, meta = sotu_meta) 
saveRDS(sotu, "sotuRData")
sotu <- readRDS("sotuRData")
cleanNLP::cnlp_get_token(sotu) %>% 
   dplyr::filter(upos == "NOUN") %>% 
   dplyr::count(lemma) %>% 
   dplyr::top_n(n = 10, n) %>% 
   dplyr::arrange(desc(n)) %>%
   use_series(lemma) # magrittr

cleanNLP::cnlp_get_token(sotu) %>% 
  dplyr::count(id) -> sotu_token 

cleanNLP::cnlp_get_document(sotu) -> sotu_docid

sotu_meta2 <- sotu_docid %>%
  dplyr::select(id) %>%
  dplyr::bind_cols(sotu_meta) %>%
  dplyr::inner_join(sotu_token)
  
ggplot2::ggplot(sotu_meta2, ggplot2::aes(year, n)) +
  ggplot2::geom_line(color = grey(0.8)) +
  ggplot2::geom_point(ggplot2::aes(color = sotu_type)) + 
  ggplot2::geom_smooth() 

cleanNLP::cnlp_get_entity(sotu) %>% 
  dplyr::filter(entity_type == "GPE") %>% 
  dplyr::count(entity) %>% 
  dplyr::top_n(n = 20, n) %>% 
  dplyr::arrange(desc(n)) -> ner_20

cleanNLP::cnlp_get_dependency(sotu, get_token = TRUE) -> sotu_dep

sotu_dep %>%
  dplyr::left_join(dplyr::select(sotu_meta2, year, id), by = "id") -> sotu_obj

sotu_obj %>%
dplyr::filter(year == 2001, relation == "dobj") %>% 
  dplyr::select(id = id, start = word, word = lemma_target) %>% 
  dplyr::left_join(word_frequency) %>% 
  dplyr::filter(frequency < 0.001) %>% 
  dplyr::select(id, start, word) 

tfidf <- cleanNLP::cnlp_get_token(sotu) %>%
  dplyr::filter(pos %in% c("NN", "NNS")) %>%
  cleanNLP::cnlp_utils_tfidf(
    min_df = 0.05,
    max_df = 0.95,
    type = "tfidf",
    tf_weight = "dnorm"
  ) 

cleanNLP::cnlp_utils_pca(tfidf, sotu_meta2) -> sotu_pca

library(topicmodels)
tm <- cleanNLP::cnlp_get_token(sotu) %>%
  dplyr::filter(pos %in% c("NN", "NNS")) %>%
  cleanNLP::cnlp_utils_tfidf(
    min_df = 0.05,
    max_df = 0.95,
    type = "tf",
    tf_weight = "raw"
  )

set.seed(666)
topicmodels::LDA(tm, k = 25, method = "Gibbs", control = list(verbose = 1)) -> tmodels
tmodels
topics(tmodels)
terms(tmodels, 10)

dplyr::filter(sotu_meta2, id == "doc69" | id=="doc70" | id == "doc71" | id=="doc72")
dplyr::filter(sotu_dep, id == "doc72")

readr::write_csv(sotu_meta2, "sout_meta2.csv")

#tidytext ###################
library(magrittr)
library(sotu)
data(sotu_text) 
data(sotu_meta) 
sotu_meta$text <- sotu_text
colnames(sotu_meta)

sotu_meta %>%
  tidytext::unnest_tokens(word, text) -> sotu_unnest
# words, n-grams, sentences
sotu_unnest 

library(tidytext)
data(stop_words)
sotu_tidy <- sotu_unnest %>%
  dplyr::anti_join(stop_words, by = "word")

sotu_tidy %>%
  dplyr::count(word, sort = TRUE)

sotu_tidy %>%
  dplyr::count(word, sort = TRUE) %>%
  dplyr::filter(n > 2500) %>%
  dplyr::mutate(word = reorder(word, n)) %>%
  ggplot2::ggplot(ggplot2::aes(word, n)) +
  ggplot2::geom_col() +
  ggplot2::xlab(NULL) +
  ggplot2::coord_flip() +
  ggthemes::theme_igray()

sotu_tidy %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(totalWords = length(word)) %>%
  dplyr::arrange(desc(totalWords))

sotu_tidy %>%
  dplyr::filter(year > 1860 & year < 1865) %>%
  dplyr::count(word, sort = TRUE)

# wordcloud::wordcloud(lincoln_words$word, lincoln_words$n, max.words = 20, use.r.layout = TRUE)

sotu_cloud <- sotu_tidy %>%
  dplyr::filter(year > 1860 & year < 1865)

qdap::trans_cloud(
  text.var = sotu_cloud$word,
  grouping.var = sotu_cloud$year,
  stem = FALSE,
  min.freq = 7
)

# nest and conduct word_associate?
nested_1862 <- sotu_tidy %>%
  dplyr::filter(year == 1862) %>%
  dplyr::select(year, word) %>%
  tidyr::nest(word) %>%
  dplyr::mutate(
    text = purrr::map(data, unlist),
    text = purrr::map_chr(text, paste, collapse = " ")
  )
# kwic?
# nested_1862 <- dplyr::filter(sotu_meta, year == 1862)
myCorpus <- tm::Corpus(tm::VectorSource(nested_1862$text))
quanteda::kwic(x = myCorpus$content, pattern = "emancipation", window = 7)

# 
# sotu_tidy %>%
#   dplyr::filter(year > 1860 & year < 1865) %>%
#   dplyr::count(year, word, sort = TRUE) %>%
#   dplyr::ungroup() -> year_words
# 
# year_words %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarize(total = sum(n)) -> total_words
# 
# year_words <- dplyr::left_join(year_words, total_words)
# 
# year_words <- year_words %>%
#   tidytext::bind_tf_idf(year, word, n)
# 
# year_words %>%
#   dplyr::select(-total) %>%
#   dplyr::arrange(desc(tf_idf))

# year_words %>%
#   dplyr::filter(year > 1965 & year < 1969) %>%
#   dplyr::arrange(desc(tf_idf)) %>%
#   dplyr::mutate(word = factor(word, levels = rev(unique(word)))) %>%
#   dplyr::group_by(year) %>%
#   dplyr::top_n(3) %>%
#   dplyr::ungroup() %>%
#   ggplot2::ggplot(ggplot2::aes(word, tf_idf, fill = year)) +
#   ggplot2::geom_col(show.legend = FALSE) +
#   ggplot2::labs(x = NULL, y = "tf-idf") +
#   ggplot2::facet_wrap(~ year, ncol = 2, scales = "free") +
#   ggplot2::coord_flip()

table(sentiments$lexicon)

#The nrc lexicon categorizes words in a binary fashion (“yes”/“no”) into categories of positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust. 
#The bing lexicon categorizes words in a binary fashion into positive and negative categories. 
#The AFINN lexicon assigns words with a score that runs between -5 and 5, with negative scores indicating negative sentiment and positive scores indicating positive sentiment

get_sentiments("nrc")

nrc_anger <- tidytext::get_sentiments("nrc") %>% 
  dplyr::filter(sentiment == "anger")
# table(nrc_joy$sentiment)

sotu_tidy %>%
  dplyr::filter(year == 1862) %>%
  dplyr::inner_join(nrc_anger) %>%
  dplyr::count(word, sort = TRUE)

sentiment <- sotu_tidy %>%
  dplyr::inner_join(tidytext::get_sentiments("bing")) %>%
  dplyr::filter(year > 1852 & year <1873) %>%
  dplyr::count(president, year, sentiment) %>%
  tidyr::spread(sentiment, n, fill = 0) %>%
  dplyr::mutate(sentiment = positive - negative) %>%
  dplyr::arrange(year)

ggplot2::ggplot(sentiment, ggplot2::aes(year, sentiment, fill = president)) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::facet_wrap(~ president, ncol = 2, scales = "free_x") +
  ggthemes::theme_pander()

sotu_tidy %>%
  dplyr::inner_join(tidytext::get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  dplyr::ungroup()

# n-grams
sotu_bigrams <- sotu_meta %>%
  dplyr::filter(year > 1860 & year < 1865) %>%
  tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2, to_lower = FALSE)

sotu_bigrams %>%
  dplyr::count(bigram, sort = TRUE)

bigrams_separated <- sotu_bigrams %>%
  tidyr::separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  dplyr::filter(!word1 %in% stop_words$word) %>%
  dplyr::filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  dplyr::count(word1, word2, sort = TRUE)

bigram_counts
# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  dplyr::filter(n > 4) %>%
  igraph::graph_from_data_frame()

# set.seed(1861)

ggraph::ggraph(bigram_graph, layout = "fr") +
  ggraph::geom_edge_link() +
  ggraph::geom_node_point() +
  ggraph::geom_node_text(ggplot2::aes(label = name), vjust = 1, hjust = 1)

# topicmodeling
sotu_meta[185:191, 1:4]

sotu_meta[188, 2] <- "1972_2"

sotu_meta[190, 2] <- "1974_2"

sotu_meta[157, 2] <- "1945_2"

sotu_meta[166, 2] <- "1953_2"

sotu_meta[170, 2] <- "1956_2"
sotu_meta[176, 2] <- "1961_2"
sotu_meta[195, 2] <- "1978_2"
sotu_meta[197, 2] <- "1979_2"
sotu_meta[199, 2] <- "1980_2"
sotu_meta[201, 2] <- "1981_2"

sotu_meta_recent <- sotu_meta %>%
  dplyr::filter(year > 1964)

sotu_meta_recent %>%
  tidytext::unnest_tokens(word, text) -> sotu_unnest_recent

# sotu_unnest_recent

sotu_recent <- sotu_unnest_recent %>%
  dplyr::anti_join(stop_words, by = "word")

sotu_recent %>%
  dplyr::group_by(year) %>%
  dplyr::count(word) -> lda_words
# colnames(lda_words)
sotu_dtm <- tidytext::cast_dtm(lda_words, year, word, n)

sotu_lda <-
  topicmodels::LDA(
    sotu_dtm,
    k = 6,
    method = "Gibbs",
    control = list(seed = 1965, verbose = 1)
  )
sotu_lda  

topicmodels::topics(sotu_lda)
table(topicmodels::topics(sotu_lda))
topicmodels::terms(sotu_lda, 5)

lda_topics <- tidytext::tidy(sotu_lda, matrix = "beta")
# lda_topics
ap_top_terms <- lda_topics %>%
  dplyr::group_by(topic) %>%
  dplyr::top_n(10, beta) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(topic, -beta)
# table(ap_top_terms$topic, ap_top_terms$term)
ap_top_terms %>%
  dplyr::mutate(term = reorder(term, beta)) %>%
  ggplot2::ggplot(ggplot2::aes(term, beta, fill = factor(topic))) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::facet_wrap(~ topic, scales = "free") +
  ggplot2::coord_flip() +
  ggthemes::theme_economist_white()

ap_documents <- tidytext::tidy(sotu_lda, matrix = "gamma")

dplyr::filter(ap_documents, document == "1981")

# Classification
 library(text2vec)
# data("movie_review")
# mr <- movie_review              
# str(mr) # dataframe
 colnames(sentences)
 sentences$id <- seq(1, 889, 1)
 data.table::setDT(sentences)
 data.table::setkey(sentences, id)
 
 all_ids = sentences$id
 set.seed(2017)
 train_ids = sample(all_ids, 700)
 train_ids = 1:400
 test_ids = setdiff(all_ids, train_ids)
 test_ids = 401:889
 train = sentences[J(train_ids)]
 test = sentences[J(test_ids)]
# 
 prep_fun = tolower
 tok_fun = word_tokenizer
 it_train = itoken(train$speech, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun, 
                   ids = train$id, 
                   progressbar = FALSE)
 vocab = create_vocabulary(it_train)
# 
 vectorizer = vocab_vectorizer(vocab)
# t1 = Sys.time()
 dtm_train = create_dtm(it_train, vectorizer)
# print(difftime(Sys.time(), t1, units = 'sec'))
# 
 dim(dtm_train)
# identical(rownames(dtm_train), train$id)
 dtmMat <- as.matrix(dtm_train)
# 
 dtmMat[1:10, 1:10] # compare to tidy


 sentences %>%
   tidytext::unnest_tokens(word, speech) -> df

 df %>%
   dplyr::group_by(id) %>%
   dplyr::count(word) %>%
   dplyr::ungroup() -> df1
 
 df <- df[df$word != "", ]
 
 sotu_wide <- df1 %>% 
   tidyr::spread(word, n, fill = 0)  
 
trn <- sotu_wide[id %in% train_ids,]
trndtm <- tidytext::cast_dtm(df, term=word, document=id)
trnmat <- as.matrix(trn)
trnmat[1:10, 1:10]
# tidy classification -----------------------------------------------------
sotu_party <- sotu_meta %>%
  dplyr::filter(year > 1899)

sotu_party %>%
  tidytext::unnest_tokens(word, text) -> sotu_unnest_party

# sotu_unnest_party

sotu_tidy_party <- sotu_unnest_party %>%
  dplyr::anti_join(stop_words, by = "word")

table(sotu_party$party)

sotu_tidy_party$word <- tm::removeNumbers(sotu_tidy_party$word)
sotu_tidy_party$word <- tm::removePunctuation(sotu_tidy_party$word)
sotu_tidy_party$word <- tm::stemDocument(sotu_tidy_party$word)

sotu_tidy_party %>%
  dplyr::group_by(year) %>%
  dplyr::count(word) %>%
  dplyr::ungroup() -> df1

df1 <- df1[df1$word != "", ]

sotu_wide <- df1 %>% 
  tidyr::spread(word, n, fill = 0) 

sotu_wide[1:2, 9000:9005]
  
sotu_class <-  sotu_wide[, colSums(sotu_wide) > 3]

#
sotu_class$party <- sotu_party$party

set.seed(222)
index <- caret::createDataPartition(sotu_class$party, p = 0.8, list = F)
train <- sotu_class[index, ]
test <- sotu_class[-index, ]

x <- as.matrix(train[, -5459])
y <- as.factor(train$party)

set.seed(123)
lasso <- glmnet::cv.glmnet(
  x,
  y,
  nfolds = 3,
  type.measure = "auc",
  alpha = 1,
  family = "binomial"
)

plot(lasso)

lasso_test <-
  data.frame(predict(lasso, newx = as.matrix(test[, -5459]), 
                     type = 'response'), s = "lambda.1se")

testY <- as.numeric(ifelse(test$party == "Republican", 1, 0))
Metrics::auc(testY, lasso_test$X1)

classifierplots::density_plot(testY, lasso_test$X1)

# advanced ----------------------------------------------------------------
tr <- paste(readLines("C:/Users/cory/Desktop/data/corpus/tr.txt"), collapse=" ") 
tr <- iconv(tr, "latin1", "ASCII", "") 


library(qdap)
prep_tr <- qdap::qprep(tr) 
prep_tr <- qdap::replace_contraction(prep_tr)
prep_tr <- qdap::rm_stopwords(prep_tr, Top100Words, separate = F)
prep_tr <- qdap::strip(prep_tr, char.keep = c("?", ".", "!")) 
address_tr <- data.frame(speech = prep_tr)
address_tr <- qdap::sentSplit(address_tr, "speech")
address_tr$pres <- "TR"

reagan <- paste(readLines("C:/Users/cory/Desktop/data/corpus/reagan.txt"), collapse=" ") 
reagan <- iconv(reagan, "latin1", "ASCII", "") 
prep_reagan <- qdap::qprep(reagan)
prep_reagan <- qdap::replace_contraction(prep_reagan)
prep_reagan <- qdap::rm_stopwords(prep_reagan, Top100Words, separate = F)
prep_reagan <- qdap::strip(prep_reagan, char.keep = c("?", ".", "!")) 
address_reagan <- data.frame(speech = prep_reagan)
address_reagan <- qdap::sentSplit(address_reagan, "speech")
address_reagan$pres <- "reagan"

sentences <- dplyr::bind_rows(address_tr, address_reagan)
plot(qdap::freq_terms(sentences$speech)) 

wordMat <- qdap::wfm(sentences$speech, sentences$pres)
head(wordMat[order(wordMat[, 1], wordMat[, 2],decreasing = TRUE),])

# qdap::trans_cloud(sentences$speech, sentences$pres, min.freq = 15)

ws <- qdap::word_stats(sentences$speech, sentences$pres, rm.incomplete = T)
# plot(ws)
ws$word.elem
ws$sent.elem

pol = qdap::polarity(sentences$speech, sentences$pres)
pol
plot(pol)

pol.df <- pol$all
which.min(pol.df$polarity) 
pol.df$text.var[86]

ari <- qdap::automated_readability_index(sentences$speech, sentences$pres)
ari$Readability

tr_sentences <- dplyr::filter(sentences, pres == "TR")
tr_sentences <- tr_sentences[1:300, ]
qdap::formality(tr_sentences$speech)

reagan_sentences <- dplyr::filter(sentences, pres == "reagan")
formality(reagan_sentences$speech)
#gc()
form$form.prop.by

diversity(sentences$speech, sentences$pres)

dispersion_plot(
  sentences$speech,
  rm.vars = sentences$pres,
  c("peace", "government", "marksmanship"),
  color = "black",
  bg.color = "white"
)
