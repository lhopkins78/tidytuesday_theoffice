#library
library(schrute)
library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(patchwork)

#get data
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

#a dead end
office_trigrams <- theoffice %>% unnest_tokens(trigram,text, token="ngrams", n=3)

trigrams_separated <- office_trigrams %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

trigram_counts <- trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)

#get sentiment lexicons
nrc <- get_sentiments("nrc")
afinn <- get_sentiments("afinn")

#get words from script text
office_words <- theoffice %>% 
  unnest_tokens(word,text) %>% select(season, episode, character, imdb_rating, word) %>%
  left_join(y=nrc, by = "word")

#get sentiments
office_sentiments <- office_words %>% group_by(character, sentiment) %>% tally() %>% spread(key=sentiment, value=n) %>%
  mutate(total=sum(anger,anticipation,disgust,fear,joy,negative,positive,sadness,surprise,trust,na.rm=T),
         neg=sum(anger,disgust,negative,fear,sadness,na.rm=T)/total) %>%
  arrange(desc(total)) %>% head(20) 

#colours and levels
cols <- brewer.pal(n=10, name="RdYlBu")
sent_levels <- c("anger","disgust","negative","fear","sadness","anticipation", "surprise",
                 "positive","trust", "joy")

#character sentiments (emotional)
p1 <- ggplot(office_sentiments %>% gather(key="sentiment", value="value", 2:11), aes(x=reorder(character,neg), y=value, fill=factor(sentiment,levels=sent_levels))) +
         geom_bar(stat="identity", position="fill") + coord_flip() + scale_fill_manual(values=cols) +
        labs(y="Proportion of words spoken", x="", title="Always look on the bright side of life", subtitle="Characters by emotional sentiment of lines", fill="Emotional sentiment")

#attach AFINN sentiment values
office_words_afinn <- theoffice %>% 
  unnest_tokens(word,text) %>% select(season, episode, imdb_rating, word) %>%
  left_join(y=afinn, by = "word")

office_sentiments_af <- office_words_afinn %>% group_by(season, episode, imdb_rating) %>% summarise(sentiment=mean(value, na.rm=T))

#vectors for mean horizontal/vertical lines
imdb_mean <- office_sentiments_af %>% group_by(season) %>% summarise(mean_imdb=mean(imdb_rating,na.rm=T), mean_sent=mean(sentiment, na.rm=T))

#sentiment by season
p2 <- ggplot(office_sentiments_af, aes(x=sentiment, y=imdb_rating)) + geom_point(alpha=0.5) +
  facet_wrap(~season, nrow=1) + theme_minimal() + geom_hline(data=imdb_mean, aes(yintercept=mean_imdb), size=3) +
  geom_vline(data=imdb_mean, aes(xintercept=mean_sent), col="orange") +
  geom_vline(xintercept=min(imdb_mean$mean_sent), col="darkred")+
  labs(title="Mean sentiment and IMDB by season", subtitle="Red line is lowest average season sentiment (season 6), orange is average season sentiment",
       y="IMDB rating", x="Mean positive/negative sentiment (AFINN scale)")

#sentiment and IMDB relationship
p3 <- ggplot(office_sentiments_af, aes(x=sentiment, y=imdb_rating)) + geom_point(alpha=0.5) + geom_smooth(col="orange", se=F) +
   theme_minimal() + xlim(c(0.2,1.0)) + labs(title="Be positive, but not too positive - The Office (US)", y="IMDB rating", x="Mean positive/negative sentiment (AFINN scale)",
                                             subtitle="Mean positive/negative sentiment of episode and IMDB rating, seasons 1-9")

office_plot <- (p1 + p3) / p2 + plot_annotation("Tidy Tuesday - The Office - Words and Numbers", 
                                                caption="Source: schrute package @bradlindblad, The Office IMDB Ratings Dataset @anujjain7",
                                                theme = theme(plot.title = element_text(size = 16)))

ggsave("office_tuesday.png", plot=office_plot, dpi="retina", width=15)






