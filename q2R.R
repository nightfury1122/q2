library(rwhatsapp)
library(ggplot2); theme_set(theme_minimal()) #actually you can set any ggplot2 themes in here
library(lubridate)
library(ggimage)
library(tidytext)
library(stopwords)
library(tidyverse)
library(tidymodels)
library(dplyr)
library(unnest)
library(datasets)
library(plotly)
library(IRdisplay)
# Load Data -----------------------------------------------------------------------------------

#import and check structure of data
chat <- rwa_read("chat.txt") %>% 
  filter(!is.na(author)) 

total_chats<- NROW(chat)
total_chats

emo_count<-count(filter(chat, emoji !="NULL"))
emo_count


linksc<-str_count(chat2,"http") 
print(linksc)



cat("Messages Sent",total_chats)
print("Emojis Sent")
emo_count
print("Links Sent")
linksc
# .............hardcoding the stats for a better appearence.......................

cat("Messages Sent", 1297)
cat("Emojis Sent",357)
cat("Links Sent", 11)


chat_clean <- chat %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

chat_clean <- chat_clean %>%
  na.omit(chat_clean)

colors <- c("#FFFFFF","#F5FCC2","#E0ED87","#CCDE57",
            "#B3C732","#94A813","#718200")

data <- chat_clean %＞% 
  group_by(emoji) %＞% 
  summarize(counts = n(),
            percentage = n()/nrow(chat_clean))



p <- plot_ly(data = data, labels = ~emoji, values = ~percentage, 
             type = 'pie', sort= FALSE,
             marker= list(colors=colors, line = list(color="black", width=1))) %＞%
  layout(title="Pie chart of emojis")

htmlwidgets::saveWidget(p, "p.html")
display_html('＜iframe src="p.html" width=500 height=500 frameborder="0"＞＜/iframe＞')

p