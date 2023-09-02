library(data.table)
library(dplyr)
library(ggplot2)
require(fixest)
library(rvest)
library(lubridate)
library(stringr)
library(httr)
library(tm)
library(wordcloud)
library(tidytext)
library(edgar)
library(topicmodels)
library(tidyverse)

### read data 
sec <- fread("~/Downloads/Unimelb subject/DAF/asm/group asm 2/sec_master.csv")

#PART 1
  #1.1. Create a list of 5 companies by name and PERMNO: choose company report around December 2022, Jan 2023 => fully capture the eventful periods
        # Verizon, Adobe, Cocacola, IBM, Walmart
        # Verizon belongs to Telecommunication industry - the Fama-French industry assigned to my group for the Week 9 presentation
  
  permno <- c("65875","75510","11308","12490","55976")
  
  chosen_comp <- function(prm){
    company_name <- sec[grepl(prm, PERMNO)]$name %>% unique
    chosen_comp <- data.frame(name=company_name,PERMNO=prm)
    return(chosen_comp)
  }
  
  chosen_comp_list <- lapply(permno, chosen_comp)
  chosen_comp_list <- do.call(rbind, chosen_comp_list)
  chosen_comp_list<- chosen_comp_list[-c(2,6),] #cut multiple name of the company, keep the most updated name
  
  #1.2. Obtain and list links to their annual report (10-K main file) on the SEC EDGAR.
  
  link <- c("https://www.sec.gov/Archives/edgar/data/732712/000073271223000012/vz-20221231.htm",
  "https://www.sec.gov/Archives/edgar/data/796343/000079634323000007/adbe-20221202.htm",
  "https://www.sec.gov/Archives/edgar/data/21344/000002134423000011/ko-20221231.htm",
  "https://www.sec.gov/Archives/edgar/data/51143/000155837023002376/ibm-20221231x10k.htm",
  "https://www.sec.gov/Archives/edgar/data/104169/000010416923000020/wmt-20230131.htm")
  
  chosen_comp_list <- as.data.table(chosen_comp_list)
  chosen_comp_list[,annual_report:=link]
  
  #1.3. Plot cumulative returns for each company (one plot or several plots) between 2012-2022.

  ### load fff
  ffdata <- fread("~/Downloads/Unimelb subject/DAF/practical/data/FF_Portfolios.csv")
  ffdata[, date := ymd(dateff)]
  ffdata[, month := month(date)]
  ffdata[, year := year(date)]
  
  ### load crsp
  crsp <- fread("~/Downloads/Unimelb subject/DAF/practical/data/crsp.csv")
  crsp[, date := ymd(date)]
  crsp[, year := year(date)]
  crsp[, RET := as.numeric(RET)]
  crsp <- crsp[!is.na(RET)]
  
  ### merging data 
  chosen_comp_list$PERMNO <- chosen_comp_list$PERMNO %>% as.character()
  crsp$PERMNO <- crsp$PERMNO %>% as.character()
  chosen_comp <- merge(chosen_comp_list, crsp, by = "PERMNO", all.x = T) #all.x = keeping all crsp data
  chosen_comp <- merge(chosen_comp, ffdata, by = "date", all.x = T)
  
  ##clean table
  chosen_comp <- chosen_comp[,-c(3)]
  chosen_comp$COMNAM <- gsub("WAL MART STORES INC", "WALMART INC", chosen_comp$COMNAM)
  chosen_comp$COMNAM <- gsub("ADOBE SYSTEMS INC", "ADOBE INC", chosen_comp$COMNAM)
  
  #set period
  chosen1 <- chosen_comp %>%  filter(year.y >= 2012 & year.y <= 2022)
  
  #calculate cumulative return for each company 
  # Verizon, Adobe, Cocacola, IBM, Walmart
  verizon_data <- chosen1[PERMNO == "65875"]
  verizon_data <- unique(verizon_data, by = "date") #critical to use this line because there are duplicate data in verizon
  verizon_data[,cumulative_return:=cumprod(1+verizon_data$RET)-1]
  
  adobe_data <- chosen1[PERMNO == "75510"]
  adobe_data <- unique(adobe_data, by = "date")
  adobe_data[,cumulative_return:=cumprod(1+adobe_data$RET)-1]
  
  cocacola_data <- chosen1[PERMNO == "11308"]
  cocacola_data <- unique(cocacola_data, by = "date")
  cocacola_data[,cumulative_return:=cumprod(1+cocacola_data$RET)-1]
  
  IBM_data <- chosen1[PERMNO == "12490"]
  IBM_data <- unique(IBM_data, by = "date") 
  IBM_data[,cumulative_return:=cumprod(1+IBM_data$RET)-1]
 
  Walmart_data <- chosen1[PERMNO == "55976"]
  Walmart_data <- unique(Walmart_data, by = "date")
  Walmart_data[,cumulative_return:=cumprod(1+Walmart_data$RET)-1]

  #plot for each company
  cum1 <- ggplot(data = verizon_data, aes(x = date, y = cumulative_return)) +
    geom_line(color = "steelblue", linewidth = 1) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "Year", y = "Cumulative Return", title = "Cumulative Return of VERIZON COMMUNICATIONS INC") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  cum1
  
  cum2 <- ggplot(data = adobe_data, aes(x = date, y = cumulative_return)) +
    geom_line(color = "steelblue", linewidth = 1) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "Year", y = "Cumulative Return", title = "Cumulative Return of ADOBE INC") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  cum2
  
  cum3 <- ggplot(data = cocacola_data, aes(x = date, y = cumulative_return)) +
    geom_line(color = "steelblue", linewidth = 1) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "Year", y = "Cumulative Return", title = "Cumulative Return of COCA COLA CO") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  cum3
  
  cum4 <- ggplot(data = IBM_data, aes(x = date, y = cumulative_return)) +
    geom_line(color = "steelblue", linewidth = 1) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "Year", y = "Cumulative Return of IBM", title = "Cumulative Return of INTERNATIONAL BUSINESS MACHS COR") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  cum4
  
  cum5 <- ggplot(data = Walmart_data, aes(x = date, y = cumulative_return)) +
    geom_line(color = "steelblue", linewidth = 1) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "Year", y = "Cumulative Return", title = "Cumulative Return of WALMART INC") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  cum5
  
  #1.4. Calculate the three-year rolling CAPM beta for the specified periods
  
  #set period
  chosen2 <- chosen_comp %>%  filter(year.y >= 2008 & year.y <= 2022)
  
  #excess return
  chosen2[, retrf := RET-rf]
  
  # Verizon, Adobe, Cocacola, IBM, Walmart data
  verizon_data2 <- chosen2[PERMNO == "65875"]
  verizon_data2 <- unique(verizon_data2, by = "date") #critical to use this line because there is duplicate data in verizon
  
  adobe_data2 <- chosen2[PERMNO == "75510"]
  adobe_data2 <- unique(adobe_data2, by = "date")
  
  cocacola_data2 <- chosen2[PERMNO == "11308"]
  cocacola_data2 <- unique(cocacola_data2, by = "date")
  
  IBM_data2 <- chosen2[PERMNO == "12490"]
  IBM_data2 <- unique(IBM_data2, by = "date") #critical to use this line because there is duplicate data in IBM
  
  Walmart_data2 <- chosen2[PERMNO == "55976"]
  Walmart_data2 <- unique(Walmart_data2, by = "date")
 
  ### function to get beta 
  get_3yr_beta <- function(row, data) {
    # Extract the three-year window
    subset_data <- data[(row - 37):(row-1), ] #start from Jan 2012 is at row 49, so Dec 2008 is at row 49-37=12 and Dec 2011 is at row 49-1=48
    # Perform the CAPM regression without intercept
    fit <- feols(retrf ~ mktrf, data = subset_data)
    
    # Get the beta coefficient
    beta <- fit$coefficients[2]
    
    # Return the result
    return(beta)
  }
  
  #Jan 2012 is at row 49, Dec 2022 is at row 180 
  
  #get beta for each company
  verizon_beta <- NULL
  verizon_beta <- sapply(49:180, get_3yr_beta, data = verizon_data2)
  adobe_beta <- NULL
  adobe_beta <- sapply(49:180, get_3yr_beta, data = adobe_data2)
  cocacola_beta <- NULL
  cocacola_beta <- sapply(49:180, get_3yr_beta, data = cocacola_data2)
  IBM_beta <- NULL
  IBM_beta <- sapply(49:180, get_3yr_beta, data = IBM_data2)
  Walmart_beta <- NULL
  Walmart_beta <- sapply(49:180, get_3yr_beta, data = Walmart_data2)

  beta_3yr <- data.frame(date=verizon_data2$date[49:180],verizon_beta=verizon_beta,adobe_beta=adobe_beta,cocacola_beta=cocacola_beta,IBM_beta=IBM_beta,Walmart_beta=Walmart_beta)
  
  #plot for each company
  beta3 <- ggplot(data = beta_3yr, aes(x = date)) +
    geom_line(aes(y = verizon_beta, color = "VERIZON COMMUNICATIONS INC"), linewidth = 1) +
    geom_line(aes(y = adobe_beta, color = "ADOBE INC"), linewidth = 1) +
    geom_line(aes(y = cocacola_beta, color = "COCA COLA CO"), linewidth = 1) +
    geom_line(aes(y = IBM_beta, color = "INTERNATIONAL BUSINESS MACHS COR"), linewidth = 1) +
    geom_line(aes(y = Walmart_beta, color = "WALMART INC"), linewidth = 1) +
    scale_y_continuous(limits = c(-0.5, 2)) +
    labs(x = "Year", y = "Beta", title = "Three-year rolling CAPM beta") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    scale_color_manual(
      values = c("VERIZON COMMUNICATIONS INC" = "steelblue",
                 "ADOBE INC" = "purple",
                 "COCA COLA CO" = "yellow",
                 "INTERNATIONAL BUSINESS MACHS COR" = "green",
                 "WALMART INC" = "pink"),
      labels = c("ADOBE INC",
                 "COCA COLA CO",
                 "INTERNATIONAL BUSINESS MACHS COR",
                 "VERIZON COMMUNICATIONS INC",
                 "WALMART INC")
    ) +
    labs(color = "Company") +
    guides(color = guide_legend(title = "Company"))
  beta3
  
#PART 2: Sentiment analysis
  #2.1. Download annual reports for these companies.
  
  link <- c("https://www.sec.gov/Archives/edgar/data/732712/000073271223000012/vz-20221231.htm",
            "https://www.sec.gov/Archives/edgar/data/796343/000079634323000007/adbe-20221202.htm",
            "https://www.sec.gov/Archives/edgar/data/21344/000002134423000011/ko-20221231.htm",
            "https://www.sec.gov/Archives/edgar/data/51143/000155837023002376/ibm-20221231x10k.htm",
            "https://www.sec.gov/Archives/edgar/data/104169/000010416923000020/wmt-20230131.htm")
  

  ### download for each company
  download.file(link[1],"verizon. html",
                headers = c ("User-Agent" = "Lucy Nguyen lucyng@gmail.com")) #should use different name because of different IP
  verizon_page <- read_html("verizon. html")
  
  download.file(link[2],"adobe. html",
                headers = c ("User-Agent" = "Lucy Nguyen lucyng@gmail.com")) #should use different name because of different IP
  adobe_page <- read_html("adobe. html")
  
  download.file(link[3],"cocacola. html",
                headers = c ("User-Agent" = "Lucy Nguyen lucyng@gmail.com")) #should use different name because of different IP
  cocacola_page <- read_html("cocacola. html")
  
  download.file(link[4],"IBM. html",
                headers = c ("User-Agent" = "Lucy Nguyen lucyng@gmail.com")) #should use different name because of different IP
  IBM_page <- read_html("IBM. html")
  
  download.file(link[5],"Walmart. html",
                headers = c ("User-Agent" = "Lucy Nguyen lucyng@gmail.com")) #should use different name because of different IP
  Walmart_page <- read_html("Walmart. html")
  
  #2.2. Convert html to text, extract words, and then filter English words.
  
  #Extract text paragraphs from page_content

  ###extract text  
  verizon_paragraphs <- verizon_page %>%
    html_node("body") %>% 
    html_nodes("div") %>%
    html_text()
  
  adobe_paragraphs <- adobe_page %>% 
    html_node("body") %>% 
    html_nodes("div") %>% 
    html_text()
  
  cocacola_paragraphs <- cocacola_page %>% 
    html_node("body") %>% 
    html_nodes("div") %>% 
    html_text()
  
  IBM_paragraphs <- IBM_page %>% 
    html_node("body") %>% 
    html_nodes("div") %>% 
    html_text()
  
  
  Walmart_paragraphs <- Walmart_page %>% 
    html_node("body") %>% 
    html_nodes("div") %>% 
    html_text()
  
  #prepare text function
  prepare_text <- function(text) {
    library(qdapDictionaries)
    library(stringr)
    library(tm)
    require(NLP)
    
    # Convert text to lowercase
    text <- tolower(text)
    
    # Extract words from text using regular expression
    text <- str_extract_all(text, "\\w+")
    
    # Unlist the extracted words
    text <- unlist(text)
    
    # Filter English words 
    dict <- c(GradyAugmented)
    text <- text[text %in% dict & str_length(text) > 2 & !text %in% stopwords("english")]
  }
  
  verizon.english.words <- lapply(verizon_paragraphs, prepare_text) %>% unlist
  adobe.english.words <- lapply(adobe_paragraphs, prepare_text) %>% unlist
  cocacola.english.words <- lapply(cocacola_paragraphs, prepare_text) %>% unlist
  IBM.english.words <- lapply(IBM_paragraphs, prepare_text) %>% unlist
  Walmart.english.words <- lapply(Walmart_paragraphs, prepare_text) %>% unlist
  
  # 2.3 English words are in each annual report
  
  # Convert all.words to a data.table
  verizon.english.words <- as.data.table(verizon.english.words)
  adobe.english.words <- as.data.table(adobe.english.words)
  cocacola.english.words <- as.data.table(cocacola.english.words)
  IBM.english.words <- as.data.table(IBM.english.words)
  Walmart.english.words <- as.data.table(Walmart.english.words)
  
  
  word_counts <- data.frame(
    Company = c("VERIZON COMMUNICATIONS INC", "ADOBE INC", "COCA COLA CO", "INTERNATIONAL BUSINESS MACHS COR", "WALMART INC"),
    English_Words = c(nrow(verizon.english.words), nrow(adobe.english.words), nrow(cocacola.english.words), nrow(IBM.english.words), nrow(Walmart.english.words)))

  #2.4. Plot a wordcloud of most common words
  
  # Set column name to "word"
  setnames(verizon.english.words, "word")
  setnames(adobe.english.words, "word")
  setnames(cocacola.english.words, "word")
  setnames(IBM.english.words, "word")
  setnames(Walmart.english.words, "word")
  
  # Calculate word frequencies and order by count (N) in descending order
  verizon.top.words <- verizon.english.words[, .N, by = word][order(-N)]
  adobe.top.words <- adobe.english.words[, .N, by = word][order(-N)]
  cocacola.top.words <- cocacola.english.words[, .N, by = word][order(-N)]
  IBM.top.words <- IBM.english.words[, .N, by = word][order(-N)]
  Walmart.top.words <- Walmart.english.words[, .N, by = word][order(-N)]
  
  # Display the top 100 most common words wordcloud for each company
  #function to get wordcloud
  com_wordcloud <- function(top.words){
    library(wordcloud)
    com_wordcloud <- wordcloud(words = top.words$word[1:100],
                                   freq = top.words$N[1:100],
                                   scale = c(0.1, 0.6),
                                   colors = top.words$N[1:100])
    return(com_wordcloud)
  }
  #wordcloud for each com
  verizon_wordcloud <- com_wordcloud(verizon.top.words)
  adobe_wordcloud <- com_wordcloud(adobe.top.words)
  cocacola_wordcloud <- com_wordcloud(cocacola.top.words)
  IBM_wordcloud <- com_wordcloud(IBM.top.words)
  Walmart_wordcloud <- com_wordcloud(Walmart.top.words)
   
   # Combine English word data frames for each company
   all_words <- rbind(
     verizon.english.words[, .N, by = word],
     adobe.english.words[, .N, by = word],
     cocacola.english.words[, .N, by = word],
     IBM.english.words[, .N, by = word],
     Walmart.english.words[, .N, by = word]
   )
   
   # Calculate frequency count and order the data frame
   all_top_words <- all_words[, .(N = sum(N)), by = word][order(-N)]
   
   # Create a word cloud for all comp
   allcom_wordcloud <- com_wordcloud(all_top_words)
     
   #2.5. Plot seven sentiments for each company
    #The sentiment categories are: negative, positive, uncertainty, litigious, strong modal, weak modal, and constraining.
   
    #Download Loughran and McDonald dictionaries
     loughran_mcdonald_dicts  <- get_sentiments("loughran")
   
    #function to crate plot
     sentiment_plot <- function(top.words){
       sent.data <- NULL
       
       # for loop for each sentiment category
       for(sent in unique(loughran_mcdonald_dicts$sentiment)){
         # filter by sentiment category
         dict <- loughran_mcdonald_dicts %>% filter(sentiment == sent)
         # % of sentiment category for all words
         prc.words <- sum(top.words[word %in% dict$word]$N)/sum(top.words$N)
         # put in table format
         sent.data <- sent.data %>%
           rbind(data.table(type = sent, sentiment = prc.words))
       }
       
       sent.data$type <- factor(sent.data$type, levels = sent.data$type[1:length(sent.data$type)])
       
       library(ggplot2)
       library(RColorBrewer)
       ggplot(data = sent.data, 
              aes(x = type, y = sentiment, fill = type)) +  
         geom_bar(stat = "identity") +
         scale_y_continuous(labels = scales::percent) + 
         scale_fill_brewer(palette = "Set2") +
         xlab("Type") + ylab("Sentiment") + 
         theme_minimal() + 
         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none") 
       
     }
   #plot for each com
   verizon_sentiment_plot <- sentiment_plot(verizon.top.words) + ggtitle("Seven Sentiments for VERIZON COMMUNICATIONS INC")
   adobe_sentiment_plot <- sentiment_plot(adobe.top.words) + ggtitle("Seven Sentiments for ADOBE INC")
   cocacola_sentiment_plot <- sentiment_plot(cocacola.top.words) + ggtitle("Seven Sentiments for COCA COLA CO")
   IBM_sentiment_plot <- sentiment_plot(IBM.top.words) + ggtitle("Seven Sentiments for INTERNATIONAL BUSINESS MACHS COR")
   Walmart_sentiment_plot <- sentiment_plot(Walmart.top.words) + ggtitle("Seven Sentiments for WALMART INC")
   
  
#PART 3: Topic modelling
  #3.1. Split company’s annual reports into smaller paragraphs
 
   # For Verizon
   small_para <- function(page){
     paragraph <- page %>%
       html_node("body") %>% 
       html_nodes("div") %>% 
       #html_nodes("span") %>%
       html_text()
     
     small_paragraphs <- data.table(text = unlist(paragraph))
     small_paragraphs <- small_paragraphs[str_length(text) > 100]
     small_paragraphs[, index := 1:.N]
     return(small_paragraphs)
   }
   
   verizon_small_paragraphs <- small_para(verizon_page)
   adobe_small_paragraphs <- small_para(adobe_page)
   cocacola_small_paragraphs <- small_para(cocacola_page)
   IBM_small_paragraphs <- small_para(IBM_page)
   Walmart_small_paragraphs <- small_para(Walmart_page)
  
   #3.2. Prepare texts for LDA analysis for each company
   
   #dictionary
   dict <- c(GradyAugmented, "covid")
   
   #function to clean
   library(stringr)
   library(tidytext)
   
   clean_par <- function(data_frame, dictionary) {
     # Convert text to lowercase
     data_frame$text <- tolower(data_frame$text)
     
     # Split the text in each row
     data_frame[, text := str_split(text, "\\s+")]
     
     # Extract words from text using regular expression
     data_frame[, text := sapply(text, function(sentence) str_extract_all(sentence, "\\w+"))]
     
     # Filter rows based on dictionary
     data_frame[, text := lapply(text, function(words) {
       filtered_words <- words[words %in% dictionary & stringr::str_length(words) > 2 & !words %in% stopwords("english")]
       if (length(filtered_words) > 0) {
         return(filtered_words)
       } else {
         return(NA_character_)
       }
     })]
     
     # Remove rows with NA values in the text column
     data_frame <- data_frame[!is.na(text)]
     
     # Reset the index column
     data_frame[, index := .I]
     
     # Group words into sentences in each row
     data_frame[, text := sapply(text, function(words) str_c(words, collapse = " "))]
     
     return(data_frame)
   }
   
   verizon_small_paragraphs <- clean_par(verizon_small_paragraphs,dict)
   adobe_small_paragraphs <- clean_par(adobe_small_paragraphs,dict)
   cocacola_small_paragraphs <- clean_par(cocacola_small_paragraphs,dict)
   IBM_small_paragraphs <- clean_par(IBM_small_paragraphs,dict)
   Walmart_small_paragraphs <- clean_par(Walmart_small_paragraphs,dict)
   
   
   # Create a Document-Term Matrix (DTM)
   
   # create DTM formula 
   company_dtm <- function(companydt){
     library(topicmodels)
     
     dtm <- companydt$text %>%
       VectorSource() %>% # Convert the text to a vector source
       Corpus() %>% # Convert the vector source to a corpus
       DocumentTermMatrix() # Create the document-term matrix
     rownames(dtm) <- companydt$index
     return(dtm)
   }
   verizon_dtm <- company_dtm(verizon_small_paragraphs)
   adobe_dtm <- company_dtm(adobe_small_paragraphs)
   cocacola_dtm <- company_dtm(cocacola_small_paragraphs)
   IBM_dtm <- company_dtm(IBM_small_paragraphs)
   Walmart_dtm <- company_dtm(Walmart_small_paragraphs)
  

   
   #3.3. Perform a topic modeling analysis using the LDA
   
   set.seed(598) # Set the seed for reproducibility
   
   # Find the best number of topics
   
   num_topic <- function(dtm){
     library(knitr) 
     library(kableExtra) 
     library(DT)
     library(tm)
     library(topicmodels)
     library(reshape2)
     library(ggplot2)
     library(wordcloud)
     library(pals)
     library(SnowballC)
     library(lda)
     library(ldatuning)
     library(flextable)
     library(topicmodels)
     
   result <- ldatuning::FindTopicsNumber(
     dtm,
     topics = seq(from = 2, to = 10, by = 1),
     metrics = c("CaoJuan2009"),
     method = "Gibbs",
     control = list(seed = 77),
     verbose = TRUE
   )
   FindTopicsNumber_plot(result) %>% return()
   }
   
   verizon.result <- num_topic(verizon_dtm) #For verizon: 3 
   adobe.result <- num_topic(adobe_dtm) #For adobe: 2 
   cocacola.result <- num_topic(cocacola_dtm) #For cocacola: 3 
   IBM.result <- num_topic(IBM_dtm)  #For IBM: 2
   Walmart.result <- num_topic(Walmart_dtm) #For Walmart: 8
   
   
   # Set the parameters for the LDA model
   LDAmodel <- function(dtm,k){
     burnin <- 100
     iter <- 1000
     thin <- 100
     
     ldaOut <- LDA(dtm, k, method = "Gibbs", 
                   control = list(burnin = burnin, iter = iter, thin = thin, keep = iter))
     return(ldaOut)
   }
   
   # Using lowest CaoJuan2009 values for k
   verizon_ldaOut <- LDAmodel(verizon_dtm,k=3)
   adobe_ldaOut <- LDAmodel(adobe_dtm,k=2)
   cocacola_ldaOut <- LDAmodel(cocacola_dtm,k=3)
   IBM_ldaOut <- LDAmodel(IBM_dtm,k=2)
   Walmart_ldaOut <- LDAmodel(Walmart_dtm,k=8)
   
  
   #3.4. Plot LDA topics 
   
   # Wordclould
       
   
   # Wordclould
   
   library(wordcloud)
   library(data.table)
   
   generate_WordClouds <- function(k, ldaOut, company) {
     # Get the topic frequencies
     topic.freq <- ldaOut %>%
       tidy(matrix = "beta") %>%  
       arrange(desc(beta)) %>%  
       group_by(topic) %>%  
       slice(1:100) %>%  
       data.table()  
     
     # Generate word cloud for each topic
     for (i in 1:k) {
       topic_words <- topic.freq[topic == i]$term
       topic_freq <- topic.freq[topic == i]$beta
       topic_col <- topic.freq[topic == i]$beta*1000
       
       # Save word cloud as PNG
       png(file = paste("Part 3.4 - ", company, "Topic", i, "wordcloud.png"),width = 800, height = 600)
       wordcloud(words = topic_words,
                 freq = topic_freq,
                 scale = c(1, 0.3),
                 colors = topic_col,
                 main = paste("Verizon Topic", i))
       dev.off()
     }
   }
   
   generate_WordClouds(3, verizon_ldaOut, "VERIZON")
   generate_WordClouds(2, adobe_ldaOut, "ADOBE")
   generate_WordClouds(3, cocacola_ldaOut, "COCACOLA")
   generate_WordClouds(2, IBM_ldaOut, "IBM")
   generate_WordClouds(8, Walmart_ldaOut, "Walmart")
   
   
   # Plot the topic frequencies
       #function
       top.freq_plot <- function(ldaOut){
         
         topic.freq <- tidytext::tidy(ldaOut, matrix = "beta") %>%
           arrange(desc(beta)) %>% 
           group_by(topic) %>% 
           slice(1:10) %>%
           data.table
         
         # Create term labels with leading zeros based on the term frequency
         topic.freq[, term.label := paste0(topic, ".", str_pad(.N:1, 2, pad = "0"), ".", term), by = topic]
         
         
         plot <- ggplot(topic.freq, aes(x = term.label, y = beta, fill = factor(topic))) +
           geom_bar(stat = "identity", show.legend = FALSE) +
           coord_flip() +
           facet_wrap(~ topic, scales = "free", ncol = 2) +
           theme_minimal() +
           xlab("Term") +
           ylab("Frequency") +
           scale_y_continuous(labels = scales::percent)
         return(plot)
       }
       
       #plot for each company
       verizon.top.freq_plot <- top.freq_plot(verizon_ldaOut) + ggtitle("Topic frequencies of VERIZON COMMUNICATIONS INC")
       adobe.top.freq_plot <- top.freq_plot(adobe_ldaOut) + ggtitle("Topic frequencies of ADOBE INC")
       cocacola.top.freq_plot <- top.freq_plot(cocacola_ldaOut) + ggtitle("Topic frequencies of COCA COLA CO")
       IBM.top.freq_plot <- top.freq_plot(IBM_ldaOut) + ggtitle("Topic frequencies of INTERNATIONAL BUSINESS MACHS COR")
       Walmart.top.freq_plot <- top.freq_plot(Walmart_ldaOut) + ggtitle("Topic frequencies of WALMART INC")
   
    #3.5. Average Frequency
   
   #function  
   visualize_topic_distribution <- function(ldaOut, small_paragraphs, company_name) {
     
     # Extract topic frequencies
     topic_freq <- tidy(ldaOut, matrix = "beta") %>%
       arrange(desc(beta)) %>% 
       group_by(topic) %>% 
       slice(1:10) %>%
       data.table
     
     # Generate topic labels
     topic_label <- topic_freq[, list(term = paste0(term, collapse = " ")), by = topic]
     topic_label[, term := paste0(topic, ".", term)]
     
     # Calculate topic probabilities
     topic_prob <- data.table(ldaOut@gamma)
     colnames(topic_prob) <- paste0(1:dim(ldaOut@gamma)[2])
     topic_prob$index <- small_paragraphs$index
     topic_prob <- topic_prob %>% 
       melt(id.vars = "index") %>% 
       group_by(variable) %>%
       summarise(value = sum(value)) %>%
       mutate(value = value / sum(value))  # Calculate the percentage
     
     # Match topic labels
     topic_prob$label <- topic_label$term[match(topic_prob$variable, topic_label$topic)]
     
     # Reorder y-axis labels in reverse order
     topic_prob$variable <- factor(topic_prob$variable, levels = rev(topic_prob$variable))
     
     # Create the plot
     plot <- ggplot(topic_prob, aes(y = variable, x = value, fill = label)) +
       geom_col(width = 0.6) +
       geom_text(aes(label = paste0(round(value * 100), "%")), 
                 position = position_dodge(width = 0.6), 
                 hjust = -0.2,
                 vjust = -0.5,
                 size = 3) +
       labs(x = paste0("Average Frequency of each topic in ", company_name, "'s Annual Report"), y = "Topic", fill = "Topic") +
       theme_minimal() +
       scale_fill_brewer(palette = "Paired") +
       theme(axis.text.x = element_blank(),
             axis.text.y = element_text(size = 10, color = "black"),
             axis.ticks.y = element_blank(),
             axis.title.x = element_text(size = 12, color = "black", face = "bold"),
             axis.title.y = element_text(size = 12, color = "black", face = "bold"),
             panel.grid.major.y = element_blank(),
             legend.position = "bottom",
             legend.title = element_text(size = 12, color = "black", face = "bold"),
             legend.text = element_text(size = 10, color = "black"),
             legend.key = element_rect(color = "black", size = 0.5),
             plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),
             plot.subtitle = element_text(size = 12, color = "black", hjust = 0.5)) +
       guides(fill = guide_legend(ncol = 1))
     
     return(plot)
   }
   
   #average frequency plot of each company
   verizon_avg_freq_plot <- visualize_topic_distribution(verizon_ldaOut, verizon_small_paragraphs, "VERIZON COMMUNICATIONS INC")
   adobe_avg_freq_plot <- visualize_topic_distribution(adobe_ldaOut, adobe_small_paragraphs, "ADOBE INC")
   cocacola_avg_freq_plot <- visualize_topic_distribution(cocacola_ldaOut, cocacola_small_paragraphs, "COCA COLA CO")
   IBM_avg_freq_plot <- visualize_topic_distribution(IBM_ldaOut, IBM_small_paragraphs, "INTERNATIONAL BUSINESS MACHS COR")
   Walmart_avg_freq_plot <- visualize_topic_distribution(Walmart_ldaOut, Walmart_small_paragraphs, "WALMART INC")

   
   