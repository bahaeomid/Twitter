# load libraries
library(rtweet)
library(tidyverse)
library(tidytext)
library(plyr)
library(purrr)
library(data.table)
library(lubridate)
library(pheatmap)
library(gridExtra)


###------------------------------
#Search for certain tweets
#tweets <- search_tweets(q = "$XSPA", n = 100,lang = "en")
###------------------------------

#Get recent tweets from target users (max 3200 per user)
users <- list("@MrZackMorris","@Hugh_Henne","@InvestorsLive","@johnwelshtrades","@TheLioncom",
              "@incrediblebob12","@AdamHGrimes","@Amp_Trades","@TraderTexMex",
              "@BioStocks","@MadMraket","@PJ_Matlock","@LadeBackk","@TrendSpider",
              "@holiday613","@petedoom","@IPODave","@atelania", "@wallstreetbets_",
              "@Brady_Atlas","@wsbmod","@MrMikeInvesting","@Manpree31938145","@UncleRyanAZ","@Alexs_trades","@stoolpresidente")

#extract tweets of users above (max n=3200 per user)
Users_list <- lapply(users, get_timeline,n=500)

#remove list elements that are empty.
Users_list <- lapply(Users_list, function(x) {if(sum(dim(x))!=0) x else NULL})
Users_list <- Users_list[!sapply(Users_list,is.null)]


#convert filter out columns of interest)
Syms_list <- lapply(Users_list, select, c("symbols","created_at"))

#convert list to a dataframe
Syms_df <- rbindlist(Syms_list, fill=TRUE)

#drop all NA rows
Syms_df_clean <- Syms_df[!is.na(Syms_df$symbols), ]

#convert local time to UTC so that it matches twitter's default (for use in the rest of the script below)
to_UTC <- Sys.time() 
attributes(to_UTC)$tzone <- "UTC"  

#filter by date/time and store in different variables
Syms_qh <- filter(Syms_df_clean,created_at <= to_UTC & created_at > to_UTC-0.25*60*60)
Syms_hh <- filter(Syms_df_clean,created_at <= to_UTC & created_at > to_UTC-0.5*60*60)
Syms_1h <- filter(Syms_df_clean,created_at <= to_UTC & created_at > to_UTC-1*60*60)
Syms_2h <- filter(Syms_df_clean,created_at <= to_UTC & created_at > to_UTC-2*60*60)
Syms_3h <- filter(Syms_df_clean,created_at <= to_UTC & created_at > to_UTC-3*60*60)
Syms_today <- filter(Syms_df_clean, created_at > Sys.Date())
Syms_3days <- filter(Syms_df_clean, created_at > Sys.Date()-2) 
Syms_7days <- filter(Syms_df_clean, created_at > Sys.Date()-6)
Syms_30days <- filter(Syms_df_clean, created_at > Sys.Date()-29)

#keep the first column only (i.e. symbols)
Syms_qh_final <- unnest(Syms_qh,cols = 1)
Syms_hh_final <- unnest(Syms_hh,cols = 1)
Syms_1h_final <- unnest(Syms_1h,cols = 1)
Syms_2h_final <- unnest(Syms_2h,cols = 1)
Syms_3h_final <- unnest(Syms_3h,cols = 1)
Syms_today_final <- unnest(Syms_today,cols = 1)
Syms_3days_final <- unnest(Syms_3days,cols = 1)
Syms_7days_final <- unnest(Syms_7days,cols = 1)
Syms_30days_final <- unnest(Syms_30days,cols = 1)

#housekeeping: If any of the df's above are empty, create a dummy record in each so merging df's below doesn't run into errors
Syms_final_list <- list(Syms_qh_final, Syms_hh_final, Syms_1h_final, Syms_2h_final, Syms_3h_final, Syms_today_final, Syms_3days_final, Syms_7days_final, Syms_30days_final)
names(Syms_final_list) <- c("Syms_qh_final", "Syms_hh_final", "Syms_1h_final", "Syms_2h_final", "Syms_3h_final", "Syms_today_final", "Syms_3days_final", "Syms_7days_final", "Syms_30days_final")

for (i in seq_along(Syms_final_list)){
  
  if( dim(Syms_final_list[[i]])[[1]]==0 ) {Syms_final_list[[i]] <- tibble(symbols="NA",created_at=Sys.Date())}
}

#set the default count function for use in the script to follow
count_d <- dplyr::count

#tabulate frequencies and filter out the significant ones (i.e. more than 3 tweet mentions)
freq_qh <- Syms_final_list$Syms_qh_final %>% group_by(symbols) %>% count_d(name = "qh") %>% filter(`qh`>=3)
freq_hh <- Syms_final_list$Syms_hh_final %>% group_by(symbols) %>% count_d(name = "hh") %>% filter(`hh`>=3)
freq_1h <- Syms_final_list$Syms_1h_final %>% group_by(symbols) %>% count_d(name = "1h") %>% filter(`1h`>=3)
freq_2h <- Syms_final_list$Syms_2h_final %>% group_by(symbols) %>% count_d(name = "2h") %>% filter(`2h`>=3)
freq_3h <- Syms_final_list$Syms_3h_final %>% group_by(symbols) %>% count_d(name = "3h") %>% filter(`3h`>=3)
freq_today <- Syms_final_list$Syms_today_final %>% group_by(symbols) %>% count_d(name = "today") %>% filter(`today`>=3)
freq_3days <- Syms_final_list$Syms_3days_final %>% group_by(symbols) %>% count_d(name = "3days") %>% filter(`3days`>=3)
freq_7days <- Syms_final_list$Syms_7days_final %>% group_by(symbols) %>% count_d(name = "7days") %>% filter(`7days`>=3)
freq_30days <- Syms_final_list$Syms_30days_final %>% group_by(symbols) %>% count_d(name = "30days") %>% filter(`30days`>=3)

#store all above frequency df's into a list
freq_list <- list(freq_qh,freq_hh,freq_1h,freq_2h,freq_3h,freq_today,freq_3days,freq_7days,freq_30days)

#join all the dataframes in the freq_list above into a single master dataframe
freq_master <- reduce(freq_list,full_join,by='symbols')

##heatmaps of the data
#first transform the data into a matrix
freq_master_matrix <- data.matrix(freq_master)[,-1] #remove the first column which contains symbols
rownames(freq_master_matrix) <- freq_master$symbols #assign symbols to rownames

#define function to flip the order of matrix in descending order for better presentation.
my_order <- function(mat){
  tmp <- mat[dim(mat)[1]:1,]
  return(tmp)
}

#produce heatmaps (2 version for each timeframe - one where data is scaled on the column and one where data is not scaled). Note that TryCatch is used to handle the error in case there's no data to create a heatmap for
hm_month_ns <- tryCatch(
  {
    hm_month_ns <- pheatmap(my_order(freq_master_matrix),scale="none",cluster_rows = F, cluster_cols = F,
                    color = colorRampPalette(c('yellow','red'))(100), border_color= NA,na_col = "white",
                    cellwidth= 20,fontsize_row = 2,fontsize_col = 8, main="Tweets Over the Past 30 Days (No Scale)",silent=T)
  },
  error=function(cond) {
    return(plot.new())
  })


hm_month_col <- tryCatch(
  {
  hm_month_col <- pheatmap(my_order(freq_master_matrix),scale="column",cluster_rows = F, cluster_cols = F,
                    color = colorRampPalette(c('yellow','red'))(100), border_color= NA,na_col = "white",
                    cellwidth= 20,fontsize_row = 2,fontsize_col = 8, main="Tweets Over the Past 30 Days (Scale on Columns)",silent=T)
  },
  error=function(cond) {
   return(plot.new())
  })


hm_week_ns <- tryCatch(
  {
  hm_week_ns <- pheatmap(my_order(freq_master_matrix[rowSums(is.na(freq_master_matrix[,6:8]))!=ncol(freq_master_matrix[,6:8]),6:8]),scale="none",cluster_rows = F, cluster_cols = F,
                        color = colorRampPalette(c('yellow','red'))(100), border_color= NA,na_col = "white",
                        cellwidth= 20,fontsize_row = 4,fontsize_col = 8,main="Tweets Over the past 7 days (No Scale)",silent=T)
  },
  error=function(cond) {
   return(plot.new())
  })


hm_week_col <- tryCatch(
  {
  hm_week_col <- pheatmap(my_order(freq_master_matrix[rowSums(is.na(freq_master_matrix[,6:8]))!=ncol(freq_master_matrix[,6:8]),6:8]),scale="column",cluster_rows = F, cluster_cols = F,
                       color = colorRampPalette(c('yellow','red'))(100), border_color= NA,na_col = "white",
                       cellwidth= 20,fontsize_row = 4,fontsize_col = 8,main="Tweets Over the past 7 days (Scale on Columns)",silent=T)
  },
  error=function(cond) {
   return(plot.new())
  })
  
  
hm_today_ns <- tryCatch(
  {
  hm_today_ns <- pheatmap(my_order(freq_master_matrix[rowSums(is.na(freq_master_matrix[,1:6]))!=ncol(freq_master_matrix[,1:6]),1:6]),scale="none",cluster_rows = F, cluster_cols = F,
                    color = colorRampPalette(c('yellow','red'))(100), border_color= NA,na_col = "white",
                    cellwidth= 20,fontsize_row = 5,fontsize_col = 8,main="Today's Tweets (No Scale)",silent=T)
  },
  error=function(cond) {
  return(plot.new())
  })


hm_today_col <- tryCatch(
  {
  hm_today_col <- pheatmap(my_order(freq_master_matrix[rowSums(is.na(freq_master_matrix[,1:6]))!=ncol(freq_master_matrix[,1:6]),1:6]),scale="column",cluster_rows = F, cluster_cols = F,
                        color = colorRampPalette(c('yellow','red'))(100), border_color= NA,na_col = "white",
                        cellwidth= 20,fontsize_row = 5,fontsize_col = 8,main="Today's Tweets (Scale on Columns)",silent=T)
  },
  error=function(cond) {
   return(plot.new())
  })

#create seperate lists of grobs
hm_today_list <- list(hm_today_ns[[4]],hm_today_col[[4]])
hm_week_list <- list(hm_week_ns[[4]],hm_week_col[[4]])
hm_month_list <- list(hm_month_ns[[4]],hm_month_col[[4]])

#combine all grobs into a single vector
grobs_all <- c(hm_today_list,hm_week_list,hm_month_list)
grobs_all <- Filter(Negate(is.null),grobs_all) #filter out any empty grobs

#output to pdf 
pages <- marrangeGrob(grobs= grobs_all, nrow=1, ncol=2, top = paste0("Print Timestamp: ",Sys.time())) #2plots side by side on each page
ggsave(paste0("C:\\Users\\bahae\\Desktop\\R files\\Twitter\\",Sys.Date(),".pdf"),pages,width = 17, height = 11)

#output table to csv
write_csv(freq_master,paste0("C:\\Users\\bahae\\Desktop\\R files\\Twitter\\",Sys.Date(),".csv"))






