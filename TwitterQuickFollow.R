# load libraries
library(rtweet)

###Note: These have been complete already
#"@MrZackMorris","@Hugh_Henne","@InvestorsLive","@johnwelshtrades","@TheLioncom","@incrediblebob12,@The_Analyst_81","@PJ_Matlock","@buysellshort","@Amp_Trades","@irdoj75","@AdamHGrimes"



#List of target users from which you want to extract data
users <- list("@atelania", "@wallstreetbets_","@Brady_Atlas","@wsbmod","@MrMikeInvesting","@Manpree31938145","@UncleRyanAZ","@Alexs_trades","@stoolpresidente")


#Lookup friends of target users above and store their twitter IDs in a list
fds_list <- lapply(users, get_friends)


#Get detailed data from the friends of target users stored above in fds_list
fds_data_list <- list() #initialise an empty list

for (i in seq_along(fds_list)) {
  fds_data_list[[i]] <- lookup_users(fds_list[[i]]$user_id)
}

#Follow the friends of target users
for (i in fds_data_list) {
  lapply(i$screen_name, post_follow)
}