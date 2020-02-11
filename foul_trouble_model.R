library(survival)
library(survminer)

setwd("/Volumes/Seagate Backup Plus Drive/NBA Player Tracking Data/Unzipped JSON files/game1_pbp")

# # Cox model
# foul_situations <- setNames(data.frame(matrix(nrow=0,ncol=6)),c("fouler","num_fouls","total_clock","score_diff","abs_scorediff","time_to_foulout"))
# 
# for (game in list.files()) {
#   print(game)
#   load(game)
# 
#   if (max(game1_pbp$quarter)>4) {
#     game1_pbp$total_clock[game1_pbp$quarter>4] <- game1_pbp$game_clock[game1_pbp$quarter>4]
#   }
#   
#   game1_pbp$hometeam <- is.na(game1_pbp$VISITORDESCRIPTION)
#   game1_pbp$score_change <- FALSE
#   lastscore <- "0 - 0"
#   for (i in 1:nrow(game1_pbp)) {
#     if (!is.na(game1_pbp$SCORE[i])) {
#       lastscore <- game1_pbp$SCORE[i]
#       game1_pbp$score_change[i] <- TRUE
#     } else {
#       game1_pbp$SCORE[i] <- lastscore
#     }
#   }
#   game1_pbp$score_diff <- ifelse(game1_pbp$hometeam, as.numeric(gsub(".*- ","",game1_pbp$SCORE))-as.numeric(gsub(" -.*","",game1_pbp$SCORE)), as.numeric(gsub(" -.*","",game1_pbp$SCORE))-as.numeric(gsub(".*- ","",game1_pbp$SCORE)))
#   game1_pbp$abs_scorediff <- abs(game1_pbp$score_diff)
#   game_fouls_pbp <- game1_pbp[game1_pbp$EVENTMSGTYPE==6 & !(game1_pbp$EVENTMSGACTIONTYPE %in% c(16,17,18)),]
#   
#   game_fouls_pbp$fouler <- ifelse(is.na(game_fouls_pbp$HOMEDESCRIPTION),gsub(" .*","",game_fouls_pbp$VISITORDESCRIPTION),gsub(" .*","",game_fouls_pbp$HOMEDESCRIPTION))
#   game_fouls_pbp$num_fouls <- ifelse(is.na(game_fouls_pbp$HOMEDESCRIPTION),gsub(".*\\(P([0-9]).*", "\\1",game_fouls_pbp$VISITORDESCRIPTION),gsub(".*\\(P([0-9]).*", "\\1",game_fouls_pbp$HOMEDESCRIPTION))
#   
#   fouled_out <- unique(game_fouls_pbp$fouler[game_fouls_pbp$num_fouls==6])
#   game_fouls_pbp$fouled_out <- ifelse(length(fouled_out)>0,!is.na(mapply(grep,game_fouls_pbp$fouler,fouled_out)>0),FALSE)
#   game_fouls_pbp$time_to_foulout <- game_fouls_pbp$total_clock
#   for (player in fouled_out) {
#     player_rows <- which(game_fouls_pbp$fouler==player)
#     foulout_time <- game_fouls_pbp$total_clock[game_fouls_pbp$fouler==player & game_fouls_pbp$num_fouls==6]
#     game_fouls_pbp$time_to_foulout[player_rows] <- game_fouls_pbp$total_clock[player_rows]-foulout_time
#   }
#   
#   player_foul_situations <- game_fouls_pbp[,c("fouler","num_fouls","total_clock","score_diff","abs_scorediff","time_to_foulout")]
#   
#   foul_situations <- rbind(foul_situations, player_foul_situations)
# }

# go through the game second by second, say has that player committed a foul?
#   count how many seconds with 0 fouls? how many of those did they commit a foul?
#   do this will all players and you have a probability
# split by position maybe
# number of seconds remaining in game tells you how many times to multiply
# plot likelihood of fouling out at each point in the game - sanity check
# split up by number of fouls, so plot prob of fouling out based on time remaining, conditioned on one number of fouls
# 
# could also do similar thing but model quarters independently, so each quarter has different model

# Markov model
markov_foul_model <- function() {
  fouls_seconds <- setNames(data.frame(matrix(nrow=6,ncol=3)),c("num_fouls","instances","total_secs"))
  fouls_seconds$num_fouls <- 1:6
  fouls_seconds$instances <- 0
  fouls_seconds$total_secs <- 0
  for (game in list.files()) {
    print(game)
    load(game)
    
    if (max(game1_pbp$quarter)>4) {
      game1_pbp$total_clock[game1_pbp$quarter>4] <- game1_pbp$game_clock[game1_pbp$quarter>4]
    }
    
    game_fouls_pbp <- game1_pbp[game1_pbp$EVENTMSGTYPE==6 & !(game1_pbp$EVENTMSGACTIONTYPE %in% c(11,16,17,18)),]
    
    game_fouls_pbp$fouler <- ifelse(is.na(game_fouls_pbp$HOMEDESCRIPTION),gsub(" .*","",game_fouls_pbp$VISITORDESCRIPTION),gsub(" .*","",game_fouls_pbp$HOMEDESCRIPTION))
    game_fouls_pbp$num_fouls <- ifelse(is.na(game_fouls_pbp$HOMEDESCRIPTION),gsub(".*\\(P([0-9]).*", "\\1",game_fouls_pbp$VISITORDESCRIPTION),gsub(".*\\(P([0-9]).*", "\\1",game_fouls_pbp$HOMEDESCRIPTION))
    game_fouls_pbp$num_fouls <- as.numeric(game_fouls_pbp$num_fouls)
    if (sum(is.na(game_fouls_pbp$fouler))>0) {
      game_fouls_pbp <- game_fouls_pbp[-which(is.na(game_fouls_pbp$fouler)),]
    }
    
    for (player in unique(game_fouls_pbp$fouler)) {
      player_fouls <- game_fouls_pbp[game_fouls_pbp$fouler==player,]
      if (sum(player_fouls$num_fouls==1)>1) {
        if (sum(!is.na(player_fouls$HOMEDESCRIPTION))<nrow(player_fouls) & sum(!is.na(player_fouls$HOMEDESCRIPTION))>0) {
          player_fouls <- player_fouls[!is.na(player_fouls$HOMEDESCRIPTION),]
        } else {
          next
        }
      }
      for (nfoul in unique(player_fouls$num_fouls)) {
        if (nfoul!=1 & sum(player_fouls$num_fouls==(nfoul-1))==0) {
          next
        }
        fouls_seconds$instances[fouls_seconds$num_fouls==nfoul] <- fouls_seconds$instances[fouls_seconds$num_fouls==nfoul]+1
        fouls_seconds$total_secs[fouls_seconds$num_fouls==nfoul] <- fouls_seconds$total_secs[fouls_seconds$num_fouls==nfoul]+ifelse(nfoul==1,2880-player_fouls$total_clock[player_fouls$num_fouls==nfoul],player_fouls$total_clock[player_fouls$num_fouls==(nfoul-1)]-player_fouls$total_clock[player_fouls$num_fouls==nfoul][1])
      }
      if (nfoul<6) {
        fouls_seconds$total_secs[fouls_seconds$num_fouls==(nfoul+1)] <- fouls_seconds$total_secs[fouls_seconds$num_fouls==(nfoul+1)]+player_fouls$total_clock[player_fouls$num_fouls==nfoul][1]
      }
    }
  }
  
  fouls_seconds$transition_prob <- fouls_seconds$instances/fouls_seconds$total_secs
  transition_matrix <- t(matrix(c(1-fouls_seconds$transition_prob[1],fouls_seconds$transition_prob[1],rep(0,6),1-fouls_seconds$transition_prob[2],fouls_seconds$transition_prob[2],rep(0,6),1-fouls_seconds$transition_prob[3],fouls_seconds$transition_prob[3],rep(0,6),1-fouls_seconds$transition_prob[4],fouls_seconds$transition_prob[4],rep(0,6),1-fouls_seconds$transition_prob[5],fouls_seconds$transition_prob[5],rep(0,6),1-fouls_seconds$transition_prob[6],fouls_seconds$transition_prob[6],rep(0,6),1),nrow=7,ncol=7))
  
  
  foulout_probs <- setNames(data.frame(matrix(nrow=2880,ncol=7)),c("0","1","2","3","4","5","6"))
  for (fouls in 0:6) {
    foul_vec <- rep(0,7)
    foul_vec[fouls+1] <- 1
    curr_lik <- 0
    fo_lik <- c()
    for (i in 1:2880) {
      curr_lik <- foul_vec%*%transition_matrix
      for (j in 2:i) {
        if (i==1) {
          break
        }
        curr_lik <- curr_lik%*%transition_matrix
      }
      fo_lik <- c(fo_lik, curr_lik[1,7])
    }
    foulout_probs[,fouls+1] <- fo_lik
  }
  
  return(foulout_probs)
}


# plot(1:2880, foulout_probs[,1], pch=".", lty=1, col=1, ylim=c(0,1), main="Probability of Fouling Out Given n Fouls", xlab="Seconds Remaining", ylab="Probability of Fouling Out")
# for (i in 2:7) {
#   lines(1:2880, foulout_probs[,i], pch=".", col=i)
# }
# legend(0,.95, legend=c(0:6), col=c(1:7), lty=1, cex=.7)
# 
# library(ggplot2)
# p <- ggplot(foulout_probs, aes(x=1:2880)) + 
#   geom_line(aes(y = `1`, colour = "1")) + 
#   geom_line(aes(y = `2`, colour = "2")) + 
#   geom_line(aes(y = `3`, colour = "3")) +
#   geom_line(aes(y = `4`, colour = "4")) +
#   geom_line(aes(y = `5`, colour = "5")) +
#   geom_line(aes(y = `6`, colour = "6")) +
#   labs(title="Probability of Fouling Out Given n Fouls", x="Seconds Remaining", y="Probability of Fouling Out")
# p$labels$colour <- "Fouls"
# p
