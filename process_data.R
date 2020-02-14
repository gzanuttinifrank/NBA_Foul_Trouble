# add print statements to everything - name of file and step
# save every time I filter in unique folder

source("~/Documents/School/Yale/Senior/Senior Essay/data_parsing_helpers.R")
# source("/home/accts/gz76/SeniorProject/data_parsing_helpers.R")

# library(stringr)

# setwd("/Volumes/Seagate Backup Plus Drive/NBA Player Tracking Data/Unzipped JSON files/January")

# for (json_file in list.files()[11:50]) {
#   gameId <- gsub("\\..*","",json_file)
#   print(gameId)
#   game1_movement <- sportvu_convert_json(json_file)
#   save(game1_movement, file=paste("../game1_movement/",gameId,".rda",sep="")) # save in a subdirectory
# }

setwd("/Volumes/Seagate Backup Plus Drive/NBA Player Tracking Data/Unzipped JSON files/game1_movement")
# setwd("/home/accts/gz76/SeniorProject/game1_movement")

for (rda_file in list.files()[1:147]) {
  # "0021500514" didn't work for some reason (25th in january)
  gameId <- gsub("\\..*","",rda_file)
  print(gameId)
  load(file = rda_file)
  game1_movement_new <- game1_movement[order(game1_movement$quarter,-game1_movement$game_clock,game1_movement$player_id,game1_movement$event.id),]
  temp <- diff(as.numeric(game1_movement_new$player_id))+diff(game1_movement_new$event.id)+diff(game1_movement_new$quarter)+diff(game1_movement_new$game_clock)
  keep <- sort(unique(c(1,which(temp!=0),which(temp!=0)+1,length(temp)+1)))
  filtered_movement <- game1_movement_new[keep,]
  
  game1_pbp <- get_pbp(gameId)
  game1_pbp <- game1_pbp[-1,]
  colnames(game1_pbp)[c(2,5)] <- c('event.id','quarter')
  game1_pbp[,(1:5)] <- lapply(game1_pbp[,(1:5)], as.numeric)
  game1_pbp <- game1_pbp %>% select(EVENTMSGTYPE,EVENTMSGACTIONTYPE,SCORE,quarter,PCTIMESTRING,HOMEDESCRIPTION,NEUTRALDESCRIPTION,VISITORDESCRIPTION) # limit the fields to join to keep the overall size manageable
  # game1_pbp <- game1_pbp[,c("EVENTMSGTYPE","EVENTMSGACTIONTYPE","SCORE","quarter","PCTIMESTRING","HOMEDESCRIPTION","NEUTRALDESCRIPTION","VISITORDESCRIPTION")]
  game1_pbp$game_clock <- as.numeric(gsub(":.*","",game1_pbp$PCTIMESTRING))*60 + as.numeric(gsub(".*:","",game1_pbp$PCTIMESTRING))
  game1_pbp$total_clock <- game1_pbp$game_clock+720*(4-game1_pbp$quarter)
  save(game1_pbp, file=paste("../game1_pbp/",gameId,".rda",sep=""))
  
  merge_pbp <- game1_pbp[game1_pbp$EVENTMSGTYPE %in% c(1,2,3,5,6,7,8,9,11,13,20),]
  merge_pbp$NEUTRALDESCRIPTION[merge_pbp$EVENTMSGTYPE==13] <- "END OF PERIOD"
  keep <- list() # rows to keep (only unique rows and first of duplicates)
  list_counter <- 1
  merge_pbp$HomeSubs <- NA
  merge_pbp$AwaySubs <- NA
  merge_pbp$additional_fouls <- NA
  i <- 1
  while (i<nrow(merge_pbp)) {
    keep[[list_counter]] <- i
    list_counter <- list_counter+1
    curr_time <- merge_pbp$game_clock[i]
    Asubs <- ""
    Hsubs <- ""
    if (merge_pbp$EVENTMSGTYPE[i]==8) {
      if (!is.na(merge_pbp$HOMEDESCRIPTION[i])) {
        Hsubs <- merge_pbp$HOMEDESCRIPTION[i]
      } else {
        Asubs <- merge_pbp$VISITORDESCRIPTION[i]
      }
    }
    updated_score <- NA
    additional_fouls <- ""
    j <- i+1
    while (merge_pbp$game_clock[j]==curr_time) {
      if (!is.na(merge_pbp$SCORE[j])) {
        updated_score <- merge_pbp$SCORE[j]
      }
      if (merge_pbp$EVENTMSGTYPE[j]==8) {
        if (!is.na(merge_pbp$HOMEDESCRIPTION[j])) {
          Hsubs <- ifelse(nchar(Hsubs)==0,paste(Hsubs,merge_pbp$HOMEDESCRIPTION[j],sep=""),paste(Hsubs,merge_pbp$HOMEDESCRIPTION[j],sep=", "))
        } else {
          Asubs <- ifelse(nchar(Asubs)==0,paste(Asubs,merge_pbp$VISITORDESCRIPTION[j],sep=""),paste(Asubs,merge_pbp$VISITORDESCRIPTION[j],sep=", "))
        }
      } else if (merge_pbp$EVENTMSGTYPE[j]==13) {
        merge_pbp$NEUTRALDESCRIPTION[i] <- merge_pbp$NEUTRALDESCRIPTION[j]
      } else if (merge_pbp$EVENTMSGTYPE[j]==6) {
        additional_fouls <- ifelse(nchar(additional_fouls)==0,paste(additional_fouls,ifelse(!is.na(merge_pbp$HOMEDESCRIPTION[j]),merge_pbp$HOMEDESCRIPTION[j],merge_pbp$VISITORDESCRIPTION[j]),sep=""),paste(additional_fouls,ifelse(!is.na(merge_pbp$HOMEDESCRIPTION[j]),merge_pbp$HOMEDESCRIPTION[j],merge_pbp$VISITORDESCRIPTION[j]),sep=", "))
      }
      j <- j+1
      if (j>nrow(merge_pbp)) {
        break
      }
    }
    if (!is.na(updated_score)) {
      merge_pbp$SCORE[i] <- updated_score
    }
    merge_pbp$HomeSubs[i] <- ifelse(nchar(Hsubs)>0,Hsubs,NA)
    merge_pbp$AwaySubs[i] <- ifelse(nchar(Asubs)>0,Asubs,NA)
    merge_pbp$additional_fouls[i] <- ifelse(nchar(additional_fouls)>0,additional_fouls,NA)
    i <- j
  }
  # keep[[list_counter]] <- nrow(merge_pbp)
  if (merge_pbp$total_clock[nrow(merge_pbp)]!=merge_pbp$total_clock[nrow(merge_pbp)-1]) {
    keep[[list_counter]] <- nrow(merge_pbp)
  }
  merge_pbp_unique <- merge_pbp[unlist(keep),]
  
  # calculate score change
  merge_pbp_unique$score_change <- FALSE
  lastscore <- "0 - 0"
  for (i in 1:nrow(merge_pbp_unique)) {
    if (!is.na(merge_pbp_unique$SCORE[i])) {
      lastscore <- merge_pbp_unique$SCORE[i]
      merge_pbp_unique$score_change[i] <- TRUE
    } else {
      merge_pbp_unique$SCORE[i] <- lastscore
    }
  }
  
  filtered_movement <- filtered_movement[!duplicated(filtered_movement[c("game_clock","quarter","player_id")]),]
  
  filtered_movement$shot_clock[is.na(filtered_movement$shot_clock)] <- 0
  shotclockdiff <- c(diff(filtered_movement$shot_clock),24) # big values on last instance of previous play
  filtered_movement$shotclockdiff <- shotclockdiff
  
  filtered_movement$merge_clock <- NA
  # start with game1_pbp, for each play find the closest shot clock change to that time (in same quarter)
  sc_changes <- filtered_movement$game_clock[abs(filtered_movement$shotclockdiff)>1] # times when shot clock changes, in theory denoting the ends of plays
  sc_changes_indices <- which(abs(filtered_movement$shotclockdiff)>1)
  sc_changes_qtrs <- filtered_movement$quarter[sc_changes_indices]
  lastend_sc <- 1
  for (i in 1:nrow(merge_pbp_unique)) {
    playtime <- merge_pbp_unique$game_clock[i]
    playqtr <- merge_pbp_unique$quarter[i]
    timerange <- c(playtime+3,playtime-3)
    matching <- c()
    matching_indices <- c()
    for (j in lastend_sc:length(sc_changes)) { # can probably be sped up using indices
      if (sc_changes[j]<timerange[2] & sc_changes_qtrs[j]>=playqtr) {
        break
      }
      if (sc_changes[j]<=timerange[1] & sc_changes[j]>=timerange[2] & sc_changes_qtrs[j]==playqtr) {
        matching <- c(matching, sc_changes[j])
        matching_indices <- c(matching_indices, j)
      }
    }
    if (length(matching)==0) {
      next
    }
    # make sure it gets the right quarter
    diffs <- abs(matching-playtime)
    if (sum(diffs<1)>1) {
      closest_match_index <- which(diffs<1)[1]
    } else {
      closest_match_index <- which(diffs==min(diffs))[length(which(diffs==min(diffs)))]  # if there are multiple, take the last
    }
    closest_match <- matching[closest_match_index]
    lastend_sc <- matching_indices[closest_match_index]
    curr_play_end <- sc_changes_indices[lastend_sc]
    while (curr_play_end!=nrow(filtered_movement) & filtered_movement$game_clock[curr_play_end+1]==filtered_movement$game_clock[curr_play_end]) {
      curr_play_end <- curr_play_end+1
    }
    filtered_movement$merge_clock[curr_play_end] <- playtime
  }
  
  # add variable for when ball crosses halfcourt: x_loc=47
  filtered_movement$cross_halfcourt <- FALSE
  prev_loc <- 47
  for (i in which(filtered_movement$player_id==-1)) {
    new_loc <- filtered_movement$x_loc[i]
    if ((new_loc>47 & prev_loc<47) | (new_loc<47 & prev_loc>47)) {
      filtered_movement$cross_halfcourt[i] <- TRUE
    }
    prev_loc <- new_loc
  }
  
  # propagate until end of play and add signal for last location before end of play
  filtered_movement$endofplay <- FALSE
  crosses <- which(filtered_movement$cross_halfcourt)
  endsofplays <- which(!is.na(filtered_movement$merge_clock))
  for (eop in endsofplays) {
    # find last crossing of halfcourt
    diffs <- eop-crosses
    if (sum(diffs>0)==0) { # if there is absolutely no crossing before, means it was first play of game so set them all to true
      filtered_movement$cross_halfcourt[1:eop] <- TRUE
      filtered_movement$endofplay[filtered_movement$game_clock==filtered_movement$game_clock[eop] & filtered_movement$quarter==filtered_movement$quarter[eop]] <- TRUE
      next
    }
    last_cross_index <- crosses[which(diffs==min(diffs[diffs>0]))]
    filtered_movement$cross_halfcourt[last_cross_index:eop] <- TRUE # set cross_halfcourt to be TRUE for every instance from original cross to end of play
    
    filtered_movement$endofplay[filtered_movement$game_clock==filtered_movement$game_clock[eop] & filtered_movement$quarter==filtered_movement$quarter[eop]] <- TRUE
  }
  
  # only observations that contribute to plays that match play by play with shot clock change, and where ball is in frontcourt
  frontcourt_movement <- filtered_movement[filtered_movement$cross_halfcourt,]
  save(frontcourt_movement, file=paste("../frontcourt_movement/",gameId,".rda",sep=""))
  
  merge_rows <- which(!is.na(frontcourt_movement$merge_clock))
  startrow <- 1
  for (row in merge_rows) {
    frontcourt_movement$merge_clock[startrow:(row-1)] <- frontcourt_movement$merge_clock[row]
    startrow <- row+1
  }
  
  movement_pbp <- merge(frontcourt_movement, merge_pbp_unique[,-5], by.x=c("merge_clock","quarter"), by.y=c("game_clock","quarter"))
  movement_pbp <- movement_pbp[order(movement_pbp$quarter,-movement_pbp$game_clock,movement_pbp$player_id),]
  
  movement_pbp$total_clock <- movement_pbp$game_clock+720*(4-movement_pbp$quarter)
  movement_pbp$mc_total <- movement_pbp$merge_clock+720*(4-movement_pbp$quarter)
  
  eliminate <- c()
  timestamps <- c()
  # situations_gc <- unique(movement_pbp[c("game_clock","quarter")])
  situations_gc <- unique(movement_pbp$total_clock)
  for (i in 1:length(situations_gc)) {
    # numrows <- which(movement_pbp$game_clock==situations_gc$game_clock[i] & movement_pbp$quarter==situations_gc$quarter[i])
    numrows <- which(movement_pbp$total_clock==situations_gc[i])
    if (length(numrows)!=11) {
      eliminate <- c(eliminate, numrows)
      timestamps <- c(timestamps, i)
      if (sum(movement_pbp$endofplay[numrows])>0) {
        # if end of play timestamp will be eliminated, must move back end of play to new last timestamp: 88 new instances of endofplay which is divisible by 11 which is promising
        # new_end <- movement_pbp$game_clock[numrows[1]-1]
        new_end <- movement_pbp$total_clock[numrows[1]-1]
        # numrows_prev <- which(movement_pbp$game_clock==new_end & movement_pbp$quarter==situations_gc$quarter[i])
        numrows_prev <- which(movement_pbp$total_clock==new_end)
        while (length(numrows_prev)!=11) {
          # new_end <- movement_pbp$game_clock[numrows_prev[1]-1]
          # numrows_prev <- which(movement_pbp$game_clock==new_end & movement_pbp$quarter==situations_gc$quarter[i])
          new_end <- movement_pbp$total_clock[numrows_prev[1]-1]
          numrows_prev <- which(movement_pbp$total_clock==new_end)
        }
        # movement_pbp$endofplay[movement_pbp$game_clock==new_end & movement_pbp$quarter==situations_gc$quarter[i]] <- TRUE
        movement_pbp$endofplay[movement_pbp$total_clock==new_end] <- TRUE
      }
    }
  }
  movement_pbp <- movement_pbp[-eliminate,]
  
  # for each play figure out which team is on defense (switch sides at halftime) based on ball location after crossing halfcourt
  homeside <- ifelse(movement_pbp$x_loc[movement_pbp$EVENTMSGTYPE==1 & !is.na(movement_pbp$HOMEDESCRIPTION) & movement_pbp$player_id==-1 & movement_pbp$endofplay & movement_pbp$quarter==1][1]<47,-1,1)
  movement_pbp$on_offense <- ifelse(movement_pbp$player_id==-1,NA,FALSE)
  # situations_mc <- unique(movement_pbp[c("merge_clock","quarter")])
  situations_mc <- unique(movement_pbp$mc_total)
  teams <- unique(movement_pbp$team_id[!is.na(movement_pbp$team_id)])
  elim_stamps <- c()
  eop_stamps <- c()
  # for (i in 1:nrow(situations_mc)) {
  for (sit_mc in situations_mc) {
    # mc_df <- movement_pbp[movement_pbp$quarter==situations_mc$quarter[i] & movement_pbp$merge_clock==situations_mc$merge_clock[i],]
    mc_df <- movement_pbp[movement_pbp$mc_total==sit_mc,]
    ball_loc <- mc_df$x_loc[mc_df$player_id==-1 & mc_df$endofplay]
    # if (situations_mc$quarter[i]<=2) {
    if (sit_mc>=1440) {
      if (sign(ball_loc-47)==sign(homeside)) {
        # movement_pbp$on_offense[movement_pbp$quarter==situations_mc$quarter[i] & movement_pbp$merge_clock==situations_mc$merge_clock[i] & movement_pbp$hometeam] <- TRUE
        movement_pbp$on_offense[movement_pbp$mc_total==sit_mc & movement_pbp$hometeam] <- TRUE
      } else {
        # movement_pbp$on_offense[movement_pbp$quarter==situations_mc$quarter[i] & movement_pbp$merge_clock==situations_mc$merge_clock[i] & !movement_pbp$hometeam] <- TRUE
        movement_pbp$on_offense[movement_pbp$mc_total==sit_mc & !movement_pbp$hometeam] <- TRUE
      }
    } else {
      if (sign(ball_loc-47)==sign(homeside)) {
        # movement_pbp$on_offense[movement_pbp$quarter==situations_mc$quarter[i] & movement_pbp$merge_clock==situations_mc$merge_clock[i] & !movement_pbp$hometeam] <- TRUE
        movement_pbp$on_offense[movement_pbp$mc_total==sit_mc & !movement_pbp$hometeam] <- TRUE
      } else {
        # movement_pbp$on_offense[movement_pbp$quarter==situations_mc$quarter[i] & movement_pbp$merge_clock==situations_mc$merge_clock[i] & movement_pbp$hometeam] <- TRUE
        movement_pbp$on_offense[movement_pbp$mc_total==sit_mc & movement_pbp$hometeam] <- TRUE
      }
    }
    
    # if there's a substitution, get rid of time stamps when new guy is being tracked (this means nothing is happening and there's an uneven number of players being tracked in the play across the teams)
    players1 <- mc_df %>% filter(team_id==teams[1]) %>% select(player_id) %>% distinct(player_id)
    players2 <- mc_df %>% filter(team_id==teams[2]) %>% select(player_id) %>% distinct(player_id)
    if (nrow(players1)!=nrow(players2) | nrow(players1)+nrow(players2)!=10) {
      hd_subs <- length(grep("SUB",mc_df$HOMEDESCRIPTION[1]))
      vd_subs <- length(grep("SUB",mc_df$VISITORDESCRIPTION[1]))
      # Hsubs <- str_count(mc_df$HomeSubs[1],"SUB")
      # Hsubs <- ifelse(!is.na(Hsubs),Hsubs,0)
      Hsubs <- lengths(regmatches(mc_df$HomeSubs[1], gregexpr("SUB", mc_df$HomeSubs[1])))
      # Asubs <- str_count(mc_df$AwaySubs[1],"SUB")
      # Asubs <- ifelse(!is.na(Asubs),Asubs,0)
      Asubs <- lengths(regmatches(mc_df$AwaySubs[1], gregexpr("SUB", mc_df$AwaySubs[1])))
      if (Hsubs+Asubs+hd_subs+vd_subs==0) {
        half <- FALSE
        for (pl in c(players1$player_id,players2$player_id)) {
          instances <- sum(mc_df$player_id==pl)
          if (instances<(nrow(mc_df)/22)) {
            bad_stamps <- mc_df$total_clock[mc_df$player_id==pl]
            elim_stamps <- c(elim_stamps, bad_stamps[!(bad_stamps %in% elim_stamps)])
          } else if (instances==(nrow(mc_df)/22)) {
            if (half) {
              bad_stamps <- mc_df$total_clock[mc_df$player_id==pl]
              elim_stamps <- c(elim_stamps, bad_stamps[!(bad_stamps %in% elim_stamps)])
              half <- FALSE
            } else {
              half <- TRUE
            }
          }
        }
      } else {
        players_in <- c()
        if (hd_subs>0) {
          player_in <- gsub("SUB: ","",gsub(" FOR.*","",mc_df$HOMEDESCRIPTION[1]))
          if (grepl("\\. ",player_in)) {
            pl_in <- unique(mc_df$player_id[mc_df$lastname==gsub(".*\\. ","",player_in) & mc_df$hometeam & grepl(gsub("\\..*","",player_in),mc_df$firstname)])
            if (length(pl_in)>1) {
              minrows <- Inf
              minpl <- NA
              for (pl in pl_in) {
                if (sum(mc_df$player_id==pl)<minrows) {
                  minrows <- sum(mc_df$player_id==pl)
                  minpl <- pl
                }
              }
              pl_in <- minpl
            }
          } else if (grepl("\\.",player_in)) {
            pl_in <- unique(mc_df$player_id[mc_df$lastname==player_in & mc_df$hometeam])
            if (length(pl_in)>1) {
              minrows <- Inf
              minpl <- NA
              for (pl in pl_in) {
                if (sum(mc_df$player_id==pl)<minrows) {
                  minrows <- sum(mc_df$player_id==pl)
                  minpl <- pl
                }
              }
              pl_in <- minpl
            } else if (length(pl_in)==0) {
              pl_in <- unique(mc_df$player_id[mc_df$lastname==gsub(" .*","",player_in) & mc_df$hometeam])
              if (length(pl_in)>1) {
                minrows <- Inf
                minpl <- NA
                for (pl in pl_in) {
                  if (sum(mc_df$player_id==pl)<minrows) {
                    minrows <- sum(mc_df$player_id==pl)
                    minpl <- pl
                  }
                }
                pl_in <- minpl
              }
            }
          } else {
            pl_in <- unique(mc_df$player_id[mc_df$lastname==player_in & mc_df$hometeam])
            if (length(pl_in)>1) {
              minrows <- Inf
              minpl <- NA
              for (pl in pl_in) {
                if (sum(mc_df$player_id==pl)<minrows) {
                  minrows <- sum(mc_df$player_id==pl)
                  minpl <- pl
                }
              }
              pl_in <- minpl
            }
          }
          players_in <- c(players_in, pl_in)
        }
        if (vd_subs>0) {
          player_in <- gsub("SUB: ","",gsub(" FOR.*","",mc_df$VISITORDESCRIPTION[1]))
          if (grepl("\\. ",player_in)) {
            pl_in <- unique(mc_df$player_id[mc_df$lastname==gsub(".*\\. ","",player_in) & !mc_df$hometeam & grepl(gsub("\\..*","",player_in),mc_df$firstname)])
            if (length(pl_in)>1) {
              minrows <- Inf
              minpl <- NA
              for (pl in pl_in) {
                if (sum(mc_df$player_id==pl)<minrows) {
                  minrows <- sum(mc_df$player_id==pl)
                  minpl <- pl
                }
              }
              pl_in <- minpl
            }
          } else if (grepl("\\.",player_in)) {
            pl_in <- unique(mc_df$player_id[mc_df$lastname==player_in & !mc_df$hometeam])
            if (length(pl_in)>1) {
              minrows <- Inf
              minpl <- NA
              for (pl in pl_in) {
                if (sum(mc_df$player_id==pl)<minrows) {
                  minrows <- sum(mc_df$player_id==pl)
                  minpl <- pl
                }
              }
              pl_in <- minpl
            } else if (length(pl_in)==0) {
              pl_in <- unique(mc_df$player_id[mc_df$lastname==gsub(" .*","",player_in) & !mc_df$hometeam])
              if (length(pl_in)>1) {
                minrows <- Inf
                minpl <- NA
                for (pl in pl_in) {
                  if (sum(mc_df$player_id==pl)<minrows) {
                    minrows <- sum(mc_df$player_id==pl)
                    minpl <- pl
                  }
                }
                pl_in <- minpl
              }
            }
          } else {
            pl_in <- unique(mc_df$player_id[mc_df$lastname==player_in & !mc_df$hometeam])
            if (length(pl_in)>1) {
              minrows <- Inf
              minpl <- NA
              for (pl in pl_in) {
                if (sum(mc_df$player_id==pl)<minrows) {
                  minrows <- sum(mc_df$player_id==pl)
                  minpl <- pl
                }
              }
              pl_in <- minpl
            }
          }
          players_in <- c(players_in, pl_in)
        }
        if (Hsubs>0) {
          if (Hsubs==1) {
            player_in <- gsub("SUB: ","",gsub(" FOR.*","",mc_df$HomeSubs[1]))
            if (grepl("\\. ",player_in)) {
              pl_in <- unique(mc_df$player_id[mc_df$lastname==gsub(".*\\. ","",player_in) & mc_df$hometeam & grepl(gsub("\\..*","",player_in),mc_df$firstname)])
              if (length(pl_in)>1) {
                minrows <- Inf
                minpl <- NA
                for (pl in pl_in) {
                  if (sum(mc_df$player_id==pl)<minrows) {
                    minrows <- sum(mc_df$player_id==pl)
                    minpl <- pl
                  }
                }
                pl_in <- minpl
              }
            } else if (grepl("\\.",player_in)) {
              pl_in <- unique(mc_df$player_id[mc_df$lastname==player_in & mc_df$hometeam])
              if (length(pl_in)>1) {
                minrows <- Inf
                minpl <- NA
                for (pl in pl_in) {
                  if (sum(mc_df$player_id==pl)<minrows) {
                    minrows <- sum(mc_df$player_id==pl)
                    minpl <- pl
                  }
                }
                pl_in <- minpl
              } else if (length(pl_in)==0) {
                pl_in <- unique(mc_df$player_id[mc_df$lastname==gsub(" .*","",player_in) & mc_df$hometeam])
                if (length(pl_in)>1) {
                  minrows <- Inf
                  minpl <- NA
                  for (pl in pl_in) {
                    if (sum(mc_df$player_id==pl)<minrows) {
                      minrows <- sum(mc_df$player_id==pl)
                      minpl <- pl
                    }
                  }
                  pl_in <- minpl
                }
              }
            } else {
              pl_in <- unique(mc_df$player_id[mc_df$lastname==player_in & mc_df$hometeam])
              if (length(pl_in)>1) {
                minrows <- Inf
                minpl <- NA
                for (pl in pl_in) {
                  if (sum(mc_df$player_id==pl)<minrows) {
                    minrows <- sum(mc_df$player_id==pl)
                    minpl <- pl
                  }
                }
                pl_in <- minpl
              }
            }
            players_in <- c(players_in, pl_in)
          } else {
            multiplesubs <- strsplit(mc_df$HomeSubs, ", ")[[1]]
            for (s in multiplesubs) {
              # go through multiplesubs and do same as above for each
              player_in <- gsub("SUB: ","",gsub(" FOR.*","",s))
              if (grepl("\\. ",player_in)) {
                pl_in <- unique(mc_df$player_id[mc_df$lastname==gsub(".*\\. ","",player_in) & mc_df$hometeam & grepl(gsub("\\..*","",player_in),mc_df$firstname)])
                if (length(pl_in)>1) {
                  minrows <- Inf
                  minpl <- NA
                  for (pl in pl_in) {
                    if (sum(mc_df$player_id==pl)<minrows) {
                      minrows <- sum(mc_df$player_id==pl)
                      minpl <- pl
                    }
                  }
                  pl_in <- minpl
                }
              } else if (grepl("\\.",player_in)) {
                pl_in <- unique(mc_df$player_id[mc_df$lastname==player_in & mc_df$hometeam])
                if (length(pl_in)>1) {
                  minrows <- Inf
                  minpl <- NA
                  for (pl in pl_in) {
                    if (sum(mc_df$player_id==pl)<minrows) {
                      minrows <- sum(mc_df$player_id==pl)
                      minpl <- pl
                    }
                  }
                  pl_in <- minpl
                } else if (length(pl_in)==0) {
                  pl_in <- unique(mc_df$player_id[mc_df$lastname==gsub(" .*","",player_in) & mc_df$hometeam])
                  if (length(pl_in)>1) {
                    minrows <- Inf
                    minpl <- NA
                    for (pl in pl_in) {
                      if (sum(mc_df$player_id==pl)<minrows) {
                        minrows <- sum(mc_df$player_id==pl)
                        minpl <- pl
                      }
                    }
                    pl_in <- minpl
                  }
                }
              } else {
                pl_in <- unique(mc_df$player_id[mc_df$lastname==player_in & mc_df$hometeam])
                if (length(pl_in)>1) {
                  minrows <- Inf
                  minpl <- NA
                  for (pl in pl_in) {
                    if (sum(mc_df$player_id==pl)<minrows) {
                      minrows <- sum(mc_df$player_id==pl)
                      minpl <- pl
                    }
                  }
                  pl_in <- minpl
                }
              }
              players_in <- c(players_in, pl_in)
            }
          }
        }
        if (Asubs>0) {
          if (Asubs==1) {
            player_in <- gsub("SUB: ","",gsub(" FOR.*","",mc_df$AwaySubs[1]))
            if (grepl("\\. ",player_in)) {
              pl_in <- unique(mc_df$player_id[mc_df$lastname==gsub(".*\\. ","",player_in) & !mc_df$hometeam & grepl(gsub("\\..*","",player_in),mc_df$firstname)])
              if (length(pl_in)>1) {
                minrows <- Inf
                minpl <- NA
                for (pl in pl_in) {
                  if (sum(mc_df$player_id==pl)<minrows) {
                    minrows <- sum(mc_df$player_id==pl)
                    minpl <- pl
                  }
                }
                pl_in <- minpl
              }
            } else if (grepl("\\.",player_in)) {
              pl_in <- unique(mc_df$player_id[mc_df$lastname==player_in & !mc_df$hometeam])
              if (length(pl_in)>1) {
                minrows <- Inf
                minpl <- NA
                for (pl in pl_in) {
                  if (sum(mc_df$player_id==pl)<minrows) {
                    minrows <- sum(mc_df$player_id==pl)
                    minpl <- pl
                  }
                }
                pl_in <- minpl
              } else if (length(pl_in)==0) {
                pl_in <- unique(mc_df$player_id[mc_df$lastname==gsub(" .*","",player_in) & !mc_df$hometeam])
                if (length(pl_in)>1) {
                  minrows <- Inf
                  minpl <- NA
                  for (pl in pl_in) {
                    if (sum(mc_df$player_id==pl)<minrows) {
                      minrows <- sum(mc_df$player_id==pl)
                      minpl <- pl
                    }
                  }
                  pl_in <- minpl
                }
              }
            } else {
              pl_in <- unique(mc_df$player_id[mc_df$lastname==player_in & !mc_df$hometeam])
              if (length(pl_in)>1) {
                minrows <- Inf
                minpl <- NA
                for (pl in pl_in) {
                  if (sum(mc_df$player_id==pl)<minrows) {
                    minrows <- sum(mc_df$player_id==pl)
                    minpl <- pl
                  }
                }
                pl_in <- minpl
              }
            }
            players_in <- c(players_in, pl_in)
          } else {
            multiplesubs <- strsplit(mc_df$AwaySubs, ", ")[[1]]
            for (s in multiplesubs) {
              # go through multiplesubs and do same as above for each
              player_in <- gsub("SUB: ","",gsub(" FOR.*","",s))
              if (grepl("\\. ",player_in)) {
                pl_in <- unique(mc_df$player_id[mc_df$lastname==gsub(".*\\. ","",player_in) & !mc_df$hometeam & grepl(gsub("\\..*","",player_in),mc_df$firstname)])
                if (length(pl_in)>1) {
                  minrows <- Inf
                  minpl <- NA
                  for (pl in pl_in) {
                    if (sum(mc_df$player_id==pl)<minrows) {
                      minrows <- sum(mc_df$player_id==pl)
                      minpl <- pl
                    }
                  }
                  pl_in <- minpl
                }
              } else if (grepl("\\.",player_in)) {
                pl_in <- unique(mc_df$player_id[mc_df$lastname==player_in & !mc_df$hometeam])
                if (length(pl_in)>1) {
                  minrows <- Inf
                  minpl <- NA
                  for (pl in pl_in) {
                    if (sum(mc_df$player_id==pl)<minrows) {
                      minrows <- sum(mc_df$player_id==pl)
                      minpl <- pl
                    }
                  }
                  pl_in <- minpl
                } else if (length(pl_in)==0) {
                  pl_in <- unique(mc_df$player_id[mc_df$lastname==gsub(" .*","",player_in) & !mc_df$hometeam])
                  if (length(pl_in)>1) {
                    minrows <- Inf
                    minpl <- NA
                    for (pl in pl_in) {
                      if (sum(mc_df$player_id==pl)<minrows) {
                        minrows <- sum(mc_df$player_id==pl)
                        minpl <- pl
                      }
                    }
                    pl_in <- minpl
                  }
                }
              } else {
                pl_in <- unique(mc_df$player_id[mc_df$lastname==player_in & !mc_df$hometeam])
                if (length(pl_in)>1) {
                  minrows <- Inf
                  minpl <- NA
                  for (pl in pl_in) {
                    if (sum(mc_df$player_id==pl)<minrows) {
                      minrows <- sum(mc_df$player_id==pl)
                      minpl <- pl
                    }
                  }
                  pl_in <- minpl
                }
              }
              players_in <- c(players_in, pl_in)
            }
          }
        }
        players_in <- unique(players_in)
        if (length(c(players1$player_id,players2$player_id))-length(unique(players_in))!=10) {
          for (pl in c(players1$player_id,players2$player_id)) {
            instances <- sum(mc_df$player_id==pl)
            if (instances<(nrow(mc_df)/22)) {
              bad_stamps <- mc_df$total_clock[mc_df$player_id==pl]
              elim_stamps <- c(elim_stamps, bad_stamps[!(bad_stamps %in% elim_stamps)])
            }
          }
        }
        for (k in players_in) {
          bad_stamps <- mc_df$total_clock[mc_df$player_id==k]
          if (max(bad_stamps)!=mc_df$total_clock[1]) {
            eop_stamps <- min(mc_df$total_clock[mc_df$total_clock>max(bad_stamps)])
          }
          elim_stamps <- c(elim_stamps, bad_stamps[!(bad_stamps %in% elim_stamps)])
        }
      }
    }
  }
  if (length(eop_stamps)>0) {
    movement_pbp$endofplay[movement_pbp$total_clock %in% eop_stamps] <- TRUE
  }
  movement_pbp <- movement_pbp[-which(movement_pbp$total_clock %in% elim_stamps),]
  
  movement_pbp$score_diff <- ifelse(movement_pbp$hometeam, as.numeric(gsub(".*- ","",movement_pbp$SCORE))-as.numeric(gsub(" -.*","",movement_pbp$SCORE)), as.numeric(gsub(" -.*","",movement_pbp$SCORE))-as.numeric(gsub(".*- ","",movement_pbp$SCORE)))
  save(movement_pbp, file=paste("../movement_pbp/",gameId,".rda",sep=""))

  dist_movement_pbp <- movement_pbp
  situations_mc <- unique(movement_pbp[c("merge_clock","quarter")])
  dists_plays <- cbind(setNames(data.frame(matrix(NA,nrow=nrow(dist_movement_pbp),ncol=6)),c("dist1","dist2","dist3","dist4","dist5","dist_ball")),dist_movement_pbp[,c("game_clock","quarter","player_id","merge_clock")])
  for (i in 1:nrow(situations_mc)) {
    clock_obs <- c(min(which(dist_movement_pbp$merge_clock==situations_mc$merge_clock[i] & dist_movement_pbp$quarter==situations_mc$quarter[i])),max(which(dist_movement_pbp$merge_clock==situations_mc$merge_clock[i] & dist_movement_pbp$quarter==situations_mc$quarter[i])))
    distMat <- player_dist_matrix(dist_movement_pbp, situations_mc$merge_clock[i], situations_mc$quarter[i])
    onfloor <- unique(dist_movement_pbp$player_id[dist_movement_pbp$merge_clock==situations_mc$merge_clock[i] & dist_movement_pbp$quarter==situations_mc$quarter[i]])
    onfloor_noball <- onfloor[-grep("-1",onfloor)]
    for (j in 1:length(onfloor_noball)) {
      player <- onfloor_noball[j]
      pl_cols <- distMat[,c(grep(paste("_",player,"$",sep=""),names(distMat)),grep(paste("^",player,"_",sep=""),names(distMat)),ncol(distMat))]
      names(pl_cols) <- c("dist1","dist2","dist3","dist4","dist5","dist_ball","game_clock") # eliminates information about who they're closest to
      pl_cols$quarter <- situations_mc$quarter[i]
      pl_cols$player_id <- player
      pl_cols$merge_clock <- situations_mc$merge_clock[i]
      player_indices <- which(dists_plays$player_id[clock_obs[1]:clock_obs[2]]==player)+(clock_obs[1]-1)
      dists_plays[player_indices,] <- pl_cols
    }
  }

  dist_movement_pbp <- merge(dist_movement_pbp, dists_plays[,-which(names(dists_plays)=="merge_clock")], by=c("game_clock","quarter","player_id"))
  dist_movement_pbp <- dist_movement_pbp[order(-dist_movement_pbp$total_clock),]

  dist_movement_pbp$closest_dist <- pmin(dist_movement_pbp$dist1,dist_movement_pbp$dist2,dist_movement_pbp$dist3,dist_movement_pbp$dist4,dist_movement_pbp$dist5)

  save(dist_movement_pbp, file=paste("../dist_movement_pbp/",gameId,".rda",sep=""))

  # get rid of ball observations, and only run it on players currently on defense
  defense_dist_movement <- dist_movement_pbp[!is.na(dist_movement_pbp$on_offense) & !dist_movement_pbp$on_offense,]

  # which player is guarding the ball - defined as player on defense closest to ball
  defense_dist_movement$on_ball <- FALSE
  startindex <- 1
  endindex <- startindex+4
  nrows <- nrow(defense_dist_movement)
  while(endindex<=nrows) {
    min_toball <- min(defense_dist_movement$dist_ball[startindex:endindex])
    player_onball <- which(defense_dist_movement$dist_ball[startindex:endindex]==min_toball)
    defense_dist_movement$on_ball[(startindex-1+player_onball)] <- TRUE
    startindex <- endindex+1
    endindex <- startindex+4
  }

  # onball_defense <- defense_dist_movement[defense_dist_movement$on_ball,]

  # score_diff is not linear:
  # onball_defense$abs_scorediff <- abs(onball_defense$score_diff)
  defense_dist_movement$abs_scorediff <- abs(defense_dist_movement$score_diff)

  defense_dist_movement$position <- factor(defense_dist_movement$position) # could also decrease the number of positions by condensing (i.e. G-F -> G, etc.)

  defense_dist_movement$num_fouls <- NA
  player_fouls <- defense_dist_movement %>% select(player_id,lastname) %>% distinct(player_id,lastname)
  player_fouls$nfouls <- 0
  curr_mergeclock <- defense_dist_movement$merge_clock[1]
  fouling_players <- c()
  for (i in 1:nrow(defense_dist_movement)) {
    if (defense_dist_movement$merge_clock[i]!=curr_mergeclock) {
      for (fouling_player in fouling_players) {
        player_fouls$nfouls[player_fouls$lastname==fouling_player] <- player_fouls$nfouls[player_fouls$lastname==fouling_player]+1
      }
      fouling_players <- c()

      if (defense_dist_movement$EVENTMSGTYPE[i]==6 | !is.na(defense_dist_movement$additional_fouls[i])) {
        if (!is.na(defense_dist_movement$HOMEDESCRIPTION[i]) & grepl("foul \\(P", defense_dist_movement$HOMEDESCRIPTION[i], ignore.case=T)) {
          fouling_player <- gsub(" .*","",defense_dist_movement$HOMEDESCRIPTION[i])
          fouling_players <- c(fouling_players, fouling_player)
        }
        if (!is.na(defense_dist_movement$VISITORDESCRIPTION[i]) & grepl("foul \\(P", defense_dist_movement$VISITORDESCRIPTION[i], ignore.case=T)) {
          fouling_player <- gsub(" .*","",defense_dist_movement$VISITORDESCRIPTION[i])
          fouling_players <- c(fouling_players, fouling_player)
        }
        if (!is.na(defense_dist_movement$additional_fouls[i]) & grepl("foul \\(P", defense_dist_movement$additional_fouls[i], ignore.case=T)) {
          add_fouls <- strsplit(defense_dist_movement$additional_fouls[i], ", ")[[1]]
          for (foul in add_fouls) {
            fouling_player <- gsub(" .*","",foul)
            fouling_players <- c(fouling_players, fouling_player)
          }
        }
      }
    }
    defense_dist_movement$num_fouls[i] <- player_fouls$nfouls[player_fouls$player_id==defense_dist_movement$player_id[i]]
    curr_mergeclock <- defense_dist_movement$merge_clock[i]
  }

  save(defense_dist_movement, file=paste("../defense_dist_movement/",gameId,".rda",sep=""))

  cut_down_def_mvmt <- defense_dist_movement[defense_dist_movement$game_clock %in% unique(defense_dist_movement$game_clock)[which(1:length(unique(defense_dist_movement$game_clock))%%25==0)],]
  save(cut_down_def_mvmt, file=paste("../cut_down_def_mvmt/",gameId,".rda",sep=""))
}
