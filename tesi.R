setwd("C:/Users/giuli/Desktop/coronavirus/")

######LOAD NEEDED LIBs
# install.packages('tuber')
# install.packages('lubridate')
# install.packages('data.table')
require(tuber)    
require(lubridate)
require(data.table)

##########DEFINE FUNCTIONS
clean_videos <- function(df){
  setDT(df)
  df = df[,.(video_id = as.character(video_id),
             publishedAt = as_datetime(publishedAt),
             channelId = as.character(channelId),
             title = as.character(title),
             description = as.character(description),
             channelTitle = as.character(channelId))]
  return(df)
}

clean_related_videos <- function(df){
  setDT(df)
  df = df[,.(video_id = as.character(rel_video_id),
             publishedAt = as_datetime(publishedAt),
             channelId = as.character(channelId),
             title = as.character(title),
             description = as.character(description),
             channelTitle = as.character(channelId))]
  return(df)
}


#########START TUBER
app_id="1065236442037-4mj6q4ojdfjs39drpllq5n5ceq64qvfn.apps.googleusercontent.com"
app_secret="PHmrSne-cm-ByvEVWUOwQbJY"

#alternative account
app_id="855928269102-pbucl0lfjd9cu6ea97c7ni4o1qnju1fb.apps.googleusercontent.com"
app_secret="PItbnnpdX5_jOaw_Rh_zTT79"


yt_oauth(app_id, app_secret)


########SEARCH VIDEOS until 23/3
df1 = yt_search(term = "coronavirus")
df2 = yt_search(term = "coronavirus weapon")
df3 = yt_search(term = "coronavirus epidemic")
df4 = yt_search(term = "coronavirus outbreak")
df5 = yt_search(term = "coronavirus pandemic")
df6 = yt_search(term = "coronavirus conspiracy")
df7 = yt_search(term = "coronavirus news")
df8 = yt_search(term = "nCov-2019")
df9 = yt_search(term = "#coronavirus")
df10 = yt_search(term = "covid-19") 

########MERGE THE DFs TOGETHER
all_df = rbind(clean_videos(df1), clean_videos(df2), clean_videos(df3), clean_videos(df4),  clean_videos(df5), clean_videos(df6), clean_videos(df7), clean_videos(df8), clean_videos(df9), clean_videos(df10))
all_df = unique(all_df)

########GET ALL RELATED VIDEOS
#related=NULL
for(i in 2316:nrow(all_df)){
  id = all_df$video_id[i]
  message(i, " Getting related videos of ", id)
  aux = try(get_related_videos(video_id = id))
  related= rbind(related, clean_related_videos(aux))
}
########got it until 604,738, 935, 1132, 1325, 1498, 1692, 1888, 2084, 2280

#df2= related
#df1= all_df
#fvideos=df1=original_n_related

setDT(related)
nrow(related)
related = related[!video_id %in% all_df$video_id, ]
nrow(related)
related = unique(related)
nrow(related)
related = related[grepl(pattern = "coronavirus|nCov", x = paste(title, " ", description), ignore.case = TRUE), ]
nrow(related)
related$got_related_videos = FALSE
original_n_related = rbind(all_df, related, fill=T)


###############################part 2
fvideos = original_n_related
setDT(fvideos)

videos_stats = NULL
for (i in c(18711:nrow(fvideos))) {
  time = as.character(Sys.time())
  vid = fvideos$video_id[i]
  message(i, " Getting data of video ", vid, ", at ", time, sep = "")
  
  # download video stats by id one by one :(
  aux = try(get_stats(video_id =  vid))
  setDT(aux)
  
  # establish the column type, drop lists
  if ("likeCount" %in% names(aux)){
    aux$likeCount = as.numeric(aux$likeCount)
  }else{
    aux$likeCount = NA
  }
  
  if ("dislikeCount" %in% names(aux)){
    aux$dislikeCount = as.numeric(aux$dislikeCount)
  }else{
    aux$dislikeCount = NA
  }
  
  if ("commentCount" %in% names(aux)){
    aux$commentCount = as.numeric(aux$commentCount)
  }else{
    aux$commentCount = NA
  }
  
  aux$video_id = as.character(fvideos$video_id[i])
  aux$title = as.character(fvideos$title[i])
  aux$description = as.character(fvideos$description[i])
  aux$publishedAt = fvideos$publishedAt[i]
  aux$channelId = fvideos$channelId[i]
  aux$viewCount = as.numeric(aux$viewCount)
  aux$favoriteCount = as.numeric(aux$favoriteCount)
  
  #remove new line markers
  aux$title <- gsub(pattern = "\"", replacement = "", x = aux$title)
  aux$title <- gsub(pattern = "[\r\n]", replacement = "", x = aux$title)
  aux$description <- gsub(pattern = "\"", replacement = "", x = aux$description)
  aux$description <- gsub(pattern = "[\r\n]", replacement = "", x = aux$description)
  
  aux = aux[,.(video_id, title, description, publishedAt, channelId,
               view_count = viewCount, like_count = likeCount, dislike_count = dislikeCount,
               favorite_count = favoriteCount, comment_count = commentCount)]
  videos_stats = rbind(videos_stats, aux)
}
#arrivati a 5415, 12090, 18711, 19120

videos = videos_stats
setDT(videos)
nrow(videos)

# remove duplicated column names
videos = videos[video_id != "video_id",]
nrow(videos)

# remove duplicate videos
setkey(x = videos, NULL)
videos = unique(videos)
videos = videos[!duplicated(video_id), ]
nrow(videos)

# ensure right type
videos$video_id = as.character(videos$video_id)
videos$title = as.character(videos$title)
videos$description = as.character(videos$description)
videos$publishedAt = as_datetime(videos$publishedAt)
videos$channelId = as_datetime(videos$channelId)
videos$view_count = as.numeric(videos$view_count)
videos$like_count = as.numeric(videos$like_count) 
videos$dislike_count = as.numeric(videos$dislike_count)
videos$favorite_count = as.numeric(videos$favorite_count) 
videos$comment_count = as.numeric(videos$comment_count)

save(videos, file = "temp_videos_stats.RData")

###############################parte 5
setDT(videos)

message("All Videos ", nrow(videos))
videos = videos[comment_count>0, ]
message("Videos with non 0 comments ", nrow(videos))

videos = videos[order(comment_count),]
all = NULL

for(v in 16179:nrow(videos)){
  my_video_id = videos$video_id[v]
  my_comment_count = videos$comment_count[v]
  comments = try(get_all_comments(video_id = my_video_id))
  if (!is(comments, "try-error")){
    message(v, " ", my_video_id, " GOT ", nrow(comments), " comments out of ", my_comment_count)
    setDT(comments)
    comments = comments[,.(user_name = authorDisplayName, user_channel_id = authorChannelId.value,
                           comment_text_display = textDisplay, comment_text_original = textOriginal,
                           can_rate = canRate, viewer_rating = viewerRating, like_count = likeCount,
                           published_at = publishedAt, updatedt_at = updatedAt,
                           video_id = my_video_id, comment_id = id, parent_id = parentId, moderationStatus)]
    all = rbind(all, comments)
  }
  else {
    break
  }
}
#######arrivato a 3971, 6987, 8831, 10115, 12239, 12754, 13197, 13574, 13890,14196, 14334, 14580, 14794, 14929, 15019, 15150, 15205, 15287, 15424, 15496, 15561, 15651, 15739, 15750, 15824, 15895, 15961, 16017, 16059, 16097, 16129, 16151, 16169, 16178

save(all, file = "temp_comments.RData")
write.csv(all, file='commenti.csv')
write.csv(videos, file='tesi.csv')
comments = all
rm(all)

setDT(comments)
message("nrow comments ", nrow(comments))
comments = unique(comments)
message("nrow unique comments ", nrow(comments))

nrow(fvideos)
fvideos = fvideos[as_datetime(publishedAt)>="2019-23-03",]
nrow(fvideos)

comments = comments[video_id %in% fvideos$video_id, ]
message("nrow unique comments with corrrect video id ", nrow(comments))

# dropping 2 columns as there are no variations on data
comments = comments[,.(user_name, user_channel_id, comment_text_display, comment_text_original,
                       like_count, published_at, updatedt_at, moderationStatus, 
                       video_id, comment_id, parent_id)]

remove_newLine <- function(x){
  x <-gsub("\"", "", x)	
  x <- gsub(pattern = "[\r\n]", replacement = "", x)
  return(x)
}
comments$user_name = remove_newLine(comments$user_name)
comments$comment_text_display = remove_newLine(comments$comment_text_display)
comments$comment_text_original = remove_newLine(comments$comment_text_original)
save(comments, file = "temp_comments.RData")
write.csv(comments, file='commenti.csv')
# check nr of edited comments
table(as_datetime(comments$published_at)==as_datetime(comments$updatedt_at))
