library(RODBC)
library(dbconnect)
library(DBI)
library(gWidgets)
library(RMySQL)
library(stringr)
library(dplyr)


con = dbcon(RMySQL::MySQL(), dbname = "data_base",username = "root", password = "f129968890"
                    ,host = "localhost", port = 3306)
dbListTables(con)
singer = dbGetQuery(con ,"select * from singer")
song = dbGetQuery(con ,"select * from song")

dbSendQuery(con, "insert into board(Board_ID,Bname) values ('100003', 'favorite')")
board = dbGetQuery(con ,"select * from board")
board

# 從專輯名找歌曲
select_al<- function(x){
 # "select name from song where AID = (select AID from albums where Aname = 'Suck')"
dbGetQuery(con ,str_c("select * from song where AID = (select AID from albums where Aname = '", "')", sep = x))
}
# 加入新歌輸入歌名、類型、SID、AID
insert_song <- function(name, genre, SID, AID){
  song = dbGetQuery(con ,"select * from song")
  dbSendQuery(con, str_c("insert into song(ID, name, release_time, genre, SID, Sname, AID) values ('", 
            as.character(max(song$ID)+1),"','", name,"','",Sys.time(),"','", genre,"','", SID,"','", 
            dbGetQuery(con, str_c("select Sname from Singer where SID =", SID)),"','", AID,"')"))
  song = dbGetQuery(con ,"select * from song")
}
# 新增playlist
insert_playlist<-function(pname, UID){
  playlist = dbGetQuery(con ,"select * from playlist")
  dbSendQuery(con ,str_c("insert into playlist(PID, Pname, UID) values ('",as.character(max(playlist$PID)+1),"','", 
                              pname,"','", UID,"')"))
  playlist = dbGetQuery(con ,"select * from playlist")
}
albums = dbGetQuery(con ,"select * from albums")
#查詢album的id
find_AID <- function(albumName){
  ans <- data.frame()
  for(i in 1:nrow(albums))
  {
    temp <- as.data.frame(albums[i,])
    if(albums[i,2] == albumName)
    {
      ans <- rbind(ans, temp)
    }
  }
  return(ans)
}
#查詢歌的id
find_ID <- function(songName){
  song = dbGetQuery(con ,"select * from song")
  ans <- data.frame()
  for(i in 1:nrow(song))
  {
    temp <- as.data.frame(song[i,])
    if(song[i,2] == songName)
    {
      ans <- rbind(ans, temp)
    }
  }
  return(ans)
}
#把歌加進playlist
insert_song_to_playlist <- function(PID2, ID){
  dbSendQuery(con ,str_c("insert into song_in_list(PID, ID) values ('",PID2,"','", ID,"')"))
  dbGetQuery(con ,str_c("select * from song_in_list where PID = ", PID2))
}
