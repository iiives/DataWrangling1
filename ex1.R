#prepare work space

ls()
rm(list())

getwd()
setwd("~/Foundations/3Ex1")
dir()

#load file
df <-read.csv("refine_original.csv", stringsAsFactors = FALSE)

#cleaning data - philips, akzo, van houten and unilever (all lowercase)

c <- function(element){
  
  if(element != "philips" | element != "akzo" | element != "van houten" | element != "unilever"){
      if (element %>% contains("lip")) {
    element <- "philips"
        } else if(element %>% starts_with("ak")) {
    element <- "akzo"
        } else if(element %>% starts_with("v")) {
    element <- "van houten"
        } else if(element %>% starts_with("un")) {
    element <- "unilever"
        }
}
}

names<-
  df %>%
  select(company)
  
sapply (names,c)

result<- cbind(names, df[2:6])