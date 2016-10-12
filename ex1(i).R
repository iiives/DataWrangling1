##prepare work space

#ls()
#rm(list())

#getwd()
#setwd("~/Foundations/3Ex1")
#dir()

##load file
df <-read.csv("refine_original.csv", stringsAsFactors = FALSE)

##cleaning data - philips, akzo, van houten and unilever (all lowercase)

company <- 
  as.vector(df['company']) %>% 
  sapply(tolower)


match_philips <- grepl("lip",company, ignore.case = TRUE) + grepl("ph",company, ignore.case = TRUE)
company[match_philips>0] <- "philips"

match_akzo <- grepl("ak",company, ignore.case = TRUE)
company[match_akzo] <- "akzo"

match_vh <- grepl("va",company, ignore.case = TRUE)
company[match_vh] <- "van houten"

match_unilever <- grepl("un",company, ignore.case = TRUE)
company[match_unilever] <- "unilever"

result2<- cbind(company, df[2:6])


##split product code and product_number
result3 <- separate_(result2, "Product.code...number", column, sep = "-",remove = FALSE)


##Add product categories - p = Smartphone ; v = TV ; x = Laptop ; q = Tablet
p_cat <- function(letter){
      
  if(letter == "p"){
    "Smartphone"
  }else if(letter == "v"){
    "TV"
  }else if(letter == "x"){
    "Laptop"
  }else if(letter == "q"){
    "Tablet"
  }else {
    NULL
  }
}

result4<- mutate(result3, product_categories = lapply(result2$product_code,p_cat))

##Add full address for geocoding - new column full_address 
## concatenates the three address fields (address, city, country), separated by commas

result5<- unite(result4, "full_address", address, city, country, sep=",", remove = FALSE)


##Create dummy variables for company and product category
dummy <- function(word, variable){
    if (word == variable) {
      TRUE
    } else {
      FALSE
    }
}

result6<- mutate(result5, company_philips = lapply(result5$company,dummy,"philips"))
result6<- mutate(result6, company_akzo = lapply(result5$company,dummy,"akzo"))
result6<- mutate(result6, company_van_houten = lapply(result5$company,dummy,"van houten"))
result6<- mutate(result6, company_unilever = lapply(result5$company,dummy,"unilever"))

result6<- mutate(result6, product_smartphone = lapply(result5$product_code,dummy,"p"))
result6<- mutate(result6, product_tv = lapply(result5$product_code,dummy,"v"))
result6<- mutate(result6, product_laptop  = lapply(result5$product_code,dummy,"x"))
result6<- mutate(result6, product_tablet = lapply(result5$product_code,dummy,"q"))
result6
    

##Write to csv file
result7 <- as.matrix(result6)
write.csv(result7, "refine_clean.csv",row.names=FALSE, qmethod = "double")
    # still investigating how to override and enable escape of comma values within a cell to prevent the data being split
