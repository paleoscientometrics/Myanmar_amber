library(fulltext)

#custom function
sp_ch <- function(x){gsub("[^0-9A-Za-z///' ]","" , x,ignore.case = TRUE)} # function to remove special characters


# Check completed ---------------------------------------------------------
refs <- read.csv(file.path("data", "2020-09-16_pbdb_references.csv"), encoding="UTF-8")
aff_myanmar <- read.csv(file.path("data", "aff_data_Myanmar.csv"))
refs <- refs[refs$reference_no %in% aff_myanmar$ï..reference_no,]


# Create empty bib file
system("rm bib.bib") # removed existing ones, may not be needed
system("touch bib.bib") #this will be in the working directory

res <- data.frame(reference_no = NA,
                  reftitle = NA)
#find doi
for(i in 1:nrow(refs)){
  bib <- NA
  Sys.sleep(3) # just to give the api a bit of time to breathe
  tryCatch({
    #get doi from crossref is possible
    if(is.na(refs$doi[i])){ 
    res <- ft_search(query=refs$reftitle[i], from="crossref")
    
    temp <-res$crossref$data
    temp <- temp[temp$container.title == refs$pubtitle[i],]
    
    if(nrow(temp) > 0){
      n <- which.min(adist(temp$title,refs$reftitle[i]))
      
      bib <- rcrossref::cr_cn(temp$doi[n])
      res <- rbind(reference_no = refs$reference_no[i], reftitle = temp$title[n])
    }
  } else{
    #gets the reference if it exists
    bib <- rcrossref::cr_cn(refs$doi[i])
    res <- rbind(reference_no = refs$reference_no[i], reftitle = refs$reftitle[i])
  }
    if(!is.na(bib))  write(bib, "bib.bib", append = TRUE)
    
  }, error=function(error){ message(error)})
  
}

