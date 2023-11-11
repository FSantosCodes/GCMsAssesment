

#input references (format MDPI)
ref_file <- "F:/DATA_AUX1/sensitivityModels/4_references/ref.txt"


#open file
ref.data <- read.delim(ref_file,header=F,encoding ="UTF-8")[,2]
ref.data <- lapply(ref.data,function(x){
  x.data <- unlist(strsplit(x,'“|”'))
  x.yrs <- unlist(strsplit(x.data[3],","))
  x.yrs <- x.yrs[grep("doi:",x.yrs)-1]
  if(length(x.yrs)==0){
    x.yrs <- 0
  }
  x.df <- data.frame(authors=x.data[1],
                     title=x.data[2],
                     journal=unlist(strsplit(x.data[3],","))[1],
                     year=x.yrs)
  return(x.df)
})
ref.data <- do.call("rbind.data.frame",ref.data)
journal.freq <- as.data.frame(table(ref.data$journal))
journal.freq <- journal.freq[order(journal.freq$Freq,decreasing = T),]







