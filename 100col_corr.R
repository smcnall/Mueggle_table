#for using Ubuntu use read.table command:
#df <- read.table(file="Downloads/Muegge_OTUtable_nr.txt", header=TRUE)
#see read.table 
#for windows use:
df<-read.delim("C:/Users/sean/Downloads/Muegge_OTUtable_nr.txt", stringsAsFactors=FALSE, header = TRUE)
df<- df[ , -which(names(df) %in% c("X"))]

#we have to initial some variables so that R knows what to do the first
#time we run this
OTUvec1 = NULL
OTUvec2 = NULL
tau     = NULL
p_value = NULL

#note that you can specify any variable to increment over in a for loop, not just i
#also, you can specify the specific range using the notation 1:10, which indicates
#start at position 1, increment until position 10. In this case, we want the number
#of columns, so length(colnames(df)) does the trick
#set up to run through first 100 columns
#finally, a df has either colnames or rownames, but no "names"
for (k in 1:100){
  for (l in 1:100){
    #we want to avoid the condition where we compare a column to itself
    if( k == l ){
      next  
    }
    #also, we don't want to repeat comparisons
    #works pretty well for the first 100 columns, although the time it takes to form the table is very long
    if( k < l ){
      next
    }    
    re <- cor.test(df[,k],df[,l],method = "kendall") #paranthesis around "kendal" are unnecessary, note spelling of kendall
    OTUvec1 = c(OTUvec1,colnames(df[l]))
    OTUvec2 = c(OTUvec2,colnames(df[k]))
    tau = c(tau, as.vector(re$estimate))
    p_value = c(p_value, as.vector(re$p.value))
  }
}
d<-data.frame(OTUvec1, OTUvec2, tau, p_value)
  
    
    
    
    


