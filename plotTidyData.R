## For each activity, plot for the different broad categories the expected spending versus total expenses, then the 
## total budget versus total expenses plus commitments

home = "Y:/_Projects/Programming/server_learning/shiny_finance";

library(lattice);
library(reshape2);
library(ggplot2);
setwd(home);
setwd("./tidyOutput/");

fileList = list.files(pattern=".csv");

masterTable=NULL;

for(thisFile in fileList){
  temp = read.csv(thisFile);
  masterTable <- rbind(masterTable, temp);
  
  
}

setwd("../pdfReports/");

##Plot expense vs budget
b2 = melt(id.vars=c("Activity", "Broad.Category", "date"), measure.vars=c("Expected.Spending","Total.Expenses"),
          data=masterTable);

catList <- unique(b2$Broad.Category);
for(thisCat in catList){
  b.this <- b2[b2$Broad.Category==thisCat,]
  title = paste("Spending vs. Budget for", thisCat);
  b.this$Activity <- as.factor(b.this$Activity);
  g <- ggplot(b.this, aes(date, value, group=variable)) + geom_line(aes(color=variable)) + geom_point(aes(color=variable));
  g <- g + facet_wrap(~ Activity, ncol=2, scales="free");
  g <- g + labs(x="Date", y="US Dollars", title=title);
  print(g);
  dev.copy2pdf(file=paste(title,".pdf",sep=""));
  dev.off();
}

##Plot commitments + expenses vs total budgets
masterTable$Total.Exp.And.Commit = masterTable$Total.Commitments + masterTable$Total.Expenses;
b3 = melt(id.vars=c("Activity", "Broad.Category", "date"), measure.vars=c("Total.Budget","Total.Exp.And.Commit"),
          data=masterTable);

catList <- unique(b3$Broad.Category);
for(thisCat in catList){
  b.this <- b3[b3$Broad.Category==thisCat,]
  title = paste("Commitment and Expenses vs. Total Budget for", thisCat);
  b.this$Activity <- as.factor(b.this$Activity);
  g <- ggplot(b.this, aes(date, value, group=variable)) + geom_line(aes(color=variable)) + geom_point(aes(color=variable));
  g <- g + facet_wrap(~ Activity, ncol=2, scales="free");
  g <- g + labs(x="Date", y="US Dollars", title=title);
  print(g);
  dev.copy2pdf(file=paste(title,".pdf",sep=""));
  dev.off();
}

