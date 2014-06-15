graphics.off();
remove(list=ls());

library(xlsx);
library(plyr);

## Read in the commitments file

commitments <- read.csv(file="./lawson_csv_exports/commitments.csv",
         skip=1, header=FALSE, stringsAsFactors=FALSE);

commitments <- unique(commitments[which(!is.na(commitments[,9])),c(8:15)]);
colnames(commitments) <- c("V8", "Account.Category", "Category.Description", "Category.Committed.Subtotal", 
                           "V12", "Activity", "Activity Description", "Activity.Committed.Total");

commitments <- commitments[,c(2,3,4,6)];

#Extract the date from the first line of the commitments file
commitments.date <- read.csv(file="./lawson_csv_exports/commitments.csv", nrows=1, header=FALSE, 
                             stringsAsFactors=FALSE);
commitments.date <- commitments.date$V2;
commitments.date <- unlist(strsplit(x=commitments.date, split="Commitments Recorded as of:  "))[2];
commitments.date <- unlist(strsplit(x=commitments.date, split=" @"))[1];
commitments.date <- as.Date(commitments.date, format="%m/%d/%Y");

## Read in the expenditures file
expenses <- read.csv(file="./lawson_csv_exports/expenditures.csv",
                     skip=6, header=FALSE, stringsAsFactors=FALSE);
expenses <- unique(expenses[,c(1,13:16)]);

last <- nrow(expenses);
exp.acctCat <- substr(expenses$V1, start=1, stop=5);
exp.acctCat <- exp.acctCat[-last];

exp.acctDesc <- substring(text=expenses$V1, first=8);
exp.acctDesc <- exp.acctDesc[-last];

subtotal <- expenses$V14;
subtotal <- subtotal[-last];

activity <- unlist(strsplit(x = expenses$V15, split=" TOTAL:"));

expenses <-data.frame(exp.acctCat=exp.acctCat, exp.acctDesc=exp.acctDesc, 
                      subtotal=subtotal, activity=activity, stringsAsFactors=FALSE);

names(expenses) <- c("Account.Category", "Category.Description", "Expenses.LTD", "Activity");

#Extract the date from the first line of the expenditures file
expenses.date <- read.csv(file="./lawson_csv_exports/expenditures.csv", nrows=1, header=FALSE,
                          stringsAsFactors=FALSE);
expenses.date <- expenses.date$V2;
expenses.date <- unlist(strsplit(x=expenses.date, split="Printed: "))[2];

expenses.date <- unlist(strsplit(x=expenses.date, split=" "))[1];
expenses.date <- as.Date(expenses.date, format="%m/%d/%Y");

# Make sure the dates match between the commitments and the expenses report
if(expenses.date!=commitments.date){
  stop("Dates do not match");
}

## Read in Activity Information, budgets, and broad categories
activities <- read.xlsx(file = "./manual_categories/Kean Finance Info.xlsx", 
                        sheetName="Activity Info", stingsAsFactors=FALSE);
budgets <- read.xlsx(file = "./manual_categories/Kean Finance Info.xlsx", 
                    sheetName="Budgets", stringsAsFactors=FALSE);

broad_categories <- read.xlsx(file = "./manual_categories/Kean Finance Info.xlsx", 
                              sheetName="Broad Categories", stringsAsFactors=FALSE);


broad_categories$Account.Category <- as.numeric(broad_categories$Account.Category);
broad_categories <- broad_categories[,-2];

activities$total.days = activities$Budget.Ends - activities$Budget.Starts

## Add broad categories to the budget and calculate the estimated amount we 
## should have spent by the date

budgets = join(x=budgets, y=broad_categories);
budgets <- budgets[,c("Activity", "Total.Budget", "Broad.Category")];
budgets <- ddply(budgets, .(Activity, Broad.Category), summarize, Total.Budget=sum(Total.Budget));
activities$Elapsed.Days = expenses.date - activities$Budget.Starts;
activities$Effective.Days = apply(X=cbind(activities$total.days, activities$Elapsed.Days), MARGIN=1,
                                  FUN=min);
budgets <- join(x=budgets, y=data.frame(Activity=activities$Activity.Number, Total.Days= as.numeric(activities$total.days), 
                                   Effective.Days=as.numeric(activities$Effective.Days)), by="Activity");
budgets$Expected.Spending = with(budgets, ((Total.Budget/Total.Days)*Effective.Days));

## Add broad categories to expenses
expenses <-join(x=expenses, y=broad_categories);
## Remove Cost transfers and indirects
expenses <- expenses[!(expenses$Account.Category==79900 | expenses$Account.Category==76129),];

## If we missed any categories in expenses, mark them as Other
naRows = which(is.na(expenses$Broad.Category));
if(length(naRows>0)){
  expenses$Broad.Category[naRows] = "Other";
}

#Remove commas and convert the expenses column to numeric
expenses$Expenses.LTD <- as.numeric(gsub(pattern=",", replacement="", x=expenses$Expenses.LTD));

## For each Activity and Broad Category, sum all the expenses
expenses<- ddply(expenses, .(Activity, Broad.Category), summarize, Total.Expenses=sum(Expenses.LTD));

## Create a TOTAL DIRECT category for expenses
exp.total <- ddply(expenses, .(Activity), summarize, Total.Expenses=sum(Total.Expenses, na.rm=TRUE));

exp.total <- data.frame(Activity=exp.total$Activity, Broad.Category="TOTAL DIRECT", Total.Expenses=exp.total$Total.Expenses,
                        stringsAsFactors=FALSE);

expenses <- rbind(exp.total, expenses);

## Add broad categories to commitments
commitments <- join(x=commitments, y=broad_categories);

## Remove commas and convert the commitments subtotal column to numeric
commitments$Category.Committed.Subtotal <- as.numeric(gsub(pattern=",", replacement="", 
                        x=commitments$Category.Committed.Subtotal));

## If we missed any categories, mark them as Other
naRows = which(is.na(commitments$Broad.Category));
if(length(naRows>0)){
  commitments$Broad.Category[naRows] = "Other";
}

## For each Activity and Broad Category, sum all the commitments
commitments<- ddply(commitments, .(Activity, Broad.Category), summarize, Total.Commitments=sum(Category.Committed.Subtotal));

## Create a TOTAL DIRECT category for commitments
## Create a TOTAL DIRECT category for expenses
com.total <- ddply(commitments, .(Activity), summarize, Total.Commitments=sum(Total.Commitments, na.rm=TRUE));

com.total <- data.frame(Activity=com.total$Activity, Broad.Category="TOTAL DIRECT", Total.Commitments=com.total$Total.Commitments,
                        stringsAsFactors=FALSE);

commitments <- rbind(com.total, commitments);

## Add expenses and commitments to budget, as well as date
budgets <- join(x=budgets, y=expenses, by=c("Activity","Broad.Category"));
budgets <- join(x=budgets, y=commitments, by=c("Activity","Broad.Category"));
budgets$date <- expenses.date;
budgets$Total.Expenses[is.na(budgets$Total.Expenses)] = 0;
budgets$Total.Commitments[is.na(budgets$Total.Commitments)] = 0;

outFile <- paste("./tidyOutput/Tidy finance data ", expenses.date,".csv",sep="");
write.csv(file=outFile, x=budgets, row.names=FALSE);



