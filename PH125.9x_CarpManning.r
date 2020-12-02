if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(pdftools)) install.packages("pdftools", repos = "http://cran.us.r-project.org")
if(!require(usmap)) install.packages("usmap", repos = "http://cran.us.r-project.org")
if(!require(treemapify)) install.packages("treemapify", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")

##############
# Import Data
##############

# Check to see if the file(s) have been downloaded to the working directory
# If the files exist and are readable, import them from the local copy
# If there are any problem, fail over to online source(s)

# Excel file with core data
file_path <- "FedCourtDatabase.xlsx"
file_url <- "https://www.umassd.edu/media/umassdartmouth/political-science/facultydocs/fdcdata_thru_2012_n=110977.xlsx"
CMget <- try(read_excel(file_path))
if (file.exists(file_path) && !inherits(CMget, "try-error")) {
    CM <- read_excel(file_path)
} else {
    GET(file_url, write_disk(dl <- tempfile(fileext = ".xlsx")))
    CM <- read_excel(dl)
}

# PDF file with reference tables and data information
pdf_path <- "DatabaseManual.pdf"
pdf_url <- "https://www.umassd.edu/media/umassdartmouth/political-science/facultydocs/codebook-5-24-2016.pdf"
PDFget <- try(CM_text <- pdf_text(pdf_path))
if (file.exists(pdf_path) && !inherits(PDFget, "try-error")) {
    CM_text <- pdf_text(pdf_path)
} else {
    CM_text <- pdf_text(pdf_url)
}

#####################
# Mine data from PDF
#####################

# Target page numbers for each reference table
CRTPOINT_pages <- c(5,6,7,8,9,10,11,12,13)
CIRCUIT_pages <- c(15)
STATE_pages <- c(16, 17)
STATDIST_pages <- c(17, 18, 19, 20)
CASETYPE_pages <- c(24, 25)
CATEGORY_pages <- c(39)
APPRES_pages <- c(43)
DEMOGRAPHIC_pages <- c(44)

# Identify data format for each reference table
CRTPOINT_regex <- "^\\d\\d\\d"
CIRCUIT_regex <- "^\\d\\d"
STATE_regex <- "^\\d\\d"
STATDIST_regex <- "^\\d\\d\\d"
CASETYPE_regex <- "^\\d\\d"
CATEGORY_regex <- "^\\d"
APPRES_regex <- "^\\d\\d"
DEMOGRAPHIC_regex <- "^\\d"

# Function to extract tables
# Stage 0 - Convert all dash objects to standard dash
# Stage 1 - Break each page into lines
# Stage 2 - Split each line on standard dash and trim excess white space
# Stage 3 - Test each row against required data format and remove mishits
# Return a collection of data frames, one for each page targeted
get_CM_tables <- function(CM_page, CM_format) {
    stage0 <- gsub("\\s[[:punct:]]+\\s)", " - ", CM_text[[CM_page]])
    stage0 <- gsub(" – ", " - ", CM_text[[CM_page]])
    stage1 <- str_split(stage0, "\n", simplify=TRUE)
    stage2 <- str_split(stage1, " - ", simplify=TRUE)
    stage2[,] <- trimws(stage2[,])
    stage3 <- stage2[which(grepl(CM_format, stage2[,1])),]
    stage3 <- as.data.frame(stage3[which(stage3[,2] != ''),])
    return(stage3)
}

# Run function for each table
# Join all outputs into a single data frame
# Rename columns for data matching to main table
CRTPOINT_table <- lapply(CRTPOINT_pages, get_CM_tables, CM_format=CRTPOINT_regex)
CRTPOINT_table <- ldply(CRTPOINT_table, data.frame)
colnames(CRTPOINT_table) <- c("crtpoint", "location")

CIRCUIT_table <-lapply(CIRCUIT_pages, get_CM_tables, CM_format=CIRCUIT_regex)
CIRCUIT_table <- ldply(CIRCUIT_table, data.frame)
colnames(CIRCUIT_table) <- c("circuit", "courtofappeals")

STATE_table <- lapply(STATE_pages, get_CM_tables, CM_format=STATE_regex)
STATE_table <- ldply(STATE_table, data.frame)
colnames(STATE_table) <- c("state", "statename")

STATDIST_table <- lapply(STATDIST_pages, get_CM_tables, CM_format=STATDIST_regex)
STATDIST_table <- ldply(STATDIST_table, data.frame)
colnames(STATDIST_table) <- c("statdist", "districtcourt")

CASETYPE_table <- lapply(CASETYPE_pages, get_CM_tables, CM_format=CASETYPE_regex)
CASETYPE_table <- ldply(CASETYPE_table, data.frame)
colnames(CASETYPE_table) <- c("casetype", "typeofcase")

CATEGORY_table <- lapply(CATEGORY_pages, get_CM_tables, CM_format=CATEGORY_regex)
CATEGORY_table <- ldply(CATEGORY_table, data.frame)
colnames(CATEGORY_table) <- c("category", "generaltypeofcase")

APPRES_table <- lapply(APPRES_pages, get_CM_tables, CM_format=APPRES_regex)
APPRES_table <- ldply(APPRES_table, data.frame)
colnames(APPRES_table) <- c("appres", "presname")

#This is outside data I mapped because I was interested
APPRES_table$prespartyname <- ifelse(APPRES_table$appres %in% c(25, 26, 27, 29, 30, 31, 34, 37, 38, 40, 41, 43), "Republican", "Democrat")

# Demographics were intially treated as a group because all the information was on a single page
# After processing, the collections were parsed into individual tables
DEMOGRAPHIC_table <- lapply(DEMOGRAPHIC_pages, get_CM_tables, CM_format=DEMOGRAPHIC_regex)
DEMOGRAPHIC_table <- ldply(DEMOGRAPHIC_table, data.frame)

PARTY_table <- DEMOGRAPHIC_table[1:3,]
colnames(PARTY_table) <- c("party", "judgeparty")

GENDER_table <- DEMOGRAPHIC_table[4:5,]
colnames(GENDER_table) <- c("gender", "judgegender")

RACE_table <- DEMOGRAPHIC_table[6:10,]
colnames(RACE_table) <- c("race", "judgerace")

detach(package:plyr) # this package causes trouble if it is loaded along side 'dplyr' so we have to unload it

# Set up table for matching from reference material
CM <- CM %>% mutate(
    judge=as.factor(judge), 
    crtpoint=as.factor(sprintf('%03d', crtpoint)), 
    circuit=as.factor(sprintf('%02d', circuit)), 
    state=as.factor(sprintf('%02d', state)), 
    statdist=as.factor(sprintf('%03d', statdist)), 
    month=as.integer(month), 
    year=as.integer(year), 
    libcon=as.integer(libcon), 
    casetype=as.factor(sprintf('%02d', casetype)), 
    category=as.factor(category), 
    apyear=as.integer(apyear), 
    appres=as.factor(sprintf('%02d', appres)), 
    party=as.factor(party), 
    race=as.factor(race) , 
    gender=as.factor(gender),
    tenure=(as.integer(year) - as.integer(apyear))
    )

# Match and add reference information
CM_master <- CM %>% 
    left_join(CRTPOINT_table, by="crtpoint") %>%
    left_join(CIRCUIT_table, by="circuit") %>%
    left_join(STATE_table, by="state") %>%
    left_join(STATDIST_table, by="statdist") %>%
    left_join(CASETYPE_table, by="casetype") %>%
    left_join(CATEGORY_table, by="category") %>%
    left_join(APPRES_table, by="appres") %>%
    left_join(PARTY_table , by="party") %>%
    left_join(GENDER_table , by="gender") %>%
    left_join(RACE_table, by="race")

CM_master$presparty <- ifelse(CM$appres %in% c(25, 26, 27, 29, 30, 31, 34, 37, 38, 40, 41, 43), 0, 1)

###################################
# Determine Test and Training Sets
###################################

# Test set will be a 15% holdout the of Carp-Manning data
set.seed(61, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = CM$libcon, times = 1, p = 0.15, list = FALSE)
CMtraining <- CM_master[-test_index,]
CMtemp <- CM_master[test_index,]

# Make sure "judge" and "casetype" in test set are also in CMtraining set
CMtest <- CMtemp %>% 
      semi_join(CMtraining, by = "judge") %>%
      semi_join(CMtraining, by = "typeofcase")

# Add rows removed from test set back into CMtraining set
removed <- anti_join(CMtemp, CMtest)

CMtraining <- rbind(CMtraining, removed)

# Clean up
rm(removed, CMget, dl, CMtemp, test_index, file_path, file_url, pdf_url, CM_text, DEMOGRAPHIC_table, CRTPOINT_pages, CIRCUIT_pages, STATE_pages, STATDIST_pages, CASETYPE_pages, CATEGORY_pages, APPRES_pages, DEMOGRAPHIC_pages, CRTPOINT_regex, CIRCUIT_regex, STATE_regex, STATDIST_regex, CASETYPE_regex, CATEGORY_regex, APPRES_regex, DEMOGRAPHIC_regex, PDFget, pdf_path, CRTPOINT_table, CIRCUIT_table, STATE_table, STATDIST_table, CASETYPE_table, CATEGORY_table, APPRES_table, PARTY_table, GENDER_table, RACE_table)

###################################
# Visualizations
###################################

# state state graph
tend_state <- CMtraining %>% group_by(statename) %>% summarise(avgyear = (mean(libcon)-.5))
colnames(tend_state) <- c("state", "libcon")
plot_usmap(data = tend_state, values = "libcon", color="white") +  scale_fill_continuous(name= "", low = "lightpink2", high = "dodgerblue", label=c("v.cons", "cons", "m.con", "m.lib", "lib", "v.lib")) + theme(legend.position = "right")

# state bar chart
tend_state$tend <- ifelse(tend_state$libcon > 0, "liberal", "conservative")
tend_state <- tend_state[order(tend_state$libcon), ]
tend_state$state <- factor(tend_state$state, levels=tend_state$state)
ggplot(data=tend_state, aes(x=state, y=libcon, label=libcon)) + geom_bar(stat='identity', aes(fill=tend)) +  scale_fill_manual(name= "", labels = c("Conservative", "Liberal"), values = c("liberal"="dodgerblue", "conservative"="lightpink2")) + ylab("") + xlab("") + scale_y_continuous(label=c("cons", "m.cons", "ind", "m.lib", "lib", "v.lib")) + coord_flip() + theme_classic() + theme(legend.position="none")
rm(tend_state)

# race plot
tend_race <- CMtraining %>% group_by(judgerace, year) %>% summarise(avgyear = (mean(libcon)-.5))
tend_race$judgerace <- ifelse(tend_race$judgerace== "African-American/black", "Af.Amer", ifelse(tend_race$judgerace == "white/Caucasian", "C.Amer", ifelse(tend_race$judgerace == "Latino/Hispanic", "L.Amer", ifelse(tend_race$judgerace =="Native American", "N.Amer" , ifelse(tend_race$judgerace== "Asian American", "As.Amer", "N/A")))))
ggplot(data= tend_race, aes(year, avgyear, color=judgerace)) + geom_point() + geom_smooth(method="glm", se=FALSE) + labs(x="Year", y="") + geom_smooth(method="loess", se=FALSE) + scale_y_continuous(labels=c("cons", "m.cons", "ind", "m.lib", "lib")) + scale_colour_manual(name="", values = c("Af.Amer"="dodgerblue", "C.Amer"="lightpink2", "L.Amer"="palegreen3", "N.Amer" = "gold", "As.Amer" = "grey45" )) + theme_classic()
rm(tend_race)

# gender plot
tend_gender <- CMtraining %>% group_by(judgegender, year) %>% summarise(avgyear = (mean(libcon)-.5))
ggplot(data= tend_gender, aes(year, avgyear, color=judgegender)) + geom_point() + geom_smooth(method="loess", se=FALSE) + labs(x="Year", y="") + scale_y_continuous(labels=c("cons", "m.cons", "ind", "m.lib", "lib")) + scale_colour_manual(name="", values = c("female"="dodgerblue", "male"="lightpink2")) + theme_classic()

# gender plot with cofidence intervals
ggplot(data= tend_gender, aes(year, avgyear, color=judgegender)) + geom_point() + geom_smooth(method="loess", se=TRUE, aes(fill=judgegender)) + labs(x="Year", y="") + scale_y_continuous(labels=c("vcons","cons", "m.cons", "ind", "m.lib", "lib")) + scale_colour_manual(name="", values = c("female"="dodgerblue", "male"="lightpink2")) + scale_fill_manual(name="", values = c("female"="dodgerblue", "male"="lightpink2")) + theme_classic()
rm(tend_gender)

# judge political party plot
tend_party <- CMtraining %>% group_by(judgeparty, year) %>% summarise(avgyear = (mean(libcon)-.5))
tend_party <- tend_party[complete.cases(tend_party), ] # Removing data with party designation "99"
tend_party$judgeparty <- ifelse(tend_party$judgeparty=="Independent/Other/Unknown", "Ind", ifelse(tend_party$judgeparty=="Democrat", "Dem", ifelse(tend_party$judgeparty=="Republican", "Rep", "N/A")))
ggplot(data= tend_party, aes(year, avgyear, color=judgeparty)) + geom_point() + geom_smooth(method="loess", se=FALSE) + labs(x="Year", y="") + scale_y_continuous(labels=c("cons", "m.cons", "ind", "m.lib", "lib")) + scale_colour_manual(name="", values = c("Dem"="dodgerblue", "Rep"="lightpink2", "Ind"="palegreen3")) + theme_classic()
rm(tend_party)

# plot for appointing president
tend_pres <- CMtraining %>% group_by(presname, year, judge) %>% summarise(avgyear = (mean(libcon)-.5), count=n())
ggplot(tend_pres, aes(x=year, y=avgyear, color=presname)) + geom_point() + labs(x="Year", y="") + scale_y_continuous(labels=c("cons", "m.cons", "ind", "m.lib", "lib")) + theme_classic()
rm(tend_pres)

# plot for appointing president's political party
tend_pres <- CMtraining %>% group_by(prespartyname, year) %>% summarise(avgyear = (mean(libcon)-.5), count=n())
ggplot(tend_pres, aes(x=year, y=avgyear, color=prespartyname)) + geom_point() + labs(x="Year", y="") + geom_smooth(method="loess", se=FALSE) + scale_y_continuous(labels=c("cons", "m.cons", "ind", "m.lib", "lib")) + scale_colour_manual(name="", values = c("Democrat"="dodgerblue", "Republican"="lightpink2")) + theme_classic()
rm(tend_pres)

# effect of how long the judge has been serving
tend_duration <- CMtraining %>% group_by(tenure, judge) %>% summarise(avgyear = (mean(libcon)-.5), count=n())
tend_duration <- tend_duration[tend_duration$tenure > -1, ]
tend_duration$sign <- ifelse(tend_duration$avgyear < 0, -1, 1)
tend_duration$count <- tend_duration$count*tend_duration$sign
ggplot(tend_duration, aes(x=tenure, y=avgyear)) + geom_point(aes(color= avgyear, size=abs(count))) + scale_color_gradient(low="lightpink2", high="dodgerblue") + scale_y_continuous(labels=c("cons", "m.cons", "ind", "m.lib", "lib")) + geom_smooth(method="loess", color="purple", fill="plum1") + labs(x="Years in Office", y="") + theme_classic() + theme(legend.position="none") 
rm(tend_duration)

# tree plot of casetype
treeMap <- CMtraining %>% group_by(generaltypeofcase, typeofcase) %>% summarise(avgtype = (mean(libcon)-.5), count=n())
treeMap$tend <- ifelse(treeMap$avgtype < 0, "conservative", "liberal")
treeMap$generaltypeofcase <- str_replace(treeMap$generaltypeofcase, " case", "")
ggplot(data=treeMap, aes(area = count, fill=avgtype, label=typeofcase, subgroup=generaltypeofcase)) + geom_treemap() + geom_treemap_subgroup_border(color="black") + geom_treemap_subgroup_text(color="black") + geom_treemap_text(fontface = "italic", color = "white", place = "topleft", reflow = TRUE) + scale_fill_continuous(low = "lightpink2", high = "dodgerblue", name = "", label=c("cons", "m.cons", "ind", "m.lib", "lib")) + theme_classic()
rm(treeMap)

# plot of case categories
trend_cat <- CMtraining %>% group_by(generaltypeofcase, year) %>% summarise(avgtype = (mean(libcon)-.5), count=n())
trend_cat$generaltypeofcase <- str_replace(trend_cat$generaltypeofcase, " case", "")
ggplot(trend_cat, aes(x=year, y=avgtype, color=generaltypeofcase)) + geom_point() + geom_smooth(method="loess", se=FALSE) + labs(x="Year", y="") + scale_y_continuous(labels=c("conservative", "m. conservative", "neutral", "m. liberal", "liberal")) + scale_colour_manual(name="", values = c("Economic Regulation and/or Labor"="dodgerblue", "Criminal Justice"="lightpink2", "Civil Liberties/Rights"="palegreen3")) + theme_classic()
rm(trend_cat)

# select predictors
CMtraining_selected <- CMtraining %>% select(appres, presparty, state, category, gender, race, tenure, casetype, libcon)
CMtest_selected <- CMtest %>% select(appres, presparty, state, category, gender, race, tenure, casetype, libcon)

CMtraining_selected <- CMtraining_selected %>% mutate(libcon=as.factor(libcon))
CMtest_selected <- CMtest_selected %>% mutate(libcon=as.factor(libcon))

# define cross-validation setup
control <- trainControl(method = "boot", number = 10, p=.85)

# define evaluation metric [Matthews correlation coefficient]
MCC <- function (conf_matrix) {
TP <- conf_matrix$table[1,1]
TN <- conf_matrix$table[2,2]
FP <- conf_matrix$table[1,2]
FN <- conf_matrix$table[2,1]

mcc_num <- (TP*TN - FP*FN)
mcc_den <- as.double((TP+FP))*as.double((TP+FN))*as.double((TN+FP))*as.double((TN+FN))

mcc_final <- ifelse(mcc_den == 0, 0, mcc_num/sqrt(mcc_den))
return(mcc_final)
}

#########
# Models
#########

# Linear
# model
train_linear <- train(libcon~., data=CMtraining_selected, method="glm", na.action=na.omit, trControl=control)
# made predictions
y_hat_linear <- predict(train_linear, CMtest_selected)
# create confusion matrix
linear_fit <- confusionMatrix(y_hat_linear, CMtest_selected$libcon)
# calculate MCC
linearMCC <- MCC(linear_fit)
# record results
Results <- tribble(
  ~method, ~MCC,
  "Linear",   linearMCC)

# Naive Bayes
# set tuning grid
nb_grid <-   expand.grid(usekernel = c(TRUE, FALSE),
                        laplace = c(0, 0.5, 1), 
                        adjust = c(0.75, 1, 1.25, 1.5))
# model
train_nb <- train(libcon~., data=CMtraining_selected, method="naive_bayes", na.action=na.omit, trControl=control, tuneGrid=nb_grid)
# made predictions
y_hat_nb <- predict(train_nb, CMtest_selected)
# create confusion matrix
nb_fit <- confusionMatrix(y_hat_nb, CMtest_selected$libcon)
# calculate MCC
nbMCC <- MCC(nb_fit)
# record results
Results <- add_row(Results, method = "Naive Bayes", MCC = nbMCC)

# Random Forest
# set tuning grid
rf_grid <-   expand.grid(predFixed = c(3,5,7),
                        minNode = c(50,100))
# model
train_rf <- train(libcon~., data=CMtraining_selected, method="Rborist", na.action=na.omit, trControl=control, tuneGrid=rf_grid)
# made predictions
y_hat_rf <- predict(train_rf, CMtest_selected)
# create confusion matrix
rf_fit <- confusionMatrix(y_hat_rf, CMtest_selected$libcon)
# calculate MCC
rfMCC <- MCC(rf_fit)
# record results
Results <- add_row(Results, method = "Random Forest", MCC = rfMCC)

# Ensemble
ES <- data.frame(y_hat_linear, y_hat_rf, y_hat_nb)
# made predictions
ES <- ES %>% mutate(totals=(as.integer(y_hat_linear)+ as.integer(y_hat_rf)+ as.integer(y_hat_nb)), y_hat_es=as.factor(ifelse(totals > 4, 1, 0)))
# create confusion matrix
ensemble_fit <- confusionMatrix(ES$y_hat_es, CMtest_selected$libcon)
# calculate MCC
ensembleMCC <- MCC(ensemble_fit)
# record results
Results <- add_row(Results, method = "Ensemble", MCC = ensembleMCC)

Results