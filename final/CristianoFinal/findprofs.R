library(rvest)
library(devtools)
library(plyr)
library(dplyr)

#### JHU
url <- "http://www.jhsph.edu/departments/biostatistics/directory/full-time-faculty/"
x <- html(url)
tab.jhu = html_table(x)[[1]]
is.prof <- sapply(1:nrow(tab.jhu), function(x) "Professor" %in% unlist(strsplit(tab.jhu$Title[x]," ")))
tab.jhu$Prof <- is.prof
tab.jhu <- tab.jhu[tab.jhu$Prof==TRUE, ]

jhu <- data.frame(Last = tab.jhu$"Last Name", First = tab.jhu$"First Name", School = "Johns Hopkins University", Type="Biostatistics")
### UW

url <- "http://www.biostat.washington.edu/directory/faculty_list"
x <- html(url)
tab.uw = html_table(x)[[1]][,c(1,2)]
is.prof <- !sapply(1:nrow(tab.uw), function(x) "Research" %in% unlist(strsplit(tab.uw$Title[x]," "))[1])
tab.uw$prof <- is.prof
tab.uw <- tab.uw[tab.uw$prof == TRUE, ]
first <- sapply(1:nrow(tab.uw), function(x) unlist(strsplit(tab.uw$Name[x], ", "))[2])
last <- sapply(1:nrow(tab.uw), function(x) unlist(strsplit(tab.uw$Name[x], ", "))[1])

uw <- data.frame(Last = last, First = first, School = "University of Washington", Type="Biostatistics")

### U of Minn
url <- "http://sph.umn.edu/faculty1/bio/"
x <- html(url)

x.node <- html_nodes(x, ".cn-left")
profs <- unlist(sapply(1:length(x.node), function(x) html_text(html_nodes(x.node[[x]], "a"))))
profs <- gsub(" PhD", "", profs)
profs <- gsub("^\\s+|\\s+$", "", profs)

first <- sapply(1:length(profs), function(x) {
                   name <- unlist(strsplit(profs[x], " "))
                   unlist(paste(name[-length(name)], collapse=" "))
})

last <- sapply(1:length(profs), function(x) {
                   name <- unlist(strsplit(profs[x], " "))
                   unlist(name[length(name)])
})

umn <- data.frame(Last = last, First = first, School = "University of Minnesota", Type="Biostatistics")

### Stanford
url <- "https://statistics.stanford.edu/people/faculty"
x <- html(url)
x2 <- html_nodes(x, "h3")
first <- rep(NA, 25)
last <- rep(NA, 25)
for(i in 1:25) {
    prof <- unlist(html_attrs(html_nodes(x2[[i]], "a")))
    if(is.null(prof)) next
    prof <- strsplit(gsub("/people/", "", prof, "a"), "-")[[1]]
     if(length(prof) > 2) {
         first[i] <- paste(prof[1:2], collapse=" ")
         last[i] <- prof[3]
     }
     else{
         first[i] <- prof[1]
         last[i] <- prof[2]
     }
}
first <- first[!is.na(first)]
last <- last[!is.na(last)]

tab.stan <- data.frame(Last = last, First = first, School = "Stanford", Prof = TRUE)
stan <- tab.stan[,1:3]
stan$Type="Statistics"

### University of Iowa
url <- "http://www.stat.iastate.edu/people/faculty/"
x <- html(url)
tab.ui  <- html_table(x)[[1]]
first <- sapply(1:nrow(tab.ui), function(x) unlist(strsplit(tab.ui$Name[x], ", "))[2])
last <- sapply(1:nrow(tab.ui), function(x) unlist(strsplit(tab.ui$Name[x], ", "))[1])
tab.ui <- data.frame(Last = last, First = first, School = "University of Iowa", Prof=TRUE)
ui <- tab.ui[,1:3]
ui$Type="Statistics"

### U of Chicago
url <- "https://galton.uchicago.edu/people/faculty.shtml"
x  <-  html(url)
x.meta <- html_nodes(x, "meta")[[2]]
ucprofs <- html_attrs(x.meta)[[2]]
ucprofs <- strsplit(ucprofs, ", ")[[1]]
ucprofs <- ucprofs[-1]
tab.uc <- data.frame(Name = ucprofs, School = "University of Chicago", Prof = TRUE)
tab.uc$Name <- as.character(tab.uc$Name)

first <- sapply(1:nrow(tab.uc), function(x) {
                   name <- unlist(strsplit(tab.uc$Name[x], " "))
                   unlist(paste(name[-length(name)], collapse=" "))
})

last <- sapply(1:nrow(tab.uc), function(x) {
                   name <- unlist(strsplit(tab.uc$Name[x], " "))
                   unlist(name[length(name)])
})

uc <- data.frame(Last=last, First=first, School = "University of Chicago", Type="Statistics")


dat <- rbind(jhu, uw, umn, stan, ui, uc)
write.table(dat, "statprofs.txt")

proftab <- read.table("statprofs.txt")

raw <- load("fixed.rda")
write.csv(dat, "finaldatasci.csv")

#### CLEAN DATA
load("dat.Rda")

# zeger <- dat[dat$name == "Scott Zeger",]
# zcites <- sort(zeger$cites, decreasing=TRUE)
hindex <- function(cites)  max(sapply(1:length(cites), function(x) min(cites[x], x)))

authors <- as.character(unique(dat$name))
h <- sapply(1:length(authors), function(x) hindex(sort(dat[dat$name == authors[x],"cites"], decreasing=TRUE)))
h.df <- as.data.frame(cbind(authors, h))
colnames(h.df) <- c("name", "h")
dat <- merge(dat, h.df,  by= "name", all.x=TRUE)
## h index:  max(1,N,x, sum(1,x,i,citations[i]>=x,1) )

### subset data by past 10 years
comp.10 <- dat[dat$year %in% 2005:2015,]
## remove NAs
comp.10 <- comp.10[!is.na(comp.10$journal.names),]
## look at list in decreasing order
j.data <- as.data.frame(table(comp.10$journal.names))
a <- j.data[order(-j.data$Freq), ]


####
##
journals <- "http://people.stern.nyu.edu/jsimonof/statjournals.html"

pull <- function(url, tag){
    url %>%
        read_html() %>%
        html_nodes(xpath = tag) %>%
        html_text() 
}

method.j <- pull(journals, '//a')
a <- unlist(strsplit(method.j, " "))
b <- gsub("[(:,)]", "", a)
# sort(table(b),decreasing=T)

regex <- "statistic|^stat|bernoulli|math|biometr|bayes|stochast|econometric|prob|decision|model|operation|siam|multivariate|machine learning|pattern|networks|comput|linear|time series|chaos|combinatorics|algorithm|theory"
method.ind <- grepl(regex, comp.10$journal.names, ignore.case=TRUE) 
comp.10$method <- method.ind
## look at journal names for methods
# unique(comp.10$journal.names[method.ind])
## non methods
# unique(comp.10$journal.names[!method.ind])
regexp.papers <- "statistic|bernoulli|bayes|stochast|econometric|probabl|multivariate|machine learning|matrix|combinatorics|likelihood|algorithm|least squares|principle components|wavelet"
method.articles <- grepl(regexp.papers, comp.10$article, ignore.case=TRUE)
method.ind[method.ind==0] <- method.articles[method.ind == 0]
comp.10$method <- method.ind


### SPLIT INTO BIOSTAT AND STat
comp.10$dept <- ifelse(comp.10$school %in% c("IOWA", "UC", "STANFORD"), "Stat", "Biost")

### remove Chong Wang
comp.10 <- comp.10[comp.10$name != "Chong WANG", ]
### remove Yehua Li
comp.10 <- comp.10[comp.10$name != "Yehua LI", ]
### Remove Andrew Mugglin (non tenure-track)
comp.10 <- comp.10[comp.10$name != "Adrew Mugglin", ]
### Remove Mihai (just part time prof)
comp.10 <- comp.10[comp.10$name != "Mihai Anitescu",]
### Remove Fang Han (visiting professor)
comp.10 <- comp.10[comp.10$name != "Fang,Han",]

### GET H INDEX
## for author
# h <- sapply(1:length(authors), function(x) hindex(sort(dat[dat$name == authors[x],"cites"], decreasing=TRUE)))

### table
tabx <- table( comp.10$method, comp.10$dept)
## odds ratio of a biostatics department publishing collaboritive work relative to stats
OR <- tabx[1,1]*tabx[2,2]/(tabx[1,2]*tabx[2,1])

### use relative risk
p.biostat <- tabx[2,1]/(tabx[2,1] + tabx[1,1])
p.stat <- tabx[2,2]/(tabx[1,2] + tabx[2,2])

RR <- p.biostat/p.stat
se.logrr <- sqrt((1/tabx[2,1] + 1/tabx[2,2]) - (1/(tabx[2,1] + tabx[1,1]) + 1/(tabx[1,2] + tabx[2,2])))
logCI <- log(RR) + c(-1,1)*se.logrr * 1.96
CI <- exp(logCI)
p.biostat
p.stat
RR
CI

## h index is not integer
comp.10$h <- as.integer(comp.10$h)

### get some measure of error in classifications (manually)
set.seed(13)
## sample methods papers
ind.method <- sample(which(comp.10$method == TRUE), 100)
ind.collab <- sample(which(comp.10$method == FALSE), 100)


#comp.10$article[ind.method]
# write.csv(comp.10$article[ind.collab], "collabpapers.csv")



### plotting stuff
library(ggplot2)
school.tab <- table(comp.10$school)
school.levels <- names(school.tab)[c(1,5,6,2,3,4)]
comp.10$school <- factor(comp.10$school, levels=school.levels)

## get number of faculty for each department
ddply(comp.10, .(school, name), summarize, length(name))
### Chong Wang has 602 papers???
ggplot(comp.10, aes(school, fill=method)) + geom_bar(position="dodge") +
    scale_fill_discrete(labels=c("Collab", "Method")) +
    theme(legend.title=element_blank()) + theme_bw() +
    labs(x = "School", y = "Number of publications", title= "Total methodological and collaborative papers by school")

datbyschool <- ddply(comp.10, .(school), summarize, npapers = length(name), nfaculty=length(unique(name)), nmethods = length(name[method==TRUE]))
datbyschool$p <- datbyschool$nmethods/datbyschool$npapers
datbyschool
ddply(comp.10, .(school, year, dept), summarize, npapers = length(name))

xdat <- ddply(comp.10, .(school, year, dept), summarize, npaper = length(name),
              nmethod = length(name[method==TRUE]), ncollab = length(name[method==FALSE]),
              p = nmethod/npaper)

ggplot(xdat, aes(year, p, group=school, colour=school, linetype=dept)) + geom_line() +
    ylim(c(0, 0.8)) + theme_bw() + labs(x = "Year", y = "Proportion of Methods Papers", title = "Proportion of methods papers to collaborative papers by year")


## get error rates



