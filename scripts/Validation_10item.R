##############################################
#
# Validation of FlexER Scale - 10 Item Version
#
# Denise Dörfel, Anne Gärtner, Christoph Scheffel
#
# Technische Universität Dresden
##############################################

# Load relevant packages

library(here)             # find path to Rproj
here::i_am("FlexERScale.Rproj")
library(renv)             # for computational reproducibility

library(dplyr)          
library(foreign)
library(psych)
library(psychometric)
library(tidyr)
library(ggplot2)
library(tibble)
library(reshape2)
library(MVN)
library(GPArotation)
library(nFactors)

### DATA IMPORT

# data import study 1 (Patricia)
data_pat <- read.csv(here::here("datasheets", "10item", "ERFlexibility_DATA_2023-03-13_1603.csv"))

# assign labels
data_pat$sex <- factor(data_pat$sex, levels = c("1", "2", "3"), labels = c("female", "male", "divers"))
data_pat$degree <- factor(data_pat$degree, levels = c("1", "2", "3"), labels = c("Hauptschule", "Realschule", "Abitur"))
data_pat$degree_job <- factor(data_pat$degree_job, levels = c("1", "2", "3", "4", "5", "6"), labels = c("none", "Ausbildung", "Fachhochschulabschluss", "Hochschulabschluss", "Promotion", "Habilitation"))
data_pat$study <- factor(data_pat$study, levels = c("1", "2"), labels = c("yes", "no"))

# data import study 2 (Berndt)
data_berndt <- foreign::read.spss(here::here("datasheets", "10item", "Berndt.sav"), to.data.frame=TRUE, use.value.labels = FALSE)

# assign labels
data_berndt$Sex <- factor(data_berndt$Sex, levels = c("1", "2", "3"), labels = c("female", "male", "divers"))
data_berndt$Education <- factor(data_berndt$Education , levels = c("1", "2", "3", "4", "5", "6", "7", "8"), labels = c("", "kein Abschluss", "Hauptschulabschluss", "Mittlere Reife", "Abgeschlossene Lehre", "Fachabitur", "Abitur", "Fachhochschul-/Hochschulabschluss"))
data_berndt$Occupation <- factor(data_berndt$Occupation , levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), labels = c("Arbeitslos", "In Ausbildung", "StudentIn", "Angestellte/r Rettungsdienst", "Beamte/r im Bereich Feuerwehr/Rettungsdienst", "Selbstständig", "", "Sonstiges", "Beamte/r, sonstiges", "Angestellte/r, sonstiges"))

# data import study 3 (CAD - Scheffel,Zerna)
data_cad <- read.csv(here::here("datasheets", "10item", "CAD_quest.csv"))
data_cad$gender <- factor(data_cad$gender, levels = c("1", "2", "3"), labels = c("male", "female", "divers"))
data_cad$edu <- factor(data_cad$edu , levels = c("1", "2", "3"), labels = c("Hauptschulabschluss", "Mittlere Reife", "Abitur"))

### EXTRACT AND RENAME VARIABLES

# extract relevant variables and rename
df1 <- data_pat %>%
  dplyr::select(record_id, age, sex, degree, degree_job, we01_01:we01_05, er_01_1:er_01_10)
df1 <- df1 %>%
  dplyr::rename(CASE = record_id, gender = sex, education = degree, job = degree_job,
                WHO1 = we01_01, WHO2 = we01_02,  WHO3 = we01_03,  WHO4 = we01_04, WHO5 = we01_05, 
                FLEXER1 = er_01_1, FLEXER2 = er_01_2, FLEXER3 = er_01_3, FLEXER4 = er_01_4, FLEXER5 = er_01_5,
                FLEXER6 = er_01_6, FLEXER7 = er_01_7, FLEXER8 = er_01_8, FLEXER9 = er_01_9, FLEXER10 = er_01_10)
                

df2 <- data_berndt %>%
  dplyr::select(CASE, Age, Sex, Education, Occupation, Flex1:Flex10, WHO1:WHO5)
df2 <- df2 %>%
  dplyr::rename(gender = Sex, age = Age, education = Education, job = Occupation,
                FLEXER1 = Flex1, FLEXER2 = Flex2, FLEXER3 = Flex3, FLEXER4 = Flex4, FLEXER5 = Flex5, 
                FLEXER6 = Flex6, FLEXER7 = Flex7, FLEXER8 = Flex8, FLEXER9 = Flex9, FLEXER10 = Flex10)
# recode FlexER Items

df2_r <- df2 %>%
  mutate(dplyr::across(starts_with("FLEXER"), ~ dplyr::recode(., `1` = 4, `2` = 3, `3` = 2, `4` = 1)))


df2_r$WHO1 <- df2_r$WHO1 - 1
df2_r$WHO2 <- df2_r$WHO2 - 1
df2_r$WHO3 <- df2_r$WHO3 - 1
df2_r$WHO4 <- df2_r$WHO4 - 1
df2_r$WHO5 <- df2_r$WHO5 - 1

df3 <- data_cad %>%
  dplyr::select(record_id, age, gender, edu, who5_1:who5_5, flexer_01:flexer_10)
df3 <- df3 %>%
  dplyr::rename(CASE = record_id, education = edu, 
                WHO1 = who5_1, WHO2 = who5_2,  WHO3 = who5_3,  WHO4 = who5_4, WHO5 = who5_5, 
                FLEXER1 = flexer_01, FLEXER2 = flexer_02, FLEXER3 = flexer_03, FLEXER4 = flexer_04, FLEXER5 = flexer_05,
                FLEXER6 = flexer_06, FLEXER7 = flexer_07, FLEXER8 = flexer_08, FLEXER9 = flexer_09, FLEXER10 = flexer_10)

# recode item 7 in study 3

df3_r <- df3 %>%
  mutate(FLEXER7 = dplyr::recode(FLEXER7, `1` = 4, `2` = 3, `3` = 2, `4` = 1))

df3_r$job <- NA

df3_r_s <- df3_r %>%
  group_by(CASE) %>%
  summarise(across(everything(), ~ na.omit(.)[1]))

df3_red <- df3_r_s[complete.cases(df3_r_s[,c("age","gender")]), ]


### COMBINE DATA SETS

df_all <- rbind(df1, df2_r, df3_red)

### REMOVE DATA SETS WITH NAs IN FLEXER SCALE

# keep only complete cases and store them in new DF named "df_complete"
df_complete <- df_all[complete.cases(df_all[,c("FLEXER1","FLEXER2","FLEXER3","FLEXER4","FLEXER5","FLEXER6","FLEXER7","FLEXER8","FLEXER9","FLEXER10")]), ]

# save df_complete to .csv
write.csv(df_complete, file = here::here("datasheets", "10item", "10item_wholeSample.csv"), row.names = TRUE)

#####

# PLEASE NOTE IF YOU WANT TO REPRODUCE RESULTS OF THE VALIDATION, PLEASE START AT THIS LINE

#####

df_complete <- read.csv(here::here("datasheets", "10item", "10item_wholeSample.csv"))

### RESPONSE PATTERNS
## EXTREME VALUES
# values from 1 to 4 possible

#  Function for checking whether there are only values of 1 or 4 in a line
check_values <- function(row) {
  all(row %in% c(1, 4))
}

df_complete %>%
  dplyr::select(FLEXER1:FLEXER10) %>%
  dplyr::mutate( Extreme = apply( ., 1, FUN = check_values)) %>%
  dplyr::select(Extreme) %>% 
  data.frame(df_complete, .) -> df_complete
  
## TENDENCY TO THE MIDDLE

# not applicable, because 4 point likert scale

## MAXIMUM LONGSTRING NIESSEN, MEIJER & TENDEIRO 2016

# function to compute length of the longest consecutive sections of identical elements in the vector x
maxRun <- function( x ) max( rle( x )$lengths )

df_complete %>%
  dplyr::select(FLEXER1:FLEXER10) %>%
  dplyr::mutate( MaxRun = apply(., 1, FUN = maxRun)) %>%
  dplyr::select(MaxRun) %>% 
  data.frame(df_complete, .) -> df_complete

# table(df_complete$MaxRun)
# 1   2   3   4   5   6   7   8   9  10 
# 7 164 194 138  68  31  29  13   7  29 


## PERSON STANDARD DEVIATION

df_complete %>% 
  dplyr::select(FLEXER1:FLEXER10) %>%
  dplyr::mutate( SD = apply(., 1, FUN = sd)) %>%
  dplyr::select(SD) %>% 
  data.frame(df_complete, .) -> df_complete

df_complete %>% dplyr::select( SD) %>% round(., 1 ) %>% table %>% plot

# 29 participants from MaxRun show of course also an SD of 0
# All in all, it is not possible to say whether 29 people ticked the boxes inattentively or whether they generally tend towards extreme or equal values.

### PARTICIPANTS

table(df_complete$gender)
mean(df_complete$age, na.rm = T); sd (df_complete$age, na.rm = T); median(df_complete$age, na.rm = T);range(df_complete$age, na.rm = T);table(df_complete$age)
hist(df_complete$age)
table(df_complete$education)



### DESCRIPTIVE STATITICS

# compute ERF mean score --> lower values reflect higher flexibility
df_complete$ERF_mean <- rowMeans(df_complete[c("FLEXER1", "FLEXER2", "FLEXER3", "FLEXER4", "FLEXER5", "FLEXER6", "FLEXER7", "FLEXER8", "FLEXER9", "FLEXER10")])

# recode mean score, because we want higher values to reflect higher flexibility

df_complete$ERF <- 5 - df_complete$ERF_mean

# descriptive statistics

df_complete %>%
  dplyr::select(CASE,FLEXER1:FLEXER10) %>%
    tidyr::pivot_longer(c(FLEXER1:FLEXER10), names_to = "key", values_to="value") -> erf_plot

ggplot(erf_plot, aes(key,value))+
  stat_boxplot(geom="errorbar",width=0.5)+
  geom_boxplot()+
  scale_x_discrete()+
  labs(x="Variablen der Skala", y="Werte", title="Boxplot")+
  geom_hline(yintercept=2.5 ,colour="red")+
  ylim(1,4)

# Item Characteristics

df_complete %>%
  dplyr::select(FLEXER1:FLEXER10,ERF) %>%
  psych::describe() %>%
  as.data.frame() %>%
  dplyr::select(c(n,mean,sd,median,min,max)) -> Item_characteristics

  # Normal Distribution
ggplot(df_complete, aes(sample=FLEXER1)) + labs(title="FLEXER 1") + stat_qq() -> g1
ggplot(df_complete, aes(sample=FLEXER2)) + labs(title="FLEXER 2") + stat_qq() -> g2
ggplot(df_complete, aes(sample=FLEXER3)) + labs(title="FLEXER 3") + stat_qq() -> g3
ggplot(df_complete, aes(sample=FLEXER4)) + labs(title="FLEXER 4") + stat_qq() -> g4
ggplot(df_complete, aes(sample=FLEXER5)) + labs(title="FLEXER 5") + stat_qq() -> g5
ggplot(df_complete, aes(sample=FLEXER6)) + labs(title="FLEXER 6") + stat_qq() -> g6
ggplot(df_complete, aes(sample=FLEXER7)) + labs(title="FLEXER 7") + stat_qq() -> g7
ggplot(df_complete, aes(sample=FLEXER8)) + labs(title="FLEXER 8") + stat_qq() -> g8
ggplot(df_complete, aes(sample=FLEXER9)) + labs(title="FLEXER 9") + stat_qq() -> g9
ggplot(df_complete, aes(sample=FLEXER10)) + labs(title="FLEXER 10") + stat_qq() -> g10
NVItems <- gridExtra::grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10)
rm(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10)

# Shaipiro test -> H0 Shapiro = empirical distribution equals normal distribution
df_complete %>%
  dplyr::select(FLEXER1:FLEXER10,ERF) %>%
  apply(2, shapiro.test) -> ItemShapiro
sapply(ItemShapiro, function(x) x[1]) %>% as.numeric -> Sh.Statistik
sapply(ItemShapiro, function(x) x[2]) %>% as.numeric -> Sh.pWert
data.frame(Item_characteristics, Sh.Statistik,Sh.pWert) -> Item_characteristics

### ITEM DIFFICULTY

source(here("scripts", "difficulty_dahl.R"))

Difficulty <- data.frame(item_difficulty(df_complete[c("FLEXER1", "FLEXER2", "FLEXER3", "FLEXER4", "FLEXER5", "FLEXER6", "FLEXER7", "FLEXER8", "FLEXER9", "FLEXER10","ERF")]))

Item_characteristics <- cbind(Item_characteristics, Difficulty[,2:3])

# The term "difficulty" is difficult to apply in terms of content because of disposition
# Difficulty: 0 (= difficult) to 100 (= easy)
# Difficulty < 20 Basement effects (difficulty is so great, subjects with high expression of the characteristic have high test scores)
# Difficulty > 80 Ceiling effects

# Table for Item characteristics

Item_characteristics %>%
  dplyr::select(n,mean,sd,min,max,median,Sh.Statistik,Sh.pWert,Difficulty,Ideal) %>%
  mutate(mean = round(mean,2),
         sd = round(sd,2),
         Sh.Statistik= round(Sh.Statistik,2),
         Sh.pWert = round(Sh.pWert,2),
         max = round(max,2),
         median = round(median,2)) %>%
  rename (
          M = mean,
          SD = sd,
          Min. = min,
          Max. = max,
          Median = median,
          "S-W-Test" = Sh.Statistik,
          "S-W-pWert" = Sh.pWert) -> Item_characteristics

erf_plot %>%
  mutate ( Variable = dplyr::recode( key,
                                     FLEXER1 = "FLEXER1 Wenn mich meine Emotionen \nbei der erfolgreichen Erledigung einer Aufgabe \n hindern, habe ich Strategien, um meine \nGefühle zu beeinflussen",
                                     FLEXER2 = "FLEXER2 Wenn es einem Ziel nützt, \nverringere ich in manchen Situationen auch positive Gefühle",
                                     FLEXER3 = "FLEXER3 Wenn ich meine positiven Gefühle reduzieren muss, \nhabe ich mehrere Strategien \ndies zu erreichen",
                                     FLEXER4 = "FLEXER4 Wenn ich mit einer Strategie nicht \nerfolgreich meine Stimmung verändern kann, \nprobiere ich eine andere Strategie aus",
                                     FLEXER5 = "FLEXER5 Wenn ich weniger negative Gefühle\n empfinden möchte, habe ich mehrere Methoden, dies zu erreichen",
                                     FLEXER6 = "FLEXER6 Wenn ich mehr positive Gefühle empfinden möchte, \nhabe ich mehrere Möglichkeiten, dies zu erreichen",
                                     FLEXER7 = "FLEXER7 Ich habe die Wahl zwischen \nverschiedenen Strategien, um mit meinen Gefühlen umzugehen",
                                     FLEXER8 = "FLEXER8 Wenn notwendig, verstärke ich auch ein \nnegatives Gefühl, um erfolgreich zu sein",
                                     FLEXER9 = "FLEXER9 Wenn ich mich in eine negative Stimmung \nversetzen möchte, habe ich Strategien, dies zu erreichen",
                                     FLEXER10 = "FLEXER10 Wenn ich meine Gefühle verändern möchte, \nweiß ich, dass es dafür verschiedene Strategien gibt")) %>%
  mutate( Werte = factor(value, labels = c( "1 sehr zutreffend", "2 zutreffend","3 ein wenig zutreffend", "4 nicht zutreffend")))-> erf_plot

ggplot(erf_plot, aes(x= Variable, group=Werte, fill=Werte)) + 
  scale_fill_grey() +
  geom_bar(stat = "count", position = "stack" ) +
  labs( y="Anzahl Personen") +
  coord_flip()

### ITEM AND SCALE STATISTICS

# Item homogenity
# Heat plot
df_complete %>%
  dplyr::select(FLEXER1:FLEXER10) %>%
  cor( use = "pair" ) %>%
  reshape2::melt() %>%
  ggplot(data = ., aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       #scale_fill_gradient2(low = "white", high = "black", mid = "grey50",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  labs( x= "FlexER Scale", y="FlexER Scale") +
  theme_bw()

# correlation between items
df_complete %>%
  dplyr::select(FLEXER1:FLEXER10) %>%
  cor(use="pair") %>%
  round(3) -> Korr
Korr

# Count how many correlations are below or above certain threshold values 
sum(abs(Korr)< .3)/2 #18 below .3
(sum(abs(Korr)>= .7)-nrow(Korr))/2 #0 over .7
sum( Korr<0)/2 #0 below 0

# Discriminatory power
# The discriminatory power (r it) is the correlation of individual items with the sum score of the items that capture a latent dimension
# Item whole correlation for this item against the scale without this item -> Part-Wohle-Korrelation
r_it <- psych::alpha(df_complete[c("FLEXER1","FLEXER2","FLEXER3","FLEXER4","FLEXER5","FLEXER6","FLEXER7","FLEXER8","FLEXER9","FLEXER10","ERF")] ,check.keys = FALSE, warnings = FALSE )
r_it <- r_it[["item.stats"]][["r.drop"]]
Item_characteristics <- cbind(Item_characteristics,r_it)
# Discriminatory power should be greater than .25 or .30. Lower discriminatory power should only be accepted if the difficulty is extreme. A prerequisite for a meaningful calculation is that the dimensionality of the test has been checked or determined beforehand. The discriminatory power depends on item spread and item difficulty The more extreme the difficulty deviates from the average range, the lower the discriminatory power --> The highest discriminatory power should be expected for items of average difficulty.

# Internal consistency 
alpha <- psych::alpha(subset(df_complete, select = c(FLEXER1:FLEXER10)))
Item_characteristics$alpha <- alpha$total[["raw_alpha"]]
rm(Korr)

Item_characteristics %>%
  dplyr::mutate(r_it = round(r_it, 3)) -> Item_characteristics

Characteristics <- sjPlot::tab_itemscale(df_complete[c("FLEXER1", "FLEXER2", "FLEXER3", "FLEXER4", "FLEXER5", "FLEXER6", "FLEXER7", "FLEXER8", "FLEXER9", "FLEXER10")])

### EXPLORATORY FACTOR ANALYSIS - PREREQ

# Testing the Multivariate Normal Distribution, however, it becomes unsurprising. Possible test:
# Mardia ("mardia") is based on skewness and kurtosis, but there is research that the test does not have such significant power
# Royston ("royston") is based on the Shapiro Wilking test and is good for small samples (cannot be used for n>5000)
# Henze-Zirkler ("hz") generally good, with moderate power at n > 75
# Dornik-Haansen ("dh") is reasonably good, but has probably not yet been evaluated in sufficient detail It can be supplemented by the ("energy") energy test (you get the E-statistic)
# Procedure is to be tested with several methods incl. graphics (Farrell et al 2007)
# Each of the tests has the same assumption: H0: Data is MVN and H1: Data is not MVN
# "adj" = adjusted quantile methodbased on Mahalanobis distance
# Depends on distance between cdf of Chi-Square and ecdf (empirical cumulative distribution function)of samples in tails
# You can also output the whole thing as a "box" (boxplot), "qq"/"qqPlot" and do a test: 
# "SW" = ShapiroWilking; "CVM" = Cramers-von Mises, "Lillie" = Lilliefors, "SF" = Shapiro-Francia, "AD"= Anderson-Darling

MVN::mvn(df_complete[,11:20],
         mvnTest=c("hz"),
         multivariatePlot="qq",
         #multivariateOutlierMethod="adj",
         univariatePlot="histogram",
         univariateTest="SW")
# qqplot gives error

# Now we get to know two more methods that check the prerequisites for the use of an EFA:
# Bartlett test for sphericity
# Kaiser-Meyer-Olkin (KMO) to check the data set for suitability for EFA

# Bartlett's test checks the null hypothesis that the correlation matrix is an identity matrix


df_complete %>%
  dplyr::select(FLEXER1:FLEXER10) %>%
  cor( use = "pairwise.complete.obs" ) -> Items.cor
anz <- NROW( df_complete )
cortest.bartlett( Items.cor, n = anz )

#H0 can be rejected because X²(45,N=680) = 2850.16, p<.001
# The null hypothesis of the Bartlett test is the so-called identity matrix (unit matrix), a square matrix with 1.0 on the diagonal and 0.0 in the lower and upper triangular matrix. As H0, the matrix has the same dimension as our correlation matrix; in terms of content, it expresses the fact that there is no correlation between the items, and our result shows that the data provide a good basis for an EFA. If P>.05, we could have concluded (a) that the correlations are too low overall or (b) that the n is insufficient.

# The KMO coefficients provide an indication of which variables are suitable for an EFA and which are not. This assessment can be derived from the following evaluation of the coefficients 
#( Measure of Sampling Adequacy) = MSA  
# < .50 - barely acceptable
# .50 - .70 - average
# .70 - .80 - good
# .80 - .90 - very good
# > .90 - excellent.

kmo <- KMO(Items.cor)
MSAi <- unlist(kmo$MSAi)
cut(MSAi, c(0, .5, .7, .8, .9, 1.0)) %>% table
kmo$MSA

# all items are above .70 -> at least good
# Overall MSA = 0.87 which is very good

### EXPLORATORY FACTOR ANALYSIS

# subset of relevant variables
df_complete %>% dplyr::select( FLEXER1:FLEXER10 ) -> efa
# parallel analyses for Factor analysis and PCA
psych::fa.parallel(efa, fm="mle", fa="both", main = "Parallel Analysis")$fa.values # Prarallel analyses suggests 2 factors 

# Further analyses...  MAP and VSS 
psych::nfactors(efa, rotate="oblimin", fm="mle") 
# MAP 2
# VSS 2
# BIC 3

# Number of factors PCA!
ev <- eigen(cor(efa))
ev
# Principle Components!
ap <- parallel(subject=nrow(efa), var=ncol(efa), rep=100,cent=.05, model ="components")
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)

plotnScree(nS) # 2 in PCA

# Method: Agreement Approach
fac <- parameters::n_factors(efa, type = "FA", algorithm = "mle", package = "all")
print(fac)
plot(fac)
fac$Method # 09 of 27 tests support choice of 2 dimensions 


# EFA, Inspection of 1 solution 
fa1 <- psych::fa(efa, fm="ml", nfactors=1, rotate = "oblimin") # further option is to change estimate and to change n.iter if bootstrapping for CI is requested
psych::print.psych( fa1, cut=0.05,sort = TRUE )
summary(fa1$communality) # Commonality
summary(fa1$complexity) # index of complexity
fa1$RMSEA # fa1$RMSEA[1] or Index
fa1$fit.off # Fit based upon off-diagonal values

# EFA, Inspection of 2 solution 
fa2 <- psych::fa( efa, fm="ml", nfactors=2, rotate = "oblimin" )
psych::print.psych( fa2, sort = TRUE, digits = 3)
summary(fa2$communality) # Commonality
summary(fa2$complexity) # index of complexity
fa2$RMSEA # fa1$RMSEA[1] or Index
fa2$fit.off # Fit based upon off-diagonal values

## grafical illustration of results

attributes(fa2$loadings)$dimnames[[2]] <- c("?","?")
psych::fa.diagram(fa2,cut=.10)

##############

# compute WHO-5 Score (sum of all items multiplied by 4)

psych::alpha(subset(df_complete, select = c(WHO1:WHO5)))

df_complete$WHO <- rowSums(df_complete[c("WHO1", "WHO2", "WHO3", "WHO4", "WHO5")]) * 4

cor_ERF_WHO <- cor(df_complete$WHO, df_complete$ERF, method = "spearman", use = "complete.obs")

cor.test(df_complete$WHO, df_complete$ERF, method = "spearman")

ggplot(data = df_complete, mapping = aes(x = ERF, y = WHO)) +
  geom_jitter(color = "gray", size = 3, width = 0.2, height = 0) +
  geom_smooth(method = "lm", se=FALSE, color = "darkred", linewidth = 2) +
  ggprism::theme_prism(base_size = 20, base_line_size = 0.5, base_fontface = "plain", base_family = "sans") +
  theme(
    legend.title = element_text(),
    axis.title = element_text(size = 22)
  ) +
  xlab("FlexER Score") +
  ylab("WHO-5 Score") +
  annotate("rect", xmin = 0.4, xmax = 1.06, ymin = 58 , ymax = 62, 
           alpha = 1, fill = "white", color = "white") + 
  annotate("text", x = 3.8, y = 57, 
         label = bquote(paste(rho) == .(round(cor_ERF_WHO, 3))), 
         size = 7, color = "black",
         hjust = 0)  
############

# save workspace image

save.image(file = "Workspace_FlexER.RData")



