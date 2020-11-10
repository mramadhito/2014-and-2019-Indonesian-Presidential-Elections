##Final Project

library(tidyverse)
library(ggplot2)
library(rpart)
library(rattle)
library(openintro)
library(tidytext)
library(randomForest)
library(caret)


#Read data
Indo.data1 <- read_csv("IndoData1.csv")

#Add population weights to absolute number indicators
Indo.data2 <- Indo.data1 %>% 
  mutate(stkchange.weighted = NE.GDI.STKB.SNA08.CR/Population,
         exportsexpend.weighted = NE.GDI.EXPT.SNA08.CR/Population,
         govtconsump.weighted = NE.GDI.CON.GOVT.SNA08.CR/Population,
         grossfixed.weighted = NE.GDI.FTOT.SNA08.CR/Population,
         importsexpend.weighted = NE.GDI.IMPT.SNA08.CR/Population,
         interregionexp.weighted = NE.GDI.INEX.SNA08.CR/Population,
         nonprofitconsump.weighted = NE.GDI.CON.NPI.SNA08.CR/Population,
         privateconsump.weighted = NE.GDI.CON.PRVT.SNA08.CR/Population,
         accomfb.weighted = NA.GDP.ACC.FB.SNA08.CR/Population,
         agriforestfish.weighted = NA.GDP.AGR.SNA08.CR/Population,
         businesssector.weighted = NA.GDP.BUSS.SNA08.CR/Population,
         construction.weighted = NA.GDP.CNST.SNA08.CR/Population,
         eduservices.weighted = NA.GDP.EDUS.SNA08.CR/Population,
         elecgas.weighted = NA.GDP.ELEC.GAS.SNA08.CR/Population,
         finance.weighted = NA.GDP.FINS.SNA08.CR/Population,
         healthsocwork.weighted = NA.GDP.HLTH.SOCW.SNA08.CR/Population,
         infocomm.weighted = NA.GDP.INF.COMM.SNA08.CR/Population,
         manufacturing.weighted = NA.GDP.MNF.SNA08.CR/Population,
         miningquarry.weighted = NA.GDP.MINQ.SNA08.CR/Population,
         otherservice.weighted = NA.GDP.SRV.OTHR.SNA08.CR/Population,
         publicdefsocsec.weighted = NA.GDP.PADM.DEF.SNA08.CR/Population,
         realestate.weighted = NA.GDP.REST.SNA08.CR/Population,
         transtor.weighted = NA.GDP.TRAN.STOR.SNA08.CR/Population,
         waterwaste.weighted = NA.GDP.WTR.WST.SNA08.CR/Population,
         wholesalerepair.weighted = NA.GDP.TRD.SNA08.CR/Population,
         laborforce.rate = 100*(SL.TLF/Population),
         GDP.weighted = NE.GDI.TOTL.SNA08.CR/Population) %>% 
  select(-NE.GDI.STKB.SNA08.CR,
         -NE.GDI.EXPT.SNA08.CR,
         -NE.GDI.CON.GOVT.SNA08.CR,
         -NE.GDI.FTOT.SNA08.CR,
         -NE.GDI.IMPT.SNA08.CR,
         -NE.GDI.INEX.SNA08.CR,
         -NE.GDI.CON.NPI.SNA08.CR,
         -NE.GDI.CON.PRVT.SNA08.CR,
         -NA.GDP.ACC.FB.SNA08.CR,
         -NA.GDP.AGR.SNA08.CR,
         -NA.GDP.BUSS.SNA08.CR,
         -NA.GDP.CNST.SNA08.CR,
         -NA.GDP.EDUS.SNA08.CR,
         -NA.GDP.ELEC.GAS.SNA08.CR,
         -NA.GDP.FINS.SNA08.CR,
         -NA.GDP.HLTH.SOCW.SNA08.CR,
         -NA.GDP.REST.SNA08.CR,
         -NA.GDP.INF.COMM.SNA08.CR,
         -NA.GDP.MNF.SNA08.CR,
         -NA.GDP.MINQ.SNA08.CR,
         -NA.GDP.SRV.OTHR.SNA08.CR,
         -NA.GDP.PADM.DEF.SNA08.CR,
         -NA.GDP.REST.SNA08.CR,
         -NA.GDP.TRAN.STOR.SNA08.CR,
         -NA.GDP.WTR.WST.SNA08.CR,
         -NA.GDP.TRD.SNA08.CR,
         -SL.EMP.AGR.FRST.FSH,
         -SL.EMP.CNST,
         -SL.EMP.ELC,
         -SL.EMP.FINS,
         -SL.EMP.IND,
         -SL.EMP.MINQ,
         -SL.EMP.SOCL,
         -SL.EMP.TRAD,
         -SL.EMP.TRAN,
         -SL.TLF,
         -SI.POV.BPL,
         -Prabowo.Vote)

Indo.data3 <- Indo.data2 %>% 
  select(-Provinces.Code,
         -Provinces.Name,
         -Year)

Indo.data4 <- Indo.data3 %>% 
  mutate(jokowi.wins = ifelse(Jokowi.Vote >= 50, 1, 0)) %>% 
  select(-Jokowi.Vote)

#Forest
jokowiforest1 <- randomForest(Jokowi.Vote ~ .,
                              data = Indo.data3,
                              na.action = na.exclude)
varImpPlot(jokowiforest1)

#Tree

jokowitree1 <- rpart(factor(jokowi.wins) ~ .,
                     data = Indo.data4)

fancyRpartPlot(jokowitree1)


#Linear models (difference-in-difference)

jokowi.linear1 <- lm(Jokowi.Vote ~ laborforce.rate,
                    data = Indo.data2)

summary(jokowi.linear1)

jokowi.linear2 <- lm(Jokowi.Vote ~ laborforce.rate + IDX.HDI.REV,
                     data = Indo.data2)

summary(jokowi.linear2)

jokowi.linear3 <- lm(Jokowi.Vote ~ laborforce.rate + IDX.HDI.REV + accomfb.weighted + eduservices.weighted,
                     data = Indo.data2)

summary(jokowi.linear3)

jokowi.linear4 <- lm(Jokowi.Vote ~ accomfb.weighted,
                     data = Indo.data2)

summary(jokowi.linear4)

#K-means clustering

laborforce.subset <- Indo.data2 %>% 
  select(Jokowi.Vote,
         laborforce.rate,
         IDX.HDI.REV,
         accomfb.weighted,
         eduservices.weighted)

jokowi.kmeans <- kmeans(na.omit(laborforce.subset), 2)

na.omit(laborforce.subset) %>% 
  mutate(jokowi.cluster = jokowi.kmeans$cluster) %>% 
  ggplot(aes(x = laborforce.rate,
             y = Jokowi.Vote)) + 
  geom_point(aes(color = factor(jokowi.cluster))) +
  geom_smooth(method = "lm", se = FALSE)
  

na.omit(laborforce.subset) %>% 
  mutate(jokowi.cluster = jokowi.kmeans$cluster) %>% 
  ggplot(aes(x = IDX.HDI.REV,
             y = Jokowi.Vote)) + 
  geom_point(aes(color = factor(jokowi.cluster))) +
  geom_smooth(method = "lm", se = FALSE) 

na.omit(laborforce.subset) %>% 
  mutate(jokowi.cluster = jokowi.kmeans$cluster) %>% 
  ggplot(aes(x = accomfb.weighted,
             y = Jokowi.Vote)) + 
  geom_point(aes(color = factor(jokowi.cluster))) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text_repel(data = Indo.data2,
                  mapping = aes(label = Provinces.Code))


#Exploratory graphs

#Accommodaton and F&B
Indo.data2 %>%
  subset(Year == 2013) %>% 
  ggplot(aes(x = reorder(Provinces.Name, accomfb.weighted),
             y = accomfb.weighted)) +
  geom_bar(stat = "identity",
           fill = "blue") +
  xlab("Province") +
  ylab("Value (IDR millions), Current Prices") +
  coord_flip() +
  ggtitle("GDP on Accommodation and F&B Activity per capita") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom", 
        legend.title = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

#Jokowi results
Indo.data2 %>%
  ggplot(aes(x = reorder(Provinces.Name, Jokowi.Vote),
             y = Jokowi.Vote,
             fill = factor(Year))) +
  geom_bar(stat = "identity",
           position = "dodge") +
  xlab("Province") +
  ylab("Vote Share (%)") +
  coord_flip() +
  ggtitle("Jokowi's Vote Share, by province") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom", 
        legend.title = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

#Prabowo results
Indo.data2 %>%
  mutate(Prabowo.Vote = 100 - Jokowi.Vote) %>% 
    ggplot(aes(x = reorder(Provinces.Name, Prabowo.Vote),
               y = Prabowo.Vote,
               fill = factor(Year))) +
    geom_bar(stat = "identity",
             position = "dodge") +
    xlab("Province") +
    ylab("Vote Share (%)") +
    coord_flip()+
  ggtitle("Prabowo's Vote Share, by province") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5), 
          legend.position = "bottom", 
          legend.title = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))

  
