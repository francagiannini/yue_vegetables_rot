
library(tidyverse)

#default theme for ggplot objects to theme_bw()
theme_set(theme_bw())
theme_update(panel.grid = element_blank())


compl<- readxl::read_xlsx(
    "C:\\Users\\au710823\\OneDrive - Aarhus universitet\\Grøntsagsforsøg\\veg_conc_061123.xlsx",
  sheet = "Sheet2") |> filter(!is.na(`Nitrat  mg/L`)) |> 
  mutate(aruge=paste(print(`År`),ifelse(Uge>10,print(Uge),print(paste("0",Uge,sep=""))),sep="")) |> 
  mutate(date=as.Date(aruge,"%Y%W"))

summary(compl$aruge)

compl |> ggplot(aes(
  x = date,
  y = `Nitrat  mg/L`,
  col = Sted #interaction(Sted, Område)
)) +
  geom_point() +
  geom_smooth()
