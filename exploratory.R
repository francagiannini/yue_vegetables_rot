library(tidyverse)
library(plotly)
library(sf)
library(tmap)

#default theme for ggplot objects to theme_bw()
theme_set(ggthemes::theme_calc())
theme_update()


# Map locations ----
#veg_sites <- st_read("C:/Users/au710823/OneDrive - Aarhus universitet/Grøntsagsforsøg/vegetables_yue/sites_veg_repro.gpkg")

veg_sites <- readxl::read_excel(
  "C:/Users/au710823/OneDrive - Aarhus universitet/Grøntsagsforsøg/vegetables_yue/general_information_inputs.xlsx", 
 sheet = "sites",
 skip = 12)

veg_sites_sf <- st_as_sf(veg_sites,coords = c('Longitude','Latitude'), crs=4258)

tmap_mode("view")

field_color <-  c(Fyn="#E7B800", Brande="#FC4E07", Auning="#11467b")

tm_shape(veg_sites_sf)+
  tm_dots(size = 1, alpha=0.8, #clustering=TRUE, 
          col='Site',
          palette=field_color)#+
  #tm_text(text = "Field") 


# Data reading with observations ----

compl <- readxl::read_xlsx(
  "C://Users/au710823//OneDrive - Aarhus universitet//Grøntsagsforsøg//Data//data of water samples with total N.xlsx",
  sheet = "suggested samples from Food"
) |>
  filter(!is.na(as.numeric(`Nitrat  mg/L`))) |>
  tbl_df() %>%
  mutate(newar = ymd(paste0(`År`, "-01-01"))) |>
  #mutate(aruge=paste(print(`År`),ifelse(Uge>10,print(Uge),print(paste("0",Uge,sep=""))),sep="")) |>
  mutate(
    date_week = newar + weeks(Uge),
    measure = ifelse(
      `types of samples` %in% c(
        'P1','P10','P11','P12','P13','P14',
        'P15','P16','P2', 'P3','P5','P6', 'P7','P8','P9'
      ),
      "P",
      "SC"
    ),
    managment = recode_factor(Område, 
                              "1"='Conventional',"2"='Conventional',
                              "3"='Organic',"4"='Organic',
                              "62"='Conventional',
                              "555"='Organic', "624"='Organic', 
                              "709"='Organic', "710"='Organic', 
                              "806"='Conventional', "813"='Conventional'),
    farmer = recode_factor(Område, 
                              "1"='AU',"2"='AU',
                              "3"='AU',"4"='AU',
                              "62"='M',"555"='M', "624"='M', 
                              "709"='T', "710"='T', 
                              "806"='T', "813"='T'),
    Område=as.factor(Område)
  ) |>
  filter(measure == "SC")

summary(compl$date_week)

# Observations per field plot ----
table(compl$Sted,compl$Område) |> as.data.frame() |>
  mutate(Freq=ifelse(Freq==0,NA, Freq)) |> 
  # mutate_all(~ na_if( .,0)) |> 
  ggplot( aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "gray") +
  scale_fill_gradientn(name = "n",
                       na.value = 'grey0',
                       colors = RColorBrewer::brewer.pal(7, "BrBG")
  ) +
  geom_text(aes(label = paste(Freq)), color = "grey0", size = 4) +
  scale_x_discrete(name = "Field") +
  scale_y_discrete(name = "Location")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Observations per field per date ----

table(compl$Sted,compl$date_week) |> as.data.frame() |>
  mutate(Freq=ifelse(Freq==0,NA, Freq)) |> 
  # mutate_all(~ na_if( .,0)) |> 
  ggplot( aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "gray") +
  scale_fill_gradientn(name = "n",
                       na.value = 'grey0',
                       colors = RColorBrewer::brewer.pal(7, "YlGnBu")
  ) +
  geom_text(aes(label = paste(Freq)), color = "grey0", size =2) +
  scale_x_discrete(name = "date") +
  scale_y_discrete(name = "Location")+ theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")

# Temporal distribution observed data
#expl_gral <- 
compl |> 
  filter(Sted %in% c('Auning','Brande')) |> 
  ggplot(aes(
  x = date_week,
  y = `Nitrat  mg/L`,
  group= Område,
  shape = Område,
  col = Sted,
  #linetype=managment
)) +
  geom_smooth(alpha=0.2)+
  geom_point() +
  #stat_summary(fun.y=mean, geom="line")+
  scale_color_manual(values=field_color)+
  scale_shape_manual(values=seq(0,11))+
  scale_x_date(date_labels= "%y  %W", date_breaks  ="2 week", 
               limits= c(min(compl$date_week), max(compl$date_week)))+
  theme(axis.text.x = element_text(angle = 90))
  
ggplotly(expl_gral)

# expl_gral_box <- 
#   compl |> ggplot(aes(
#     x = as.factor(date_week),
#     y = `Nitrat  mg/L`,
#     col=Område#,
#     #col = Sted,
#     #linetype=managment
#   )) +
#   geom_boxplot()+
#   #geom_smooth(alpha=0.2)+
#   #geom_point() +
#   
#   stat_summary(fun.y=mean, geom="line")+
#   scale_color_manual(values=field_color)+
#   scale_x_date(date_labels= "%y  %W", date_breaks  ="2 week", 
#                limits= c(min(compl$date_week), max(compl$date_week)))+
#   theme(axis.text.x = element_text(angle = 90),
#         legend.position = "bottom")

#ggplotly(expl_gral)


compl |> 
  group_by(Område, date_week,Sted,managment) |> 
  summarise(Nave=mean(`Nitrat  mg/L`),
            sd=sd(`Nitrat  mg/L`),
            min=min(`Nitrat  mg/L`),
            max=max(`Nitrat  mg/L`))|> 
  filter(Sted %in% c('Auning','Fyn')) |>
  ggplot(aes(x=date_week, y=Nave, ymax=max, ymin=min,
             group=Område,
             col = Sted,
             shape=Område,
             #linetype=managment
             )) +
  geom_pointrange()+
  stat_summary(fun.y=mean, geom="line")+
  scale_shape_manual(values = c(4,8,15,16,17,18,21,22,3,42))+
  scale_color_manual(values=field_color) +
  scale_x_date(date_labels= "%y  %W", date_breaks  ="2 week",
                 limits= c(min(compl$date_week), max(compl$date_week))) +
  theme(axis.text.x = element_text(angle = 90),
          legend.position = "bottom")+
  scale_y_continuous(name="Nitrat  mg/L")


