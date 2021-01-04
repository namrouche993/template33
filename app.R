#setwd("C:/Users/nadji/amrouche2/electron-quick-start/ElectronShinyAppWindows/electron-quick-start-win32-ia32/resources/app")
#getwd()

#.libPaths(paste0(getwd(),"/R-Portable-Win/library"))
#.Library=paste0(getwd(),"/R-Portable-Win/library")

library(shiny)
library(fresh)
#library(shinydashboard)
#library(shinydashboardPlus)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
#library(ECharts2Shiny)
#library(rAmCharts)
library(shinyBS)

library(leaflet)
#library(htmltools)
library(leaflet.extras)


library(rgdal)
library(sp)
library(readxl)
library(highcharter)
library(tidyverse)
library(excelR)
library(farver)
library(readxl)


library(reactable)
#library(grDevices)
#library(janitor)

library(shinyjs)


livraison_wilayas <- read_excel(paste0(getwd(),"/livraison_wilayas.xlsx"))
estimation_tolpopparc <- read_excel(paste0(getwd(),"/Estimation_Population_TOL_Parc_par_Wilaya.xlsx"))

estimation_tolpopparc[,3:5]=round(estimation_tolpopparc[,3:5])
sit_fin <- read_excel(paste0(getwd(),"/sitfin.xlsx"))
zones <- read_excel(paste0(getwd(),"/zones2.xlsx"))
zones00=zones
zones=zones[1:48,]
sitphy <- read_excel(paste0(getwd(),"/sitphy.xlsx"))
sitphy$`Type de logements`[which(sitphy$`Type de logements`=="RURAL")]=c("Rural")

# for(i in 1:nrow(zones)){
#   for(j in 1:ncol(zones)){
#     zones[i,j]=print(zones[i,j])
#   }
# }
# 
# myspread <- function(df, key, value) {
#   # quote key
#   keyq <- rlang::enquo(key)
#   # break value vector into quotes
#   valueq <- rlang::enquo(value)
#   s <- rlang::quos(!!valueq)
#   df %>% gather(variable, value, !!!s) %>%
#     unite(temp, !!keyq, variable) %>%
#     spread(temp, value)
# }

Statistiques_des_projets_par_secteurs <- read_excel(paste0(getwd(),"/sitpro.xlsx"))
equip=Statistiques_des_projets_par_secteurs[,-1]
colnames(equip)[4]="En Cours"
colnames(equip)[2]="Nbre de Projets"
colnames(equip)[3]="Acheves"
colnames(equip)[5]="Non Lances"



equip0 <- read_excel(paste0(getwd(),"/equip0.xlsx"))
colnames(equip0)[4]="En Cours"
colnames(equip0)[2]="Nbre de Projets"
colnames(equip0)[3]="Acheves"
colnames(equip0)[5]="Non Lances"




ab=equip0[,6:ncol(equip0)]
a=1:40
ai=rep(c(3,0,-2,-1),10)
ai2=a+ai
ab0=ab %>% select(ai2)
colnames(ab0)=paste0(c("Nbre de Projets","Acheves","En Cours","Non Lances")," ",1:40)

equip000=cbind(equip0[,1:5],ab0)

equip11=equip[,c(1,6,2,3,4,5)]
data_equip = equip %>%  group_by(Wilaya) %>% summarise("Nbre de Projets"=sum(`Nbre de Projets`),Acheves=sum(Acheves),"En Cours"=sum(`En Cours`),"Non Lances"=sum(`Non Lances`))


pos <- read_excel("pos.xlsx")
pos$`Non Lancées`[which(pos$URBANISME=='PDAU')]=rep(NA,48)

pos2=pos %>% 
  mutate(Wilaya=rep(unique(livraison_wilayas$waw),3)) %>% 
  select(Wilaya,URBANISME,Achevées,`En Cours`,`Non Lancées`)

pos3=matrix(0,ncol=5,nrow=147)
pos3=data.frame(pos3)
colnames(pos3)=colnames(pos2)
pos3=pos2[1:48,]
pos3[49,1]="Total"
pos3[49,2]="POS"
pos3[49,3]=6539
pos3[49,4]=372
pos3[49,5]=9

pos3[50:98,]=pos2[49:97,]
pos3[98,1]="TOTAL"
pos3[98,2]="PDAU"
pos3[98,3]=1508
pos3[98,4]=33
pos3[98,5]=NA

pos3[99:146,]=pos2[97:144,]
pos3[147,1]="TOTAL"
pos3[147,2]="EGU"
pos3[147,3]=1424
pos3[147,4]=251
pos3[147,5]=33
# 
pos5=pos3
pos5=pos3[1:49,]
pos5[,6:8]=pos3[50:98,3:5]
pos5[,9:11]=pos3[99:147,3:5]
pos5=pos5[,-c(2,8)]

s1=sit_fin[1:48,2:5]
s1[,5:7]=sit_fin[49:96,3:5]
s1[,8:10]=sit_fin[97:144,3:5]
s1[,11:13]=sit_fin[145:192,3:5]
s1[49,1]="Total"
for(i in 2:ncol(s1)){
  s1[49,i]=sum(s1[1:48,i])
}

sit_fin1=sit_fin[,c(2,3,6)] %>% spread(key = Type,value = NOTIFICATION)
sit_fin2=sit_fin[,c(2,4,6)] %>% spread(key = Type,value = INSCRIPTION)
sit_fin3=sit_fin[,c(2,5,6)] %>% spread(key = Type,value = Reliquat)

ss=data.frame(sit_fin1[,1:2],sit_fin2[,2],sit_fin3[,2]
              , sit_fin1[,3],sit_fin2[,3],sit_fin3[,3],
              sit_fin1[,4],sit_fin2[,4],sit_fin3[,4],
              sit_fin1[,5],sit_fin2[,5],sit_fin3[,5])
ss1=ss %>% select(1,2,5,8,11,3,6,9,12,4,7,10,13)


for(i in 1:ncol(sitphy)){
  for(j in 1:nrow(sitphy)){
    if(is.na(sitphy[j,i])==TRUE){sitphy[j,i]=0}  
  }
}
green_pal <- function(x) rgb(colorRamp(c("#d1e8d1", "#198c19"))(x), maxColorValue = 255)
blue_pal <- function(x) rgb(colorRamp(c("#d1d1ff", "#1919ff"))(x), maxColorValue = 255)
red_pal <- function(x) rgb(colorRamp(c("#ffcccc", "#ff0000"))(x), maxColorValue = 255)



sitphy0=sitphy %>% 
  group_by(Wilaya_matricule) %>% 
  summarise(Livraison=sum(Livraison),Prevision=sum(Prevision),Consistance=sum(Consistance),Achevés=sum(Achevés),"En Cours"=sum(`En Cours`),"Non Lancés"=sum(`Non lancés`)) %>% 
  rename(Wilaya=Wilaya_matricule) %>% rowwise() %>% 
  #mutate(Consistance=format(Consistance2,big.mark = " ",trim=TRUE,digits = 3)) %>% 
  #mutate("Achevés"=sprintf("%1.0f%%", 100*sum(Achevés)/sum(Consistance2))) %>% 
  #mutate("En Cours"=sprintf("%1.0f%%", 100*sum(`En Cours`)/sum(Consistance2))) %>% 
  #mutate("Non Lancés"=sprintf("%1.0f%%", 100*sum(`Non Lancés`)/sum(Consistance2))) %>% 
  mutate("Achevés"=sum(Achevés)/sum(Consistance)) %>% 
  mutate("En Cours"=sum(`En Cours`)/sum(Consistance)) %>% 
  mutate(`Non Lancés`=sum(`Non Lancés`)/sum(Consistance)) %>% 
  mutate(a=as.numeric(str_sub(`Achevés`,-3,-2))) %>% 
  select(Wilaya,Consistance,`Achevés`,`En Cours`,`Non Lancés`)

sitphy2=sitphy
sitphy2=add_column(sitphy,"Acheves%"=round(100*sitphy$Achevés/sitphy$Consistance,1),.after=5)
sitphy2=add_column(sitphy2,"En Cours%"=round(100*sitphy$`En Cours`/sitphy$Consistance,1),.after=7)
sitphy2=add_column(sitphy2,"Non Lancés%"=round(100*sitphy$`Non lancés`/sitphy$Consistance,1),.after=9)
colnames(sitphy2)[3]="Segment"

# 
# da0sit=cbind(sitphy2[which(sitphy2$`Type de logements`=="LPL"),c(2,4,5,6,7,8,9,10,11,12)],sitphy2[which(sitphy2$`Type de logements`=="Rural"),4:12],sitphy2[which(sitphy2$`Type de logements`=="LSP"),4:12],sitphy2[which(sitphy2$`Type de logements`=="Location-Vente"),4:12],sitphy2[which(sitphy2$`Type de logements`=="LPP"),4:12])
# da1sit=rbind(colnames(da0sit),da0sit)
# da2sit=rbind(c("Wilaya",rep(unique(sitphy2$`Type de logements`),each=9)),da1sit)
# da2sit[2,1]="Wilaya"
# 
col_stops <- data.frame(
  q = c(0.33, 0.66, .99),
  c = c('#DF5353','#DDDF0D','#55BF3B'),
  stringsAsFactors = FALSE
)


livraison_wilayas=livraison_wilayas%>%
  filter(type_de_logement %in% c( "LPL","Rural","LPP","LSP","Location-Vente"))

#liv=livraison_wilayas%>%
#  filter(annee==2019,type_de_logement=="LPL")
#colnames(liv)[14]="Parc_logement2019"
#colnames(liv)[12]="Population_2019"

###################################### VILLE #########################
data_ville=data.frame(matrix(0,nrow=6,ncol=13))
data_ville$X1=c("BOUINANE","SIDI ABDELLAH","BOUGHEZOUL","DRAA ERICH","ALI MENDJLI","EL MENEAA")
colnames(data_ville)=c("Nom","Wilaya_Matricule","Wilaya","zoom","setview_long","setview_lat","cor_long","cor_lat","logements","habitants","equipements","energie","transport")
data_ville$Wilaya_Matricule=c("09-BLIDA","16-ALGER","17-26 DJELFA MEDEA","23-ANNABA","25-Constantine","47-GHARDAIA")
data_ville$Wilaya=c("BLIDA","ALGER","DJELFA MEDEA","ANNABA","Constantine","GHARDAIA")


#providers[[1]]  #OpenStreetMap.Mapnik
#providers[[44]] # ne s'affiche pas quand on zoom plus
#providers[[55]] # ne s'affiche pas quand on zoom plus
#providers[[57]]  #Satellite


####Sidi abdellah
data_ville[2,4]=14
data_ville[2,5:6]=c(2.853174,36.685945)
data_ville[2,7:8]=c(2.853174,36.689245)

#Bouinane
data_ville[1,4]=15
data_ville[1,5:6]=c(2.957531,36.533262)
data_ville[1,7:8]=c(2.957531,36.533262)

#BOUGHEZOUL
data_ville[3,4]=13
data_ville[3,5:6]=c(2.848758,35.721653)
data_ville[3,7:8]=c(2.848758,35.741653)



#DRAA ERRICH
data_ville[4,4]=13
data_ville[4,5:6]=c(7.528319,36.858245)
data_ville[4,7:8]=c(7.528319,36.872245)

#ALI MENDJLI
data_ville[5,4]=14
data_ville[5,5:6]=c(6.572546,36.249052)
data_ville[5,7:8]=c(6.572546,36.254052)

#EL MENEAA
data_ville[6,4]=14
data_ville[6,5:6]=c(2.905987,30.594278)
data_ville[6,7:8]=c(2.915987,30.594278)



###################################### VILLE #########################



#algeria=rgdal::readOGR("/cloud/project/polbnda_dza.json")
algeria=rgdal::readOGR(paste0(getwd(),"/polbnda_dza.json"))


#class(algeria)
#glimpse(algeria)
#glimpse(algeria@data)
#slotNames(algeria)
#algeria@data$pop[1:48]=rnorm(48,1000000,300000)
#algeria@data$pop[49:96]=algeria@data$pop[1:48]

id_wilaya=c(27,31,29,22,46,13,20,15,6,35,16,42,9,10,2,19,26,44,34,28,38,48,17,14,5,7,21,23,36,18,24,43,25,41,4,12,40,8,32,45,1,3,47,30,39,33,37,11)

algeria@data$id_wilaya=id_wilaya
algeria@data=algeria@data[1:96,]

#algeria@data=algeria@data%>%
#arrange(id_wilaya)

#for(i in 1:96){
#  j=algeria@data$id_wilaya[i]
#  algeria@data$parc_logts[i]=liv$Parc_logement2019[j]
#}

#for(i in 1:96){
# j=algeria@data$id_wilaya[i]
#  algeria@data$pop[i]=liv$Population_2019[j]
#}

algeria@data=algeria@data[1:48,]

#palo <- colorNumeric("YlGnBu",algeria@data$pop)
#algeria@data$couleur=palo(algeria@data$pop)

gps=data.frame(longitude=rep(0,48),latitude=rep(0,48))
gps=data.frame(longitude=rep(0,48),latitude=rep(0,48))
for(i in 1:48){
  gps[i,]=algeria@polygons[[i]]@labpt
}
algeria@data$longitude=gps$longitude
algeria@data$latitude=gps$latitude
algeria@data$wilayas=unique(livraison_wilayas$waw)[id_wilaya]

#algeria@data$nam=unique(livraison_wilayas$waw)[round(livraison_wilayas%>%
#                                                      group_by(id_wilaya)%>%
#                                                     summarise(liv=sum(Livraison))%>%
#                                                    arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,30,44,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
#                                                   select(id_wilaya))$id_wilaya]

mapdz=leaflet(algeria)%>%
  setView(lng = 3.03333 , lat = 28.6167, zoom = 5)%>%
  #clearBounds()%>%
  
  addProviderTiles("OpenStreetMap.BZH",
                   options = leafletOptions(minZoom = 5, maxZoom = 10,dragging = TRUE))%>%   #or we can use addProviderTiles(one_providers)
  
  setMapWidgetStyle(list(background= "#ffffff"))


mytheme0=create_theme(
  theme = "united",
  bs_vars_wells(
    bg = "#fff"
  ),
  bs_vars_wells(
    
  )
)

jsCode1 <- "shinyjs.opac1 = function(params){$('#well_gauge').css('opacity', params);}"
jsCode2 <- "shinyjs.opac2 = function(params){$('#well_gauge2').css('opacity', params);}"
jsCode3 <- "shinyjs.opac3 = function(params){$('#well_gauge3').css('opacity', params);}"
jsCode4 <- "shinyjs.opac4 = function(params){$('#well_gauge4').css('opacity', params);}"
jsCode5 <- "shinyjs.opac5 = function(params){$('#well_gauge5').css('opacity', params);}"
jsCode6 <- "shinyjs.opac6 = function(params){$('#well_gauge6').css('opacity', params);}"



# var df=document.getElementsByClassName('highcharts-data-label')
# df[2].children[0].lastElementChild.textContent="4645"   #(325 458)

# df[2].children[0].style.fontSize="0px"           # LSP 325 458 to 0px


# var arr=document.getElementsByClassName('highcharts-data-label-connector')
# arr[1].style.stroke="#ffffff"             for arrow





#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
