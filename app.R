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





ui <- fluidPage(
  #includeScript("www/check.js"),
  shinyjs::useShinyjs(),
  extendShinyjs(text = jsCode1, functions = c("opac1")),
  extendShinyjs(text = jsCode2, functions = c("opac2")),
  extendShinyjs(text = jsCode3, functions = c("opac3")),
  extendShinyjs(text = jsCode4, functions = c("opac4")),
  extendShinyjs(text = jsCode5, functions = c("opac5")),
  extendShinyjs(text = jsCode6, functions = c("opac6")),
  

  use_theme(create_theme(theme="united",bs_vars_wells(bg="#fff"))),
  setBackgroundColor(
    color = c("#f2f2f2", "#f2f2f2"), 
    gradient = "radial",
    direction = c("top", "left")
  ),
  
  tags$script(HTML(
  "const {container-fluid} = require('electron')
    container-fluid.setZoomFactor(3);
    "
  )),
  
  navbarPage(
    HTML("Ministere de l'Habitat,<br/> de l'Urbanisme et de la Ville"),
    id = "main_navbar",
    tabPanel("Livraison de Logements",
             dropdown(
               
               tags$h3("Filtrage des données"),
               br(),
               pickerInput(
                 inputId = "wilayas",
                 label = "", 
                 choices = unique(livraison_wilayas$waw),
                 options = list(`actions-box` = TRUE,style = "btn-primary",
                                `selected-text-format`= "count>2",
                                `count-selected-text` = "{0} Wilayas séléctionnés",
                                `none-selected-text` ="Wilaya",`select-all-text`=
                                  tags$div(
                                    "Séléctionner", 
                                    tags$br(),
                                    "Tout"
                                  ),
                                `deselect-all-text`=tags$div(
                                  "Deselectionner", 
                                  tags$br(),
                                  "Tout"
                                )),
                 multiple = TRUE
               ),
               pickerInput(
                 inputId = "segments",
                 label = "", 
                 choices = unique(livraison_wilayas$type_de_logement),
                 options = list(`actions-box` = TRUE,style = "btn-primary",
                                `selected-text-format`= "count>2",
                                `count-selected-text` = "{0} Segment séléctionnés",
                                `none-selected-text` ="Segment",`select-all-text`=
                                  tags$div(
                                    "Séléctionner", 
                                    tags$br(),
                                    "Tout"
                                  ),
                                `deselect-all-text`=tags$div(
                                  "Deselectionner", 
                                  tags$br(),
                                  "Tout"
                                )),
                 multiple = TRUE
               ),
               br(),
               sliderInput("annees","Année :",
                           min = 2000, max = 2019, value = c(2000,2019),sep = ""),
               
               
               style = "material-circle", icon = icon("bars"),
               status = "primary", width = "300px"
               #, animate = animateOptions(
               # enter = animations$fading_entrances$fadeInRightBig,
               #exit = animations$fading_exits$fadeOutRightBig
               # )
             ),
             fluidRow(
               column(width=2,style = "background-color:#eaeaea;",
                      fluidRow( tags$div(class="card",
                                         tags$span(class="title","Livraisons des logements",style="display:inline-block;"),textOutput("titre_livraison"),
                                         tags$h3(class="metric",textOutput("livraisons")),
                                         tags$span(class="color__nb",htmlOutput("taux_livraisons"),style="color:green;display: inline-block;"),textOutput("dernier_an") #style=paste0("color:",ifelse(34>0,"green","red"))
                      ),
                      
                      
                      
                      tags$head(HTML('<style type="text/css">

.modal-open .modal {
    overflow-x: hidden;
    overflow-y: auto;
    z-index: 99999999;
}

#display_when_hover_choose_leaflet {
    position: absolute;
    top: 137px;
    right: 498px;
    z-index: 999999;
    background: white;
    border: 3px solid darkgrey;
    width: 191px;
    color: black;
    font-family: unset;
    font-size: 13px;
    height:132px;
    display:none;
}


#display_when_hover_choose_line1 {
    position: absolute;
    top: 334px;
    right: 471px;
    z-index: 999999999;
    background: white;
    border: 3px solid darkgrey;
    width: 191px;
    color: black;
    font-family: unset;
    font-size: 13px;
    height: 82px;
    display: none
}


#choose_leaflet {
    position: absolute;
    top: 148px;
    right: 628px;
    z-index: 99999;
    background: white;
    border: 3px solid darkgrey;
    width: 43px;
}

#choose_line1 {
    position: absolute;
    top: 373px;
    right: 619px;
    z-index: 999;
    background: white;
    border: 3px solid darkgrey;
    width: 43px;
}



#choose_leaflet .glyphicon-th-list{
  color: black;
  margin-left: -3px;
}

#choose_line1 .glyphicon-th-list{
  color: black;
  margin-left: -3px;
}

#eptotal{
    margin-top: -25px;
    text-align: end;
}

.f1e{
padding-bottom: 34px;
padding-left: 0px;
font-family: inherit;
font-size: 26px;
text-align:end;
}


#equip_highchart {
  padding-top:20px;
}

.highcharts-text-outline{
stroke-width:0px;
}

#tabmodal_sitphy {
    position: absolute;
    z-index: 9999;
    top: 51px;
}

#rowselect .shiny-input-container:not(.shiny-input-container-inline) {
    width: 300px;
    max-width: 100%;
    position: absolute;
    z-index: 999999;
    right: 900px;
    top:7px;
}

#select_segment_gauge{
padding-left:80px
}

.btn-primary:active.focus, .btn-primary.active:hover {
    color: #fff;
    background-color: rebeccapurple;
    border-color: darkslategray;
}

.btn-primary.active {
    color: #fff;
    background-color: rebeccapurple;
    background-image: none;
    border-color: darkslategray;
}

#wilayaselectgauge1 {
    padding-top: 8px;
    padding-left: 71px;
    color: white;
    font-family: cursive;
    font-size: 18px;
}



#wilayaselectgauge2 {
    padding-top: 8px;
    padding-left: 311px;
    color: white;
    font-family: cursive;
    font-size: 18px;
}



#wilayaselectgauge3 {
    padding-top: 8px;
    padding-left: 308px;
    color: white;
    font-family: cursive;
    font-size: 18px;
}


#wilayaselectgauge4 {
    padding-top: 8px;
    padding-left: 290px;
    color: white;
    font-family: cursive;
    font-size: 18px;
}



#wilayaselectgauge5 {
    padding-top: 8px;
    padding-left: 165px;
    color: white;
    font-family: cursive;
    font-size: 18px;
}


#wilayaselectgauge6 {
    padding-top: 8px;
    padding-left: 308px;
    color: white;
    font-family: cursive;
    font-size: 18px;
}



#cgauge6{
margin-left:150px;
}

#gauge6 {
    margin-left: -160px;
    padding-left: 92px;
    margin-top:-12px;
    height:247px;
}

#livraison_gauge6 {
    position: relative;
    top: -238px;
    right: -393px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#livraison_gauge6_val {
    position: relative;
    top: -250px;
    right: -171px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}

#prevision_gauge6 {
    position: relative;
    top: -246px;
    right: -394px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#prevision_gauge6_val {
    position: relative;
    top: -260px;
    right: -173px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}




#well_gauge6{
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    width:570px;
    height:268px;
    background-color: ghostwhite;

}

.chart-title-gauge6 {
    width: 570px;
    height: 20px;
    border-bottom: 34px solid;
    border-bottom-color: darkcyan;
    font-size: 20px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -19px;
    display:flex;
    margin-bottom:10px;
}

#titregauge6 {
    padding-left: 17px;
    color: white;
    font-size: 29px;
    padding-top: 0px;
    font-family: inherit;
}









#cgauge5{
margin-left:150px;
}

#gauge5 {
    margin-left: -160px;
    padding-left: 92px;
    margin-top:-12px;
    height:247px;

}


#livraison_gauge5 {
    position: relative;
    top: -238px;
    right: -393px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#livraison_gauge5_val {
    position: relative;
    top: -250px;
    right: -171px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}

#prevision_gauge5 {
    position: relative;
    top: -246px;
    right: -394px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#prevision_gauge5_val {
    position: relative;
    top: -260px;
    right: -173px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}




#well_gauge5{
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    width:570px;
    height:268px;
    background-color: ghostwhite;

}

.chart-title-gauge5 {
    width: 570px;
    height: 20px;
    border-bottom: 34px solid;
    border-bottom-color: darkcyan;
    font-size: 20px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -19px;
        display:flex;
            margin-bottom:10px;


}

#titregauge5 {
    padding-left: 17px;
    color: white;
    font-size: 29px;
    padding-top: 0px;
    font-family: inherit;
}









#cgauge4{
margin-left:150px;
}

#gauge4 {
    margin-left: -160px;
    padding-left: 92px;
    margin-top:-12px;
    height:247px;
    
}



#livraison_gauge4 {
    position: relative;
    top: -238px;
    right: -393px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#livraison_gauge4_val {
    position: relative;
    top: -250px;
    right: -171px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}

#prevision_gauge4 {
    position: relative;
    top: -246px;
    right: -394px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#prevision_gauge4_val {
    position: relative;
    top: -260px;
    right: -173px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}


#well_gauge4{
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    width:570px;
    height:268px;
    background-color: ghostwhite;

}

.chart-title-gauge4 {
    width: 570px;
    height: 20px;
    border-bottom: 34px solid;
    border-bottom-color: darkcyan;
    font-size: 20px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -19px;
        display:flex;
    margin-bottom:10px;

}

#titregauge4 {
    padding-left: 17px;
    color: white;
    font-size: 29px;
    padding-top: 0px;
    font-family: inherit;
}





#gauge3 {
    margin-left: -160px;
    padding-left: 92px;
    margin-top:-12px;
    height:247px;
    
}
#livraison_gauge3 {
    position: relative;
    top: -238px;
    right: -393px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#livraison_gauge3_val {
    position: relative;
    top: -250px;
    right: -171px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}

#prevision_gauge3 {
    position: relative;
    top: -246px;
    right: -394px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#prevision_gauge3_val {
    position: relative;
    top: -260px;
    right: -173px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}


#well_gauge3{
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    width:570px;
    height:268px;
    background-color: ghostwhite;

}

.chart-title-gauge3 {
    width: 570px;
    height: 20px;
    border-bottom: 34px solid;
    border-bottom-color: darkcyan;
    font-size: 20px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -19px;
        display:flex;
    margin-bottom:10px;

}

#titregauge3 {
    padding-left: 17px;
    color: white;
    font-size: 29px;
    padding-top: 0px;
    font-family: inherit;
}














#cgauge2{
margin-left:150px;
}

#gauge2 {
    margin-left: -160px;
    padding-left: 92px;
    margin-top:-12px;
    height:247px;
    
}


#livraison_gauge2 {
    position: relative;
    top: -238px;
    right: -393px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#livraison_gauge2_val {
    position: relative;
    top: -250px;
    right: -171px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}

#prevision_gauge2 {
    position: relative;
    top: -246px;
    right: -394px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#prevision_gauge2_val {
    position: relative;
    top: -260px;
    right: -173px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}


#well_gauge2{
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    width:570px;
    height:268px;
    background-color: ghostwhite;

}

.chart-title-gauge2 {
    width: 570px;
    height: 20px;
    border-bottom: 34px solid;
    border-bottom-color: darkcyan;
    font-size: 20px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -19px;
        display:flex;
    margin-bottom:10px;

}

#titregauge2 {
    padding-left: 17px;
    color: white;
    font-size: 29px;
    padding-top: 0px;
    font-family: inherit;
}





#gauge1 {
    margin-left: -160px;
    padding-left: 92px;
    margin-top:-12px;
    height:247px;
}


#livraison_gauge1 {
    position: relative;
    top: -238px;
    right: -393px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#livraison_gauge1_val {
    position: relative;
    top: -250px;
    right: -171px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}

#prevision_gauge1 {
    position: relative;
    top: -246px;
    right: -394px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#prevision_gauge1_val {
    position: relative;
    top: -260px;
    right: -173px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}


#well_gauge{
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    width:570px;
    height:268px;
    background-color: ghostwhite;

}

.chart-title-gauge {
    width: 570px;
    height: 20px;
    border-bottom: 34px solid;
    border-bottom-color: lightcoral;
    font-size: 20px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -19px;
        display:flex;
    margin-bottom:10px;

}

#toutsegments_gauge {
    color: white;
    font-size: 21px;
    padding-top: 4px;
    margin-left: -6px;
}

#titregauge1 {
    padding-left: 17px;
    color: white;
    font-size: 27px;
    padding-top: 0px;
    font-family: inherit;
}


                      
#tabsitphy {
    margin-right: -15px;
    margin-left: -13px;
    margin-top: -8px;
    overflow: -webkit-paged-x;
    zoom:0.95;

}

                      
#ttwilayas {
text-align: center;
font-size:32px;
font-family: "Roboto";
color: #167c76;
margin-top:-6px;
}

#ttwilayas2 {
text-align: center;
font-size:32px;
font-family: "Roboto";
color: #167c76;
margin-top:1px;
padding-bottom:14px;
}


#region {
    text-align: center;
    padding-bottom: 1px;
    font-size: 20px;
    margin-top: -10px;
    font-family: "Roboto";
    color: #5a5096;
}


#urbanisme1 {
    width: 100%;
    height: 350px;
    visibility: inherit;
    overflow: hidden;
    zoom:0.89;
}

                      
#urbanisme2 {
    width: 100%;
    height: 350px;
    visibility: inherit;
    overflow: hidden;
    zoom: 0.88;
    padding-top: 10px;
}


#table2 .ReactTable .rt-tr-group {
    -webkit-box-flex: 1;
    -ms-flex: 1 0 auto;
    flex: 1 0 auto;
    display: -webkit-box;
    display: -ms-flexbox;
    display: flex;
    -webkit-box-orient: vertical;
    -webkit-box-direction: normal;
    -ms-flex-direction: column;
    flex-direction: column;
    -webkit-box-align: stretch;
    -ms-flex-align: stretch;
    align-items: stretch;
    cursor: pointer;
}
  
#table2 .ReactTable .rt-tr-group:hover {
     background-color: lightgrey;
  }


#equip_reactable .ReactTable .rt-tr-group:hover {
     background-color: lightgrey;
  }
  
#table2 .rt-td rt-align-left{
  width:144px;
  }
  
#table2 .rt-th-content {
    overflow: hidden;
    text-overflow: ellipsis;
    font-size: 12px;
    text-align: left;
  }

#c1 {
margin-right:-300px;
}

#cg2{
    margin-right: 150px;
}

  #table .ReactTable .rt-tr-group:hover {
     background-color: lightgrey;
  }
  
  #table .ReactTable .rt-tbody {
    -webkit-box-flex: 99999;
    flex: 99999 1 auto;
    display: -webkit-box;
    display: flex;
    -webkit-box-orient: vertical;
    -webkit-box-direction: normal;
    flex-direction: column;
    padding-top: 105px;
}
  
  #table .ReactTable .rt-tr-group {
    -webkit-box-flex: 1;
    -ms-flex: 1 0 auto;
    flex: 1 0 auto;
    display: -webkit-box;
    display: -ms-flexbox;
    display: flex;
    -webkit-box-orient: vertical;
    -webkit-box-direction: normal;
    -ms-flex-direction: column;
    flex-direction: column;
    -webkit-box-align: stretch;
    -ms-flex-align: stretch;
    align-items: stretch;
    cursor: pointer;
    padding-bottom: 0px;
    margin: -2px;
    font-family: system-ui;
    font-size: 16px;
  }


#table .ReactTable .rt-table {
    flex: auto 1;
    flex-direction: column;
    -webkit-box-align: stretch;
    align-items: stretch;
    width: 100%;
    border-collapse: collapse;
    overflow: auto;
    overflow-y: hidden;
    overflow-x: hidden;
}
  
#side_wilaya {
    min-height: 20px;
    padding: 0px;
    margin-bottom: 20px;
    background-color: transparent;
    box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
    border: 0px solid #e3e3e3;
    border-radius: 0px;
}


#table .ReactTable .rt-thead.-header {
    position: fixed;
    background: inherit;
    z-index: 2;
    width: 277px;
    padding-top:70px;
}

 
.jexcel_content {  display: inline-block;
    box-sizing: border-box;
    padding-right: 3px;
    padding-bottom: 3px;
    position: relative;
    max-height: 120% !important;
          }
                            
.navbar-default .navbar-nav>li>a:hover, .navbar-default .navbar-nav>li>a:focus {
    color: #fff;
    background-color: transparent;
}

.navbar-default .navbar-nav>.open>a, .navbar-default .navbar-nav>.open>a:hover, .navbar-default .navbar-nav>.open>a:focus {
    color: #fff;
    background-color: transparent;
}

.dropdown-menu>li>a:hover, .dropdown-menu>li>a:focus {
    color: #fff;
    text-decoration: none;
    background-color: lightseagreen;
}

.dropdown-menu>li>a:hover {
    background-color: lightseagreen;
}

.dropdown-menu>.active>a, .dropdown-menu>.active>a:hover, .dropdown-menu>.active>a:focus {
    color: #fff;
    text-decoration: none;
    background-color: lightseagreen;
    outline: 0;
}

         
#excel1{
         width: 100%;
         height: 560px;
         visibility: inherit;
}
      
#excel1.jexcel_content{

    overflow-y: auto;
    max-height: 550px;
}
             
.navbar>.container-fluid .navbar-brand {
    margin-left: -15px;
    margin-top: -14px;
    line-height: 110%;
    font-family: system-ui;
    font-weight: 500;
 }

#tabmodalserie_eqp {
    position: absolute;
    top: -12px;
    right: -177px;
    z-index:9999999;
}

#tabmodalserie1 {
position: absolute;
top: -6px;
right: 15px;
}


#tabmodalserie2 {
position: absolute;
top: -6px;
right: 15px;
}


#tabmodalserie3 {
position: absolute;
top: -6px;
right: 15px;
}

.modal-body {
    position: relative;
    padding: 20px;
    overflow-x: auto;
}

#tabmodalserie_urbanisme1 {
position: absolute;
top: -6px;
right: 15px;
}

#tabmodalserie_urbanisme2 {
position: absolute;
top: 396px;
right: 15px;
}


#tabmodalserie_urbanisme3 {
position: absolute;
top: -6px;
right: 15px;
}

#tabmodalserie_urbanisme4 {
position: absolute;
top: 396px;
right: 15px;
}


.bttn-bordered.bttn-success{
border-color: transparent;
color:darkslategrey;
}

.bttn-bordered.bttn-success:focus, .bttn-bordered.bttn-success:hover{
border-color: transparent;
color:black;
}


#titre_serie2 {
display: inline
}

#periode {
display: inline
}

#periode2 {
display: inline
}



#titre_serie3 {
display: inline
}


#titre_serie {
display: inline
}


.chart-title {
    width:107%;height:20px;
    border-bottom: 1px solid #d7d7d7;
    color: #666;
    font-size: 12px;
    font-weight: 300;
    margin-left:-20px;
    margin-top:5px;
    display: flex;
}


.chart-title-eqp {

    width: 128%;
    height: 20px;
    border-bottom: 1px solid #d7d7d7;
    color: #666;
    font-size: 12px;
    font-weight: 300;
    margin-top: -2px;
    display: flex;
    margin-left: -27px;

}

.chart-title-urbanisme {
    width: 106%;
    height: 20px;
    border-bottom: 1px solid #d7d7d7;
    color: #666;
    font-size: 12px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -10px;
    display: flex;
}

.chart-title-urbanisme2 {
    width: 106%;
    height: 20px;
    border-bottom: 1px solid #d7d7d7;
    color: #666;
    font-size: 12px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -10px;
    display: flex;
}


.pretty {
position : absolute;
right: 250px;
}

#livraisons {

    margin-bottom: -5px;

}

#titre_livraison {
    
    display:inline;
    font-size:12px;

                }
#dernier_an {
    display: inline-block;
}


#dernier_an2 {
    display: inline-block;
}


#dernier_an3 {
    display: inline-block;
}


#dernier_an4 {
    display: inline-block;
}


#dernier_an8 {
    display: inline-block;
}


.sw-dropdown {
    position: absolute;
    /* display: inline-block; */
    top: 10px;
    right: 30px;
    z-index: 1000;
}

.leaflet-top, .leaflet-bottom {
    position: absolute;
    z-index: 100;
    pointer-events: none;
}

.sw-dropdown-content {
    display: none;
    position: absolute;
    right: 0px;
    -moz-border-radius: 10px;
    -webkit-border-radius: 10px;
    border-radius: 10px;
    background: none repeat scroll 0% 0% #FFF;
    -moz-box-shadow: 0px 0px 15px 0px #c0c0c0;
    -webkit-box-shadow: 0px 0px 15px 0px #c0c0c0;
    -o-box-shadow: 0px 0px 15px 0px #c0c0c0;
    box-shadow: 0px 0px 15px 0px #c0c0c0;
    z-index: 5;
}

.irs-with-grid {
    height: 60px;
    
}

.irs-min, .irs-max {
    color: #333;
    font-size: 10px;
    line-height: 1.333;
    text-shadow: none;
    top: 0;
    padding: 1px 3px;
    background: rgba(0,0,0,0.1);
    border-radius: 3px;
    -moz-border-radius: 3px;
}

.irs-from, .irs-to, .irs-single {
    color: #fff;
    font-size: 16px;
    line-height: 1.333;
    text-shadow: none;
    padding: 0px 2px;
    background: #428bca;
    border-radius: 3px;
    -moz-border-radius: 3px;
}

.navbar-default {
  background-color: #007bff !important;
    border-bottom: 3px solid #0062cc;
  box-shadow: 0px 5px 15px grey;
}

.navbar {
  position: relative;
  min-height: 63px;
  margin-bottom: 20px;
  border: 1px solid transparent;
  margin-right: -15px;
  margin-left: -15px;
  padding-top: 5px;
}

navbar-default .navbar-brand {
  color: #fff;
    font-size: 20px;
}

.navbar-brand {
  float: left;
  height: 50px;
  padding: 18px 15px;
  font-size: 20px;
  line-height: 15px;
}

.navbar-nav {
  float: left;
  margin: 0;
  font-size: 16px;
  border-left: 3px solid #f2f2f2;
}


.navbar-default .navbar-nav > .active > a {
  color: #fff;
  background-color: transparent;
  font-size: 19px;
}



.navbar-default .navbar-nav > li > a {
  color: #e5e5e5;
}


.navbar-default .navbar-nav > .active > a:link {
  background-color: transparent;
}


.navbar-default .navbar-nav > li > a:hover {
  background-color: transparent;
}

.title {
    font-size: 16px;
    font-weight: 500;
    margin: 0;
}

.well#well1 {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
}


.well#well12 {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
}

.btn-primary {
    color: #fff;
    background-color: cadetblue;
    border: 3px solid grey;
}

:not(.input-group)>.bootstrap-select.form-control:not([class*=col-]){
width:80%
}


.btn dropdown-toggle btn-primary bs-placeholder{

background-color:cadetblue

}


.bootstrap-select .dropdown-toggle .filter-option {
    position: static;
    top: 0;
    left: 0;
    float: left;
    height: 100%;
    width: 100%;
    text-align: left;
    font-weight: 500;
    font-family: system-ui;
    font-size: 16px;
    color: white;
    overflow: hidden;
    -webkit-box-flex: 0;
    -webkit-flex: 0 1 auto;
    -ms-flex: 0 1 auto;
    flex: 0 1 auto;
}

.well .shiny-input-container {
    width: auto;
    display: inline;
    margin-right: 30px;
    zoom: 1.05;
    margin-left: 110px;
}


.btn-primary {
    color: #fff;
    background-color: rgb(44, 168, 116);
    border: 3px solid grey;
}

.btn-primary:hover {
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}

.btn-primary:active {
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}

.open>.btn-primary.dropdown-toggle:focus{

    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}

.dropdown-menu>li>a:hover{
    background-color: mediumseagreen;

}


.btn-primary:active:hover {
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}


.btn-primary:focus {
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}

.btn-primary:visited {
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}


.open>.btn-primary.dropdown-toggle{
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}



.open>.btn-primary.dropdown-toggle:selected{
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}


.open>.btn-primary.dropdown-toggle:hover{
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}


.title i[class^="fa "] {
    color: #a9a9a9;
    font-size: 14px;
    position: relative;
    top: 10px;
    right: 10px
}

.metric {
    font-size: 33px;
    font-weight: 700;
    margin: .25em 0
}

.color__nb {
  display: inline;
  font-size: 16px;
  font-weight: 700;
  margin: .25em 0
}


.title {
  background: transparent ;
  text-align: left;

}

.title h2 {
  font-size: 16px;
  font-weight: 530;
  margin-bottom: 5;
  margin-right:-1em;
  margin-top:-2em;
  padding: 0px 15px;
  padding-left: 0px;
  padding-right: 2px;

  color: #595959;
  display: inline-block;
  font-family: Verdana;
}


.color__nb span {
    font-weight: 100;
    color: #000000;
}


.color__nb {
    color: green;
}


.card{
	border-radius:1px;
  font-family: sans-serif;
  padding: 1rem;
  width: 30rem;
  height: 12rem;
  float: left;
  margin-top: 0rem;
  margin-bottom: 1.66rem;
  background: white;
  box-shadow: 4px 5px 11px grey;
  transition: all .2s ease;
  border-bottom: 4px solid transparent;
  border-top: 4px solid #109485;
}
.card:hover{
  border-bottom: 4px solid #008571;
      z-index: 2;
    -webkit-transition: all 200ms ease-in;
    -webkit-transform: scale(1.5);
    -ms-transition: all 200ms ease-in;
    -ms-transform: scale(1.5);   
    -moz-transition: all 200ms ease-in;
    -moz-transform: scale(1.5);
    transition: all 200ms ease-in;
    transform: scale(1.03);
}
.card.selected{
  transform: scale(1.075);
  box-shadow: 0 0 16px 1px rgba(0,0,0,.3);
}

</style>')) ),
                      
                      fluidRow( tags$div(class="card",
                                         tags$h4(class="title","Lancements de logements"),
                                         tags$h3(class="metric","3 333 333"),
                                         tags$span(class="color__nb","+34",style=paste0("color:",ifelse(34>0,"green","red"))),"siste 3050 dagene"
                      ) ),
                      
                      
                      fluidRow( tags$div(class="card",
                                         tags$h4(class="title",textOutput("max_annee_parclogement")),
                                         tags$h3(class="metric",textOutput("parclogements")),
                                         tags$span(class="color__nb",textOutput("taux_parclogement"),style="color:green;display: inline-block;"),textOutput("dernier_an2") #style=paste0("color:",ifelse(34>0,"green","red"))
                      ) ),
                      
                      
                      
                      fluidRow( tags$div(class="card",
                                         tags$h4(class="title",textOutput("max_annee_population")),
                                         tags$h3(class="metric",textOutput("populations")),
                                         tags$span(class="color__nb",textOutput("taux_population"),style="color:green;display: inline-block;"),textOutput("dernier_an3")
                      ) ),
                      
                      
                      
                      fluidRow( tags$div(class="card",
                                         tags$h4(class="title",textOutput("max_annee_tol")),
                                         tags$h3(class="metric",textOutput("tols")),
                                         tags$span(class="color__nb",textOutput("taux_tol"),style="color:green;display: inline-block;"),textOutput("dernier_an4")
                      ) ),
                      
                      fluidRow( tags$div(class="card",
                                         tags$h4(class="title",textOutput("max_annee_densite")),
                                         tags$h3(class="metric",textOutput("densite")),
                                         tags$span(class="color__nb",textOutput("taux_densite"),style="color:green;display: inline-block;"),textOutput("dernier_an8") #style=paste0("color:",ifelse(34>0,"green","red"))
                      ) )
                      
               ),
               column(width=5,style = "background-color:#eaeaea;",
                      wellPanel(id="well1",
                                tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_hchart1"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie1_hchart1"),
                                         
                                         actionBttn(
                                           inputId = "tabmodalserie1",
                                           label = NULL,
                                           style = "bordered", 
                                           color = "success",
                                           icon = icon("table")
                                         ),
                                         
                                         actionBttn(
                                           inputId = "choose_line1",
                                           label = NULL,
                                           style = "simple", 
                                           color = "primary",
                                           icon = icon("th-list",lib = "glyphicon")
                                         ),
                                         div(id="display_when_hover_choose_line1",
                                             
                                             awesomeRadio(
                                               inputId = "radio_choose_line1",
                                               label = "", 
                                               choices = c("Livraisons de Logements", "TOL"),
                                               selected = "Livraisons de Logements"
                                               #,color="default"
                                             )
                                             
                                         )
                                ),
                                div(
                                  style = "padding-top: 30px;zoom:100%;"
                                  ,highchartOutput("hchart1", height = 330)
                                  
                                )
                                
                                ,prettySwitch(
                                  inputId = "Id027",
                                  label = "Par Segment", 
                                  status = "primary",
                                  slim = TRUE,
                                  value=TRUE,
                                )),
                      fluidRow(
                        column(width=12,style = "background-color: transparent;zoom:100%",
                               wellPanel(id="well12",
                                         
                                         tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),"Livraisons de logements par Segments : ",HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie2"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("periode"),
                                                  
                                                  actionBttn(
                                                    inputId = "tabmodalserie2",
                                                    label = NULL,
                                                    style = "bordered", 
                                                    color = "success",
                                                    icon = icon("table")
                                                  ))
                                         ,div(style = "margin-top:-1em;margin-bottom:-1em;margin-left:-1em;margin-right:-1em;padding-top: 10px;",
                                              highchartOutput("pie", height = 330)))
                        )
                      )
               ),
               column(width=5,style = "background-color:#eaeaea;",
                      wellPanel(id="well1",
                                tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_map"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie3"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("periode2"),
                                         
                                         actionBttn(
                                           inputId = "tabmodalserie3",
                                           label = NULL,
                                           style = "bordered", 
                                           color = "success",
                                           icon = icon("table")
                                         ),
                                         
                                         actionBttn(
                                           inputId = "choose_leaflet",
                                           label = NULL,
                                           style = "simple", 
                                           color = "primary",
                                           icon = icon("th-list",lib = "glyphicon")
                                         ),
                                         div(id="display_when_hover_choose_leaflet",
                                             
                                             awesomeRadio(
                                               inputId = "radio_choose_leaflet",
                                               label = "", 
                                               choices = c("Livraisons de Logements", "TOL", "Population","Parc Logements"),
                                               selected = "Livraisons de Logements"
                                              #,color="default"
                                             )
                                             
                                             )
                                         
                                         
                                         )
                                
                                ,div(style = "padding: 0px 0px;zoom:122%;height:620px;",
                                     shinycssloaders::withSpinner(
                                       leafletOutput("distPlot2",height=613
                                                     #ifelse(abad()==1,700,568) 
                                       ))
                                ))
               )
               
               
             ),
             #tabmodalserie_eqp
             bsModal("modal1", htmlOutput("tablededonnes1"), "tabmodalserie1", size = "large"
                     ,excelOutput("excel1")),
             
             bsModal("modal2",htmlOutput("tablededonnes2"), "tabmodalserie2", size = "large"
                     ,excelOutput("excel2")),
             
             bsModal("modal3",htmlOutput("tablededonnes3"), "tabmodalserie3", size = "large"
                     ,excelOutput("excel3"))
    ),
    
    
    
    
    
    tabPanel("Urbanisme",
             fluidRow(
               
               style = "display:flex; align-items:flex-start",
               wellPanel(id="side_wilaya",
                         style = "overflow-y: auto; position:fixed; width:300px; top:0; bottom:0",
                         reactableOutput("table")
               ),
               fluidRow( #~~ Main panel ~~#
                 style = "flex-grow:1; resize:horizontal; overflow: hidden; position:relative; margin-left: 310px",
                 column(6,
                        wellPanel(id="urbanisme_well",style="width:100% ; height:380px;",
                                  tags$div(class="chart-title-urbanisme",HTML('&nbsp;'),HTML('&nbsp;'),tags$p("Instruments d'Urbanisme : "),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("wilayaselect1"),
                                           actionBttn(
                                             inputId = "tabmodalserie_urbanisme1",
                                             label = NULL,
                                             style = "bordered", 
                                             color = "success",
                                             icon = icon("table")
                                           )
                                  ),
                                  
                                  highchartOutput("urbanisme1")      
                        ),
                        
                        wellPanel(id="urbanisme_well2",style="width:100% ; height:380px;",
                                  tags$div(class="chart-title-urbanisme2",HTML('&nbsp;'),HTML('&nbsp;'),"Situation Financières : ",HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("wilayaselect2"),
                                           actionBttn(
                                             inputId = "tabmodalserie_urbanisme2",
                                             label = NULL,
                                             style = "bordered", 
                                             color = "success",
                                             icon = icon("table")
                                           )
                                  ),
                                  
                                  highchartOutput("urbanisme2")
                        )
                 ),
                 column(6,
                        wellPanel(id="urbanisme_well",style="width:100% ; height:380px;",
                                  tags$div(class="chart-title-urbanisme",HTML('&nbsp;'),HTML('&nbsp;'),"Lotissements sociaux : ",HTML('&nbsp;'),
                                           actionBttn(
                                             inputId = "tabmodalserie_urbanisme3",
                                             label = NULL,
                                             style = "bordered", 
                                             color = "success",
                                             icon = icon("table")
                                           )
                                  ),
                                  
                                  div(style="padding-top:2px;",
                                      textOutput("ttwilayas"),
                                      textOutput("region"),
                                      tags$table(class="ta",style="font-size: 21px;font-family: system-ui;",
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Nature juridique",style="padding-bottom: 3px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                         tags$td(class="f3",textOutput("zones1"),style="padding-bottom: 3px;text-align: end;font-weight: 500;font-size:19px;")
                                                 ),
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Nombre de Communes Concernées",style="padding-bottom: 3px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                         tags$td(class="f3",textOutput("zones2"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                 ),
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Superficie (ha)",style="padding-bottom: 3px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                         tags$td(class="f3",textOutput("zones3"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                 ),
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Nombre de lots",style="padding-bottom: 3px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                         tags$td(class="f3",textOutput("zones4"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                 )
                                                 ,
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Nombre de lots retenues",style="padding-bottom: 3px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-right: 40px;padding-left: 45px;padding-bottom: 3px;"),
                                                         tags$td(class="f3",textOutput("zones5"),style="text-align: end;padding-bottom: 3px;font-weight: 500;")
                                                 ),
                                                 
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Nombre de lots dégagés (Porte feuille)",style="padding-bottom: 3px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                         tags$td(class="f3",textOutput("zones6"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                 ),
                                                 tags$tr(class="f0",style="display:none;",
                                                         tags$td(class="f1","Surface moyenne des lots (m²)",style="padding-bottom: 3px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                         tags$td(class="f3",textOutput("zones7"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                 ),
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Nombre de sites",style="padding-bottom: 3px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-right: 40px;padding-left: 45px;padding-bottom: 3px;"),
                                                         tags$td(class="f3",textOutput("zones8"),style="text-align: end;padding-bottom: 3px;font-weight: 500;")
                                                 ),
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Nombre de permis",style="padding-bottom: 3px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-right: 40px;padding-left: 45px;padding-bottom: 3px;"),
                                                         tags$td(class="f3",textOutput("zones9"),style="text-align: end;padding-bottom: 3px;font-weight: 500;")
                                                 )
                                      )
                                      
                                  )                                 ),
                        
                        wellPanel(id="urbanisme_well",style="width:100% ; height:380px;",
                                  tags$div(class="chart-title-urbanisme",HTML('&nbsp;'),HTML('&nbsp;'),"Loi 18-05 : ",HTML('&nbsp;'),
                                           actionBttn(
                                             inputId = "tabmodalserie_urbanisme4",
                                             label = NULL,
                                             style = "bordered", 
                                             color = "success",
                                             icon = icon("table")
                                           )
                                           
                                  ),
                                  
                                  
                                  div(style="padding-top:2px;",
                                      textOutput("ttwilayas2"),
                                      tags$table(class="ta",style="font-size: 21px;font-family: system-ui;",
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Déposés",style="padding-bottom: 13px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                         tags$td(class="f3",textOutput("loi1"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                 ),
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Traités",style="padding-bottom: 13px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                         tags$td(class="f3",textOutput("loi2"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                 ),
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Favorables",style="padding-bottom: 13px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                         tags$td(class="f3",textOutput("loi3"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                 ),
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Défavorables",style="padding-bottom: 13px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                         tags$td(class="f3",textOutput("loi4"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                 )
                                                 ,
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Instances",style="padding-bottom: 13px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-right: 40px;padding-left: 45px;padding-bottom: 13px;"),
                                                         tags$td(class="f3",textOutput("loi5"),style="text-align: end;padding-bottom: 13px;font-weight: 500;")
                                                 ),
                                                 
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Déposés Instruction N°01",style="padding-bottom: 13px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                         tags$td(class="f3",textOutput("loi6"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                 ),
                                                 tags$tr(class="f0",
                                                         tags$td(class="f1","Traités_a",style="padding-bottom: 13px;padding-left: 27px;"),
                                                         tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                         tags$td(class="f3",textOutput("loi7"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                 )
                                      )
                                      
                                  )                                 
                        )
                 )
                 
                 
                 #verbatimTextOutput("selected")  
               )
             ),
             
             
             bsModal("modal_urbanisme1","Table de donnees", "tabmodalserie_urbanisme1", size = "large"
                     ,excelOutput("excel_urbanisme1")),
             
             bsModal("modal_urbanisme2","Table de donnees", "tabmodalserie_urbanisme2", size = "large"
                     ,excelOutput("excel_urbanisme2")),
             
             bsModal("modal_urbanisme3","Table de donnees", "tabmodalserie_urbanisme3", size = "large"
                     ,excelOutput("excel_urbanisme3")),
             
             bsModal("modal_urbanisme4","Table de donnees", "tabmodalserie_urbanisme4", size = "large"
                     ,excelOutput("excel_urbanisme4"))
             
             
             
             
    )
    
    
    
    
    ,tabPanel("Situation Physique des Logements",
              fluidRow(
                id="tabsitphy",
                
                column(6,id="c1",
                       # 
                       # awesomeCheckboxGroup(
                       #   inputId = "select_segment_gauge",
                       #   label = "Segments : ", 
                       #   choices = c("Rural", "LPL", "Location-Vente","LSP","LPP"),
                       #   inline = TRUE, 
                       #   status = "success"
                       # ),
                       # 
                       actionBttn(
                         inputId = "tabmodal_sitphy",
                         label = NULL,
                         style = "bordered", 
                         color = "success",
                         icon = icon("table")
                       ),
                       checkboxGroupButtons(
                         inputId = "select_segment_gauge",
                         choices = c("Rural", "LPL", "Location-Vente","LSP","LPP"),
                         status = "primary",
                         checkIcon = list(
                           yes = icon("ok", 
                                      lib = "glyphicon")
                         ),
                         width = "150%"
                       ),
                       
                       
                       
                       reactableOutput("table2")
                ),
                column(3,id="cg2",
                       wellPanel(id="well_gauge",
                                 tags$div(class="chart-title-gauge",tags$p(id="titregauge1",textOutput("toutsegments_gauge"),textOutput("wilayaselectgauge1")#textOutput("wgauge")
                                 )),
                                 highchartOutput("gauge1",height = "247px"),tags$p(id="livraison_gauge1","Livraison"),tags$span(id="livraison_gauge1_val",textOutput("val_livraison_gauge1")),tags$p(id="prevision_gauge1","Prevision"),tags$span(id="prevision_gauge1_val",textOutput("val_prevision_gauge1"))
                                 
                                 
                                 #,verbatimTextOutput("selected")
                       ),
                       wellPanel(id="well_gauge2",
                                 tags$div(class="chart-title-gauge2",tags$p(id="titregauge2","LPL",textOutput("wilayaselectgauge2"))),
                                 highchartOutput("gauge2",height = "247px"),tags$p(id="livraison_gauge2","Livraison"),tags$span(id="livraison_gauge2_val",textOutput("val_livraison_gauge2")),tags$p(id="prevision_gauge2","Prevision"),tags$span(id="prevision_gauge2_val",textOutput("val_prevision_gauge2"))
                                 
                                 
                       ),
                       wellPanel(id="well_gauge3",
                                 tags$div(class="chart-title-gauge3",tags$p(id="titregauge3","LSP",textOutput("wilayaselectgauge3"))),
                                 highchartOutput("gauge3",height = "247px"),tags$p(id="livraison_gauge3","Livraison"),tags$span(id="livraison_gauge3_val",textOutput("val_livraison_gauge3")),tags$p(id="prevision_gauge3","Prevision"),tags$span(id="prevision_gauge3_val",textOutput("val_prevision_gauge3"))
                                 
                                 
                                 #,verbatimTextOutput("selected")
                       )
                       
                ),
                column(3,   #3eme colonne
                       
                       wellPanel(id="well_gauge4",
                                 tags$div(class="chart-title-gauge4",tags$p(id="titregauge4","Rural",textOutput("wilayaselectgauge4"))),
                                 highchartOutput("gauge4",height = "247px"),tags$p(id="livraison_gauge4","Livraison"),tags$span(id="livraison_gauge4_val",textOutput("val_livraison_gauge4")),tags$p(id="prevision_gauge4","Prevision"),tags$span(id="prevision_gauge4_val",textOutput("val_prevision_gauge4"))
                                 
                                 
                                 #,verbatimTextOutput("selected")
                       ),
                       
                       #column(5,id="cgauge4",
                       wellPanel(id="well_gauge5",
                                 tags$div(class="chart-title-gauge5",tags$p(id="titregauge5","Location-Vente",textOutput("wilayaselectgauge5"))),
                                 highchartOutput("gauge5",height = "247px"),tags$p(id="livraison_gauge5","Livraison"),tags$span(id="livraison_gauge5_val",textOutput("val_livraison_gauge5")),tags$p(id="prevision_gauge5","Prevision"),tags$span(id="prevision_gauge5_val",textOutput("val_prevision_gauge5"))
                                 
                                 
                                 #,verbatimTextOutput("selected")
                       ),
                       wellPanel(id="well_gauge6",
                                 tags$div(class="chart-title-gauge6",tags$p(id="titregauge6","LPP",textOutput("wilayaselectgauge6"))),
                                 highchartOutput("gauge6",height = "247px"),tags$p(id="livraison_gauge6","Livraison"),tags$span(id="livraison_gauge6_val",textOutput("val_livraison_gauge6")),tags$p(id="prevision_gauge6","Prevision"),tags$span(id="prevision_gauge6_val",textOutput("val_prevision_gauge6"))
                                 
                                 
                                 #,verbatimTextOutput("selected")
                       )
                       
                ),
                
                bsModal("modal_sitphy","Table de donnees", "tabmodal_sitphy", size = "large"
                        ,excelOutput("excel_sitphy"))
              )
    ),
    
    
    tabPanel("Ville",
             fluidRow(
               column(width=3,style="height: 800px;background-color:white;box-shadow: 4px 5px 11px grey;width: 21%;margin-right: 20px;margin-left: 36px;"),
               column(width=9,style="margin-left: 0px;width: 74%;background-color:white;",
                      fluidRow(id="rowselect",style="height: 550px;",
                               pickerInput(
                                 inputId = "select_villes",
                                 label = "",
                                 #color = "success",
                                 #style = "stretch",
                                 choices = c("Villes", "ALI MENDJLI", "BOUGHEZOUL","BOUINANE", 
                                             "DRAA ERICH","SIDI ABDELLAH","EL MENEAA")
                                 ,choicesOpt = list(
                                   style = c("font-size: 114%;font-weight: bold;"))
                                 #             "color: firebrick; text-align: right;", "font-weight: bold;", 
                                 #             "background: forestgreen; color: white;")
                                 #   )
                                 # 
                               ),
                               column(width=9,style="box-shadow:4px 5px 11px grey;background-color:white;",
                                      shinycssloaders::withSpinner(
                                        leafletOutput("leaflet_ville",height="536px")
                                      )
                               ),
                               column(width=3,
                                      style="height: 534px;background-color:white;box-shadow: 4px 5px 11px grey;width: 23%;margin-left: 24px"
                               )
                      ),
                      fluidRow(style="height: 250px;background-color:white;box-shadow: 4px 5px 11px grey;")
               )
             )
    ),
    
    
    # 
    # tabPanel("Bilan2020",
    #          htmlOutput("bilan")
    #          ),
    # 
    
    
    tabPanel("Equipements Publics",
             fluidRow(
               #id="tabequip",
               
               column(5,     #id="cequip",
                      reactableOutput("equip_reactable")
                      
               ),
               column(7,style="box-shadow:4px 5px 11px grey;background-color:white;",
                      column(10,
                             tags$div(class="chart-title-eqp",HTML('&nbsp;'),HTML('&nbsp;'),tags$p("Nombre de Projets Par Etablissement : "),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("wilayaselecteqp"),
                                      actionBttn(
                                        inputId = "tabmodalserie_eqp",
                                        label = NULL,
                                        style = "bordered", 
                                        color = "success",
                                        icon = icon("table")
                                      )
                             ),
                      highchartOutput("equip_highchart",height = "800px",width = "100%")
                      ),
               column(2,style="padding-top:16px",
                      tags$table(class="tae",style="font-size: 21px;font-family: system-ui;",
                                 tags$tr(class="f0eTotal",
                                         tags$td(class="f1eTotal","Total",style="    padding-bottom: 4px;padding-left: 0px;font-family: 'Roboto';font-weight: bold;")
                                 ),
                                 tags$tr(class="f0e",
                                         tags$td(class="f1e",textOutput("ep1"))
                                 ),
                                 tags$tr(class="f0e",
                                         tags$td(class="f1e",textOutput("ep2"))
                                 ),
                                 tags$tr(class="f0e",
                                         tags$td(class="f1e",textOutput("ep3"))
                                 ),
                                 tags$tr(class="f0e",
                                         tags$td(class="f1e",textOutput("ep4"))
                                 ),
                                 tags$tr(class="f0e",
                                         tags$td(class="f1e",textOutput("ep5"))
                                 ),
                                 tags$tr(class="f0e",
                                         tags$td(class="f1e",textOutput("ep6"))
                                 ),
                                 tags$tr(class="f0e",
                                         tags$td(class="f1e",textOutput("ep7"))
                                 ),
                                 tags$tr(class="f0e",
                                         tags$td(class="f1e",textOutput("ep8"))
                                 ),
                                 tags$tr(class="f0e",
                                         tags$td(class="f1e",textOutput("ep9"))
                                 ),
                                 tags$tr(class="f0e",
                                         tags$td(class="f1e",textOutput("ep10"))
                                 ),
                                 tags$tr(class="f0eTotal",
                                         tags$td(class="f1eTotal",textOutput("eptotal"),style="    margin-top: 0px;padding-left: 0px;font-family: 'Roboto';   font-weight: bold;font-size: 25px;text-align: end;")
                                 )
                     )
                )
             
               ),
               
               bsModal("modal_eqp", "Table de donnees", "tabmodalserie_eqp", size = "large"
                       ,excelOutput("excel_eqp")),
    
               )
    ),
    navbarMenu("Données",
               tabPanel("Données de logements ",
                        
                        fluidRow(id="rowdonnees1",style="padding-top: 25px",
                                 column(2,offset = 2,style="padding-top: 22px;",
                                        pickerInput(
                                          inputId = "selectwilayas",
                                          label = "", 
                                          choices = unique(livraison_wilayas$waw),
                                          options = list(`actions-box` = TRUE,style = "btn-primary",
                                                         `selected-text-format`= "count>2",
                                                         `count-selected-text` = "{0} Wilayas séléctionnés",
                                                         `none-selected-text` ="Wilaya",`select-all-text`=
                                                           tags$div(
                                                             "Séléctionner", 
                                                             tags$br(),
                                                             "Tout"
                                                           ),
                                                         `deselect-all-text`=tags$div(
                                                           "Deselectionner", 
                                                           tags$br(),
                                                           "Tout"
                                                         )),
                                          multiple = TRUE
                                        )
                                        
                                        
                                 ),
                                 column(2,style="padding-top: 22px;",
                                        pickerInput(
                                          inputId = "selectsegments",
                                          label = "", 
                                          choices = unique(livraison_wilayas$type_de_logement),
                                          options = list(`actions-box` = TRUE,style = "btn-primary",
                                                         `selected-text-format`= "count>2",
                                                         `count-selected-text` = "{0} Segment séléctionnés",
                                                         `none-selected-text` ="Segment",`select-all-text`=
                                                           tags$div(
                                                             "Séléctionner", 
                                                             tags$br(),
                                                             "Tout"
                                                           ),
                                                         `deselect-all-text`=tags$div(
                                                           "Deselectionner", 
                                                           tags$br(),
                                                           "Tout"
                                                         )),
                                          multiple = TRUE
                                        )
                                 ),
                                 column(4,
                                        sliderInput("selectannees","",
                                                    min = 2000, max = 2019, value = c(2000,2019),sep = "")
                                        
                                 )
                        ),
                        fluidRow(id="rowdonnes2",
                                 column(2,offset = 3,
                                        prettySwitch(
                                          inputId = "parwilayas",
                                          label = "Par Wilaya", 
                                          status = "primary",
                                          slim = TRUE,
                                          value=FALSE
                                        )
                                        
                                 ),
                                 column(2,style="padding-left:310px;",
                                        prettySwitch(
                                          inputId = "parsegments",
                                          label = "Par Segment", 
                                          status = "primary",
                                          slim = TRUE,
                                          value=TRUE
                                        )
                                 ),
                                 column(2,style="padding-left:310px;",
                                        prettySwitch(
                                          inputId = "parperiode",
                                          label = "Par Annee", 
                                          status = "primary",
                                          slim = TRUE,
                                          value=FALSE
                                        )
                                 )
                        ),
                        fluidRow(id="rowdonnes33",style="margin-top:35px;",
                                 htmlOutput('rowdonnees33html')
                        ),
                        fluidRow(
                          
                          excelOutput("donnees_excel",height="100%")
                          
                        )
                        
               ),
               tabPanel("Données des Wilaya",
                        
                        fluidRow(id="rowdonnees1",style="padding-top: 25px",
                                 column(2,offset = 3,style="padding-top: 22px;",
                                        pickerInput(
                                          inputId = "selectwilayas_wilaya",
                                          label = "", 
                                          choices = unique(livraison_wilayas$waw),
                                          options = list(`actions-box` = TRUE,style = "btn-primary",
                                                         `selected-text-format`= "count>2",
                                                         `count-selected-text` = "{0} Wilayas séléctionnés",
                                                         `none-selected-text` ="Wilaya",`select-all-text`=
                                                           tags$div(
                                                             "Séléctionner", 
                                                             tags$br(),
                                                             "Tout"
                                                           ),
                                                         `deselect-all-text`=tags$div(
                                                           "Deselectionner", 
                                                           tags$br(),
                                                           "Tout"
                                                         )),
                                          multiple = TRUE
                                        )
                                        
                                        
                                 ),
                                 
                                 column(4,
                                        sliderInput("selectannees_wilaya","",
                                                    min = 2000, max = 2019, value = c(2000,2019),sep = "")
                                        
                                 )
                        ),
                        fluidRow(id="rowdonnes2",style="margin-left: -6px;margin-bottom:30px; ",
                                 column(2,offset = 5,
                                        prettySwitch(
                                          inputId = "parwilayas_wilaya",
                                          label = "Par Wilayas", 
                                          status = "primary",
                                          slim = TRUE,
                                          value=FALSE
                                        )
                                        
                                 )),
                        fluidRow(id="rowdonnes332",style="margin-top:35px;",
                                 htmlOutput('rowdonnees33html2')
                        ),
                        fluidRow(
                          
                          excelOutput("donnees_excel_wilaya",height="100%")
                          
                        )
                        
               ))
    
  ))
# Define server logic required to draw a histogram
server <- function(input, output) {

    
  # output$bilan <- renderUI({
  #   includeHTML(
  #     rmarkdown::render("bilan.Rmd", params = list(data4 = "01-ADRAR"))
  #   )
  # })
  # 
  # 
  
  
  #  abad=reactive({
  #   ifelse(substr(as.character(input$check),12,15)=="TRUE",1,0)
  #  })
  
  
  # var vaz=$('#segments').val();
  # if(vaz.includes('LSP')==true ) {
  #   var dab =document.getElementsByClassName('highcharts-data-label');
  #   dab[2].children[0].style.fontSize='10px';
  # }
  #
  
  
  # document.getElementsByClassName('highcharts-data-label').children[0].style.fontSize="0px"
  # var df=document.getElementsByClassName('highcharts-data-label')
  # df[2].children[0].lastElementChild.textContent="4645"   #(325 458)
  
  # df[2].children[0].style.fontSize="0px"           # LSP 325 458 to 0px
  
  
  # var arr=document.getElementsByClassName('highcharts-data-label-connector')
  # arr[1].style.stroke="#ffffff"             for arrow
  

  
#   threevent_table_de_donnees=reactive({
#     list(input$annees,input$segments,input$wilayas)
#   })
#   
#   observeEvent(threevent_table_de_donnees(),{
#   runjs(paste0("
#         
# var ann=document.getElementById('annees');
# var ann0=ann.value.split(';').join(' au ');
# 
# var wila=document.querySelector('div#titre_serie.shiny-text-output.shiny-bound-output');
# var wila0=wila.textContent;
# 
# var seg=document.querySelector('div#titre_serie3.shiny-text-output.shiny-bound-output');
# var seg0=seg.textContent;
# 
# 
# 
# var abaz=document.getElementById('modal1');
# var a0a=abaz.children[0].children[0].children[0].children[1];
# 
# a0a.innerHTML='<span style=",'"font-size:25px;vertical-align:-37%;">Table de donnees</span> <br> <span style=','"font-size:13px;">Pour : </span> <span style=','"font-size:16px;">',"' + seg0 + ' - </span>';
#         ")
#   )
#      })
#   
#   
  
  hchart12_data=reactive({
    estimation_tolpopparc %>%
      filter(waw %in% wilaya_reactive(),
      Annee>= min(input$annees),Annee <= max(input$annees) ) %>% 
      group_by(Annee) %>% 
      summarise(Population=sum(Population),Parc_logement=sum(Parc_logement)) %>% 
      select(Annee,Population,Parc_logement) %>%
      mutate(TOL=round(Population/Parc_logement,2)) %>% 
      mutate(Population=round(Population),Parc_logement=round(Parc_logement))
  })
  
  
  
  hchart12m_data=reactive({
    estimation_tolpopparc %>%
      filter(waw %in% wilaya_reactive(),
             Annee <= max(input$annees) ) %>% 
      group_by(Annee) %>% 
      summarise(Population=sum(Population),Parc_logement=sum(Parc_logement)) %>% 
      select(Annee,Population,Parc_logement) %>%
      mutate(TOL=round(Population/Parc_logement,2)) %>% 
      mutate(Population=round(Population),Parc_logement=round(Parc_logement))
  })
  
  
  output$excel_eqp<-renderExcel({
    excelTable(editable = FALSE,showToolbar = TRUE,
               
               equip11,
               columns = data.frame(width=c(250,400,300,250,250,250)),
               
               #columns = data.frame(title=rep("",ncol(sitphy2))),
               #mergeCells = list(A1=c(1,2),B1=c(9,1),K1=c(9,1),T1=c(9,1),AC1=c(9,1),AL1=c(9,1)   ),
               columnSorting=FALSE,search=TRUE
    )
  })
  
  
  selected_eqp <- reactive(
    `if`(length(getReactableState("equip_reactable", "selected"))==0,1:48,getReactableState("equip_reactable", "selected"))
  )
  
  
  output$wilayaselecteqp<-renderText({
    `if`(length(selected_eqp())==48,
         print(""),
         print(data_equip$Wilaya[selected_eqp()]))
    
      })
  
  
  ssw1<-reactive({
    `if`(length(input$selectwilayas) %in% c(0,48),paste0("Tout les Wilayas"),`if`(length(input$selectwilayas)>4,paste0("Les ",length(input$selectwilayas)," ","Wilayas selectionnees"),paste(input$selectwilayas,collapse = "+")))
  })
  
  output$rowdonnees33html<-renderUI(
    HTML(paste(
      '<span style="font-size:11px;">Donnees Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ssw1(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(length(segments_reactive2())==5,paste('Tout les Segments'),paste(segments_reactive2(),collapse = '+')),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$selectannees)!=max(input$selectannees),paste(min(input$selectannees),' au ',max(input$selectannees)),paste('Annee :',max(input$selectannees)) ),'</span>'
    ))    
  )
  
  
  ssw2<-reactive({
    `if`(length(input$selectwilayas_wilaya) %in% c(0,48),paste0("Tout les Wilayas"),`if`(length(input$selectwilayas_wilaya)>4,paste0("Les ",length(input$selectwilayas_wilaya)," ","Wilayas selectionnees"),paste(input$selectwilayas_wilaya,collapse = "+")))
  })
  

  output$rowdonnees33html2<-renderUI(
    HTML(paste(
      '<span style="font-size:11px;">Donnees Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ssw2(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$selectannees_wilaya)!=max(input$selectannees_wilaya),paste(min(input$selectannees_wilaya),' au ',max(input$selectannees_wilaya)),paste('Annee :',max(input$selectannees_wilaya)) ),'</span>'
    ))    
  )
  
  
  
  ttw1<-reactive({
    `if`(length(input$wilayas) %in% c(0,48),paste0("Tout les Wilayas"),`if`(length(input$wilayas)>4,paste0("Les ",length(input$wilayas)," ","Wilayas selectionnees"),paste(input$wilayas,collapse = "+")))
  })
  
  
  output$tablededonnes1<-renderUI(
    HTML(paste(
    '<span style="font-size:25px;vertical-align:-25%;">Table de donnees</span> <br> <span style="font-size:10px;">Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ttw1(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(length(segments_reactive())==5,paste('Tout les Segments'),paste(segments_reactive(),collapse = ',')),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$annees)!=max(input$annees),paste(min(input$annees),' au ',max(input$annees)),paste('Annee :',max(input$annees)) ),'</span>'
         ))
    
  )
  
  
  output$tablededonnes2<-renderUI(
    HTML(paste(
      '<span style="font-size:25px;vertical-align:-25%;">Table de donnees</span> <br> <span style="font-size:10px;">Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ttw1(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$annees)!=max(input$annees),paste(min(input$annees),' au ',max(input$annees)),paste('Annee :',max(input$annees)) ),'</span>'
    ))
  )
  
  
  output$tablededonnes3<-renderUI(
    HTML(paste(
      '<span style="font-size:25px;vertical-align:-25%;">Table de donnees</span> <br> <span style="font-size:12px;">Livraisons pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',`if`(length(segments_reactive())==5,paste('Tout les Segments'),paste(segments_reactive(),collapse = '+')),'- &nbsp;',`if`(min(input$annees)!=max(input$annees),paste(min(input$annees),' au ',max(input$annees)),paste('Annee :',max(input$annees)) ),'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp; (Population,   Parc logements,   TOL) : Annee',max(input$annees),'</span>'
    ))
  )
  
  
  observeEvent(input$annees, {
    # Run JS code that simply shows a message
    runjs("
    var pa = document.getElementById('annees');
    var ipa = document.getElementsByClassName('state p-primary');
    var cheka=document.getElementById('radio_choose_line11');

    abv=pa.value.split(';') ;
    if(abv[0]==abv[1] || cheka.checked==false){
    ipa[0].style.display='none'
    } else {
    ipa[0].style.display=''
    }
          ")
  })
  
  
  observeEvent(input$radio_choose_line1, {
         runjs("
        var chek=document.getElementById('radio_choose_line11')
        var ipa = document.getElementsByClassName('state p-primary');
  
        if(chek.checked==false){
          ipa[0].style.display='none'
              } else {
          ipa[0].style.display=''
                      }
                        ")
         
  })
  
  
  output$titre_hchart1<-renderText({
    `if`(input$radio_choose_line1=="TOL",
         paste0('TOL Par Annees'),
    `if`(min(input$annees)!=max(input$annees),
    paste("Livraisons de logements par Annees : "),
    paste("Livraisons de logements de l'annee ",max(input$annees)," :")
    )
    )
  })
  
  
  output$titre_map<-renderText({
    `if`(input$radio_choose_leaflet=="Livraisons de Logements",
    paste0("Livraisons de logements par Wilaya : "),paste0(input$radio_choose_leaflet," Par Wilaya :")
    )
  })
  
  onevent("mouseenter", "choose_leaflet", show("display_when_hover_choose_leaflet"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_choose_leaflet", show("display_when_hover_choose_leaflet"))
  onevent("mouseleave", "display_when_hover_choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  
  
  
  onevent("mouseenter", "choose_line1", show("display_when_hover_choose_line1"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_choose_line1", show("display_when_hover_choose_line1"))
  onevent("mouseleave", "display_when_hover_choose_line1", hide("display_when_hover_choose_line1"))
  
  
  
  
  selected_equip <- reactive(
    `if`(length(getReactableState("equip_reactable", "selected"))==0,1:48,getReactableState("equip_reactable", "selected"))
  )

  equ=reactive({
    equip %>%
      filter(as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip()) %>% 
      group_by(Secteur) %>%
      summarise("Acheves"=sum(Acheves),"En Cours"=sum(`En Cours`),"Non Lances"=sum(`Non Lances`)
      ) %>% 
      gather("Cas","Nb",2:4) %>% 
      arrange(desc(Cas))
  })
  
  equ_p=reactive({
    equip %>%
      filter(as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip()) %>% 
      group_by(Secteur) %>%
      summarise(pach=sum(Acheves)/sum(`Nbre de Projets`),penc=sum(`En Cours`)/sum(`Nbre de Projets`),
                pnonl=sum(`Non Lances`)/sum(`Nbre de Projets`)
      ) %>% 
      gather("Cas","Nb_p",2:4) %>% 
      arrange(desc(Cas))
  })
  
  equ0p=reactive({
    cbind(equ(),p=round(100*equ_p()$Nb_p,0))  
  })
  
  output$ep1=renderText({
    print((equip %>% filter(as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip(),Secteur==unique(equ_p()$Secteur)[1]) %>%
             summarise(ad=sum(`Nbre de Projets`)) %>% select(ad))$ad)
    })

  output$ep2=renderText({
    
    print((equip %>% filter(as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip(),Secteur==unique(equ_p()$Secteur)[2]) %>%
             summarise(ad=sum(`Nbre de Projets`)) %>% select(ad))$ad)
    })
  
  output$ep3=renderText({
    
    print((equip %>% filter(as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip(),Secteur==unique(equ_p()$Secteur)[3]) %>%
             summarise(ad=sum(`Nbre de Projets`)) %>% select(ad))$ad)
  })
  
  output$ep4=renderText({
    
    print((equip %>% filter(as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip(),Secteur==unique(equ_p()$Secteur)[4]) %>%
             summarise(ad=sum(`Nbre de Projets`)) %>% select(ad))$ad)
  })
  
  
  output$ep5=renderText({
    
    print((equip %>% filter(as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip(),Secteur==unique(equ_p()$Secteur)[5]) %>%
             summarise(ad=sum(`Nbre de Projets`)) %>% select(ad))$ad)
  })
  
  
  output$ep6=renderText({
    
    print((equip %>% filter(as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip(),Secteur==unique(equ_p()$Secteur)[6]) %>%
             summarise(ad=sum(`Nbre de Projets`)) %>% select(ad))$ad)
  })
  
  
  output$ep7=renderText({
    
    print((equip %>% filter(as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip(),Secteur==unique(equ_p()$Secteur)[7]) %>%
             summarise(ad=sum(`Nbre de Projets`)) %>% select(ad))$ad)
  })
  
  
  output$ep8=renderText({
    
    print((equip %>% filter(as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip(),Secteur==unique(equ_p()$Secteur)[8]) %>%
             summarise(ad=sum(`Nbre de Projets`)) %>% select(ad))$ad)
  })
  
  output$ep9=renderText({
    
    print((equip %>% filter(as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip(),Secteur==unique(equ_p()$Secteur)[9]) %>%
             summarise(ad=sum(`Nbre de Projets`)) %>% select(ad))$ad)
  })
  
  
  output$ep10=renderText({
    
    
    print((equip %>% filter(as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip(),Secteur==unique(equ_p()$Secteur)[10]) %>%
             summarise(ad=sum(`Nbre de Projets`)) %>% select(ad))$ad)
  })
  

  output$eptotal=renderText({
    
    print((equip %>% filter(as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip()) %>%
             summarise(ad=sum(`Nbre de Projets`)) %>% select(ad))$ad)
  })
  
  
  output$equip_highchart <-renderHighchart({
    hchart(
      equ0p(), 
      "bar",
      hcaes(x = Secteur, y = Nb, group = Cas),
      stacking = "percent",
      pointWidth =43,
      color=c("#5cb85c","#f0ad4e","#d9534f ")
      
      #color = c("#7CB5EC", "#F7A35C"),
      #name = c("Year 1999", "Year 2008"),
      #showInLegend = c(TRUE, FALSE) # only show the first one in the legend
    ) %>%
      hc_plotOptions(
        series = list(
          showInLegend = TRUE,
          pointFormat = "{point.p}",
          dataLabels=list(style=list(fontSize= "14px",fontWeight="normal",color="white"),format="{point.Nb} ({point.p}%)",enabled=TRUE)
        )) %>%
      
      hc_tooltip(
        crosshairs=TRUE,
        backgroundColor = "#F0F0F0",
        borderColor="#212121",
        shared = TRUE, 
        borderWidth = 3,
        sort=TRUE,
        formatter=JS(paste0("function() {
        var s = '<p>'+this.points[0].key+'</p>',
            sum = 0;

           var sortedPoints = this.points.sort(function(a, b){
                 return ((a.y > b.y) ? -1 : ((a.y < b.y) ? 1 : 0));
             });
             
             
           $.each(sortedPoints , function(i, point) {
            s += '<br/>'+'<span",paste0('style="color:'),paste0("'"),paste0('+ point.series.color +'),paste0("'"),paste0(';"')    ,"> \u25CF </span>' + point.series.name +': '+
                '<b>'+ point.y +'<b/>';
            sum += point.y;
            });

        s += '<br/> <b> Total </b>: '+ sum

        return s;
    }")),
        style=list(
          fontSize="13px"),
        title=list(style=list(fontSize="16px"))
      ) %>% 
      hc_xAxis(
        title=list(text = ""),
        labels=list(style=list(fontSize= "15px",align="left"))
      ) %>% 
      hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')+'%'
		}")),title=list(text = ""),
               reversedStacks=FALSE
               
      )
  }) 
  
  output$equip_reactable <- renderReactable({
    
    reactable(data_equip,
              defaultPageSize = 48, striped = TRUE,borderless = TRUE,
              height="800px",  selection = "single",
              rowStyle =list(cursor = "pointer"),
              onClick = "select",
              columns = list(
                Wilaya=colDef(width = 136,footer="Total"),
                `Nbre de Projets`=colDef(footer=sum(data_equip$`Nbre de Projets`)),
                Acheves=colDef(footer=sum(data_equip$Acheves)),
                `En Cours`=colDef(footer=sum(data_equip$`En Cours`)),
                `Non Lances`=colDef(footer=sum(data_equip$`Non Lances`))
                
              ),
              columnGroups = list(
                colGroup(name = paste("Nombre de Projets Par Wilaya"), columns = colnames(data_equip),headerStyle=list(fontWeight='100',fontFamily='Roboto'))
              ),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#9fc693", boxShadow = "inset 2px 0 0 0 #ffa62d")
              ),
              
              details = function(index) {
                plant_data <- equip11[equip11$Wilaya == data_equip$Wilaya[index], ]
                htmltools::div(style = "padding: 1px",borderless = TRUE,compact=TRUE,
                               reactable(plant_data[,-1], outlined = TRUE,style=list(fontSize="13px"),
                                         defaultColDef = colDef(width=120,footerStyle = list(fontWeight = "bold")),
                                         
                               )
                )
              }
    )
    
    })
  
  
  
  
  
  
  output$excel_sitphy<-renderExcel({
    excelTable(editable = FALSE,showToolbar = TRUE,
               
               sitphy2[,-1],
               
               #columns = data.frame(title=rep("",ncol(sitphy2))),
               #mergeCells = list(A1=c(1,2),B1=c(9,1),K1=c(9,1),T1=c(9,1),AC1=c(9,1),AL1=c(9,1)   ),
               columnSorting=FALSE,search=TRUE
    )
  })
  
  select_villes_reactive<-reactive({
    `if`(input$select_villes=="Villes",data.frame(setview_lat=33.994278,setview_long=2.905987,zoom=6),data_ville[data_ville$Nom==input$select_villes,c(8,7,4,6,5)])
  })
  
  output$leaflet_ville<-renderLeaflet({
    leaflet() %>% 
      addProviderTiles(`if`(input$select_villes=="Villes",providers[[6]],providers[[1]]),group = `if`(input$select_villes=="Villes",providers[[6]],providers[[1]])) %>%
      setView(lat=select_villes_reactive()$setview_lat,lng=select_villes_reactive()$setview_long,zoom=select_villes_reactive()$zoom) %>%    ## in default page we set maxzoom=14  in providers[[44]]  
      addProviderTiles(`if`(input$select_villes=="Villes",providers[[1]],providers[[6]]),group=`if`(input$select_villes=="Villes",providers[[1]],providers[[6]])) %>% 
      addProviderTiles(providers[[57]],group="Satellite") %>% 
      addLayersControl(
        baseGroups = c(`if`(input$select_villes=="Villes",providers[[6]],providers[[1]]),`if`(input$select_villes=="Villes",providers[[1]],providers[[6]]),"Satellite")
      ) %>% 
      addMeasure(
        position = "bottomright",
        primaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479",
        localization="fr",
        thousandsSep=" "
      ) %>% 
      addAwesomeMarkers(lat=data_ville$cor_lat[1],lng=data_ville$cor_long[1],label=data_ville$Nom[1],
                        labelOptions = labelOptions(noHide = TRUE,textsize="14px",
                                                    direction = "right",offset = c(8,-33),opacity = 0.95)
      ) %>% 
      
      addAwesomeMarkers(lat=data_ville$cor_lat[2],lng=data_ville$cor_long[2],label=data_ville$Nom[2],
                        labelOptions = labelOptions(noHide = TRUE,textsize="14px",
                                                    direction = "left",offset = c(-10,-40),opacity = 0.95)
      ) %>% 
      addAwesomeMarkers(lat=data_ville$cor_lat[3],lng=data_ville$cor_long[3],label=data_ville$Nom[3],
                        labelOptions = labelOptions(noHide = TRUE,textsize="14px",
                                                    direction = "bottom",offset = c(0,0),opacity = 0.95)
      ) %>% 
      addAwesomeMarkers(lat=data_ville$cor_lat[4],lng=data_ville$cor_long[4],label=data_ville$Nom[4],
                        labelOptions = labelOptions(noHide = TRUE,textsize="14px",
                                                    direction = "top",offset = c(0,-35),opacity = 0.95)
      ) %>% 
      addAwesomeMarkers(lat=data_ville$cor_lat[5],lng=data_ville$cor_long[5],label=data_ville$Nom[5],
                        labelOptions = labelOptions(noHide = TRUE,textsize="14px",
                                                    direction = "bottom",opacity=0.95) #,offset = c(-10,-40),opacity = 0.8)
      ) %>% 
      addAwesomeMarkers(lat=data_ville$cor_lat[6],lng=data_ville$cor_long[6],label=data_ville$Nom[6],
                        labelOptions = labelOptions(noHide = TRUE,textsize="14px",
                                                    direction = "top",offset = c(0,-35),opacity = 0.95)
      )
  })
  
  output$val_livraison_gauge1<-renderText({
    paste(format(sitphy %>% filter(id %in% selected20(),`Type de logements` %in% select_segment()) %>% summarise(sum(Livraison))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  
  output$val_prevision_gauge1<-renderText({
    paste(format(sitphy %>% filter(id %in% selected20(),`Type de logements` %in% select_segment()) %>% summarise(sum(Prevision))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  #####
  
  output$val_livraison_gauge2<-renderText({
    paste(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="LPL") %>% summarise(sum(Livraison))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  
  output$val_prevision_gauge2<-renderText({
    paste(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="LPL") %>% summarise(sum(Prevision))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  #####
  
  
  output$val_livraison_gauge3<-renderText({
    paste(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="LSP") %>% summarise(sum(Livraison))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  
  output$val_prevision_gauge3<-renderText({
    paste(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="LSP") %>% summarise(sum(Prevision))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  #####
  
  
  output$val_livraison_gauge4<-renderText({
    paste(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="Rural") %>% summarise(sum(Livraison))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  
  output$val_prevision_gauge4<-renderText({
    paste(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="Rural") %>% summarise(sum(Prevision))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  #####
  
  
  output$val_livraison_gauge5<-renderText({
    paste(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="Location-Vente") %>% summarise(sum(Livraison))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  
  output$val_prevision_gauge5<-renderText({
    paste(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="Location-Vente") %>% summarise(sum(Prevision))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  #####
  
  
  output$val_livraison_gauge6<-renderText({
    paste(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="LPP") %>% summarise(sum(Livraison))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  
  output$val_prevision_gauge6<-renderText({
    paste(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="LPP") %>% summarise(sum(Prevision))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  #####

    
  observe({
    `if`(length(input$select_segment_gauge) %in% c(0,2,3,4,5),
         js$opac1('1'),
         js$opac1('0.13')
    )
    
    `if`(length(which(input$select_segment_gauge=="LPL"))!=0 | length(input$select_segment_gauge) %in% c(0,5),
         js$opac2('1'),
         js$opac2('0.13')
    )
    
    
    `if`(length(which(input$select_segment_gauge=="LSP"))!=0 | length(input$select_segment_gauge) %in% c(0,5),
         js$opac3('1'),
         js$opac3('0.13')
    )
    
    `if`(length(which(input$select_segment_gauge=="Rural"))!=0 | length(input$select_segment_gauge) %in% c(0,5),
         js$opac4('1'),
         js$opac4('0.13')
    )
    
    `if`(length(which(input$select_segment_gauge=="Location-Vente"))!=0 | length(input$select_segment_gauge) %in% c(0,5),
         js$opac5('1'),
         js$opac5('0.13')
    )
    
    
    `if`(length(which(input$select_segment_gauge=="LPP"))!=0 | length(input$select_segment_gauge) %in% c(0,5),
         js$opac6('1'),
         js$opac6('0.13')
    )
    
  })
  
  output$toutsegments_gauge<-renderText({
    `if`(length(input$select_segment_gauge) %in% c(0,1,5),
         print("Tout les Segments"),
         paste(input$select_segment_gauge,collapse="+")
    )
  })
  
  
  output$wilayaselectgauge1<-renderText({
    `if`(length(selected20())==48,"",sitphy$Wilaya_matricule[selected20()])
  })
  
  output$wilayaselectgauge2<-renderText({
    `if`(length(selected20())==48,"",sitphy$Wilaya_matricule[selected20()])
  })
  
  
  output$wilayaselectgauge3<-renderText({
    `if`(length(selected20())==48,"",sitphy$Wilaya_matricule[selected20()])
  })
  
  
  output$wilayaselectgauge4<-renderText({
    `if`(length(selected20())==48,"",sitphy$Wilaya_matricule[selected20()])
  })
  
  
  output$wilayaselectgauge5<-renderText({
    `if`(length(selected20())==48,"",sitphy$Wilaya_matricule[selected20()])
  })
  
  output$wilayaselectgauge6<-renderText({
    `if`(length(selected20())==48,"",sitphy$Wilaya_matricule[selected20()])
  })
  
  output$gauge6=renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge",height='88%') %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data=as.numeric(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="LPP") %>% summarise(sum(Livraison)/sum(Prevision))*100,scientific = FALSE,digits = 0)),
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          format="<span style='font-size:40px;font-family:inherit;'>{y}</span><span style='font-size:36px;font-family:inherit;opacity:1'>%</span>",
          style = list(fontSize = "40px"  )
        )
      )
  })
  
  
  
  output$gauge5=renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge",height='88%') %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data=as.numeric(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="Location-Vente") %>% summarise(sum(Livraison)/sum(Prevision))*100,scientific = FALSE,digits = 0)),
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          format="<span style='font-size:40px;font-family:inherit;'>{y}</span><span style='font-size:36px;font-family:inherit;opacity:1'>%</span>",
          style = list(fontSize = "40px"  )
        )
      )
  })
  
  
  output$gauge4=renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge",height='88%') %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data=as.numeric(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="Rural") %>% summarise(sum(Livraison)/sum(Prevision))*100,scientific = FALSE,digits = 0)),
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          format="<span style='font-size:40px;font-family:inherit;'>{y}</span><span style='font-size:36px;font-family:inherit;opacity:1'>%</span>",
          style = list(fontSize = "40px"  )
        )
      )
  })
  
  
  
  output$gauge3=renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge",height='88%') %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data=as.numeric(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="LSP") %>% summarise(sum(Livraison)/sum(Prevision))*100,scientific = FALSE,digits = 0)),
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          format="<span style='font-size:40px;font-family:inherit;'>{y}</span><span style='font-size:36px;font-family:inherit;opacity:1'>%</span>",
          style = list(fontSize = "40px"  )
        )
      )
  })
  
  
  output$gauge2=renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge",height='88%') %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data=as.numeric(format(sitphy %>% filter(id %in% selected20(),`Type de logements`=="LPL") %>% summarise(sum(Livraison)/sum(Prevision))*100,scientific = FALSE,digits = 0)),
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          format="<span style='font-size:40px;font-family:inherit;'>{y}</span><span style='font-size:36px;font-family:inherit;opacity:1'>%</span>",
          style = list(fontSize = "40px"  )
        )
        
      )
  })
  
  
  output$gauge1=renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge",height='88%') %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        #data = round(100*sum(sitphy$Livraison[sitphy$id==selected20() & sitphy$`Type de logements` %in% select_segment()])/sum(sitphy$Prevision[sitphy$id==selected20()])),
        data=as.numeric(format(sitphy %>% filter(id %in% selected20(),`Type de logements` %in% select_segment()) %>% summarise(sum(Livraison)/sum(Prevision))*100,scientific = FALSE,digits = 0)),
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          format="<span style='font-size:40px;font-family:inherit;'>{y}</span><span style='font-size:36px;font-family:inherit;opacity:1'>%</span>",
          style = list(fontSize = "40px"  )
        )
      )
  })
  
  select_segment=reactive({
    `if`(length(input$select_segment_gauge)==0,c("Rural","LPL","Location-Vente","LSP","LPP"),input$select_segment_gauge)
  })
  
  
  select_segment_title=reactive({
    `if`(length(input$select_segment_gauge) %in% c(0,5),"",input$select_segment_gauge)
  })
  
  selected2 <- reactive(getReactableState("table2", "selected"))
  
  selected20 <- reactive(
    `if`(length(getReactableState("table2", "selected"))==0,1:48,getReactableState("table2", "selected"))
  )
  
  output$wgauge=renderText({
    print(selected20())
  })
  
  sitphy00=reactive({
    sitphy %>% 
      filter(`Type de logements` %in% select_segment()) %>% 
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
      select(Wilaya,Consistance,`Achevés`,`En Cours`,`Non Lancés`) %>% 
      replace_na(list(`Achevés`=0,`En Cours`=0,`Non Lancés`=0))
    
  })
  
  output$table2 <- renderReactable({
    reactable(sitphy00(),defaultPageSize = 48,striped = TRUE,
              selection = "single",
              borderless = TRUE,
              height="800px",
              width="66%",
              columns = list(
                Wilaya = colDef(width = 148,align="left",footer="Total"),   # 50% width, 200px minimum
                Consistance=colDef(format=colFormat(digits = 0,separators = TRUE),width = 104,align="center",footer=format(sum(sitphy00()$Consistance),trim=TRUE,digits = 3,big.mark = " ")),
                `Achevés`=colDef(format=colFormat(percent = TRUE,digits = 0),width = 80,align="center",footer=sprintf("%3.0f %%",100*sum(sitphy00()$Consistance*sitphy00()$Achevés)/sum(sitphy00()$Consistance)),
                                  style = function(value) {
                                    color<-green_pal(value)
                                    list(background=color)
                                  }
                ),
                `En Cours`=colDef(format=colFormat(percent = TRUE,digits = 0),width = 85,align="center",footer=sprintf("%3.0f %%",100*sum(sitphy00()$Consistance*sitphy00()$`En Cours`)/sum(sitphy00()$Consistance)),
                                  style = function(value) {
                                    color<-blue_pal(value)
                                    list(background=color)
                                  }),
                `Non Lancés`=colDef(format=colFormat(percent = TRUE,digits = 0),width = 99,align="center",footer=sprintf("%3.0f %%",100*sum(sitphy00()$Consistance*sitphy00()$`Non Lancés`)/sum(sitphy00()$Consistance)),
                                     style = function(value) {
                                       color<-red_pal(value)
                                       list(background=color)
                                     }
                )
              ),
              columnGroups = list(
                colGroup(name = paste("Situation Physiques des logements"," ",paste(select_segment_title(),collapse = "+")), columns = colnames(sitphy00()))
              ),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              onClick = "select",
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#9fc693", boxShadow = "inset 2px 0 0 0 #ffa62d")
              )
    )
  })
  
  output$selected2 <- renderPrint({
    print(selected2())
  })
  
  observe({
    print(sitphy0[selected2(), ])
  })
  
  #################################
  output$wilayaselect1<-renderText({
    `if`(length(selected())==48,
         print(""),
         print(zones$Wilaya[selected()]))
  })
  
  
  output$wilayaselect2<-renderText({
    `if`(length(selected())==48,
         print(""),
         print(zones$Wilaya[selected()]))
  })
  
  
  output$ttwilayas=renderText({
    `if`(length(selected())==48,
         print("Toutes les Wilayas"),
         print(zones$Wilaya[selected()])
    )
  })
  
  
  output$ttwilayas2=renderText({
    `if`(length(selected())==48,
         print("Toutes les Wilayas"),
         print(zones$Wilaya[selected()])
    )
  })
  
  output$region=renderText({
    `if`(length(selected())==48,
         print("Région : HAUT PLATEU - SUD"),
         paste0("Région : ",zones$Zone[selected()])
    )
  })
  output$zones1=renderText({
    `if`(length(selected())==48,
         print("Domanial"),
         `if`(is.na((zones %>% filter(id_wilaya==selected()) %>% 
                       select(`Nature juridique`))$`Nature juridique`
         )==TRUE,print(c()),
         (zones %>% filter(id_wilaya==selected()) %>% 
            select(`Nature juridique`))$`Nature juridique`)
    )
  })
  
  
  output$zones2=renderText({
    `if`(
      (zones %>% filter(id_wilaya==selected()) %>% 
         summarise(sumnb=sum(`Nombre de communes concernées`,na.rm = TRUE)))$sumnb==0,print(""),
      
      format((zones %>% filter(id_wilaya==selected()) %>% 
                summarise(sumnb=sum(`Nombre de communes concernées`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
    )
  })
  
  
  output$zones3=renderText({
    `if`((zones %>% filter(id_wilaya==selected()) %>% 
            summarise(sumnb=sum(`Superficie (ha)`,na.rm = TRUE)))$sumnb==0,print(""),
         format((zones %>% filter(id_wilaya==selected()) %>% 
                   summarise(sumnb=sum(`Superficie (ha)`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
    )
  })
  
  output$zones4=renderText({
    `if`((zones %>% filter(id_wilaya==selected()) %>% 
            summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
         format((zones %>% filter(id_wilaya==selected()) %>% 
                   summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
         
    )
  })
  
  output$zones5=renderText({
    `if`((zones %>% filter(id_wilaya==selected()) %>% 
            summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
         format((zones %>% filter(id_wilaya==selected()) %>% 
                   summarise(sumnb=sum(`Nombre de lots retenues`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
         
    )
  })
  
  output$zones6=renderText({
    `if`(
      (zones %>% filter(id_wilaya==selected()) %>% 
         summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
      format((zones %>% filter(id_wilaya==selected()) %>% 
                summarise(sumnb=sum(`Nombre de lots dégagés (Porte feuille)`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
      
    )
  })
  
  output$zones7=renderText({
    
    (zones %>% filter(id_wilaya==selected()) %>%
       select(`Surface moyenne des lots (m²)`))$`Surface moyenne des lots (m²)`
    
  })
  
  output$zones8=renderText({
    `if`(
      (zones %>% filter(id_wilaya==selected()) %>% 
         summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
      format((zones %>% filter(id_wilaya==selected()) %>% 
                summarise(sumnb=sum(`Nombre de sites`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
      
    )
  })
  
  output$zones9=renderText({
    `if`((zones %>% filter(id_wilaya==selected()) %>% 
            summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
         format((zones %>% filter(id_wilaya==selected()) %>% 
                   summarise(sumnb=sum(`Nombre des permis`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
         
    )
  })
  
  
  
  display_zones7=renderText({
    `if`(length(selected())==48,
         paste0("display:none;"),paste0("display:block;")
    )
  })
  
  
  output$loi1=renderText({
    format((zones %>% filter(id_wilaya %in% selected()) %>% 
              summarise(sumnb=sum(Déposés,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi2=renderText({
    format((zones %>% filter(id_wilaya %in% selected()) %>% 
              summarise(sumnb=sum(Traités,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi3=renderText({
    format((zones %>% filter(id_wilaya %in% selected()) %>% 
              summarise(sumnb=sum(Favorables,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi4=renderText({
    format((zones %>% filter(id_wilaya %in% selected()) %>% 
              summarise(sumnb=sum(Défavorables,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi5=renderText({
    format((zones %>% filter(id_wilaya %in% selected()) %>% 
              summarise(sumnb=sum(Instances,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi6=renderText({
    format((zones %>% filter(id_wilaya %in% selected()) %>% 
              summarise(sumnb=sum(`Déposés Instruction N°01`,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi7=renderText({
    format((zones %>% filter(id_wilaya %in% selected()) %>% 
              summarise(sumnb=sum(Traités_a,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$urbanisme2<-renderHighchart({
    sit_fin %>% 
      filter(id %in% selected()) %>% 
      group_by(Type) %>% 
      summarise(Notification=sum(NOTIFICATION),Reliquat=sum(Reliquat)) %>% 
      gather("etat","nb",2:3) %>% arrange(etat) %>% 
      hchart('column',hcaes(x=Type,y=nb,group=etat),stacking="normal") %>% 
      hc_tooltip(
        crosshairs=TRUE,
        backgroundColor = "#F0F0F0",
        borderColor="#212121",
        shared = TRUE, 
        borderWidth = 3,
        sort=TRUE,
        formatter=JS(paste0("function() {
        var s = '<p>'+this.points[0].key+'</p>',
            sum = 0;

           var sortedPoints = this.points.sort(function(a, b){
                 return ((a.y > b.y) ? -1 : ((a.y < b.y) ? 1 : 0));
             });
             
             
           $.each(sortedPoints , function(i, point) {
            s += '<br/>'+'<span",paste0('style="color:'),paste0("'"),paste0('+ point.series.color +'),paste0("'"),paste0(';"')    ,"> \u25CF </span>' + point.series.name +': '+
                '<b>'+ Highcharts.numberFormat(point.y,0) +' DA'+'<b/>';
            sum += point.y;
            });

        s += '<br/> <b> Inscription : '+ Highcharts.numberFormat(sum,0) + ' DA </b>' 

        return s;
    }")),
        style=list(
          fontSize="15px"),
        title=list(style=list(fontSize="16px"))
      ) %>% hc_xAxis(
        title=list(text = ""),
        labels=list(style=list(fontSize= "15px",fontWeight="normal"))
        
      ) %>%
      hc_yAxis(
        labels=list(style=list(fontSize= "15px",fontWeight="normal"),
                    formatter=JS('function() {
			if ( this.value > 1000 ) return Highcharts.numberFormat(this.value/1000000000,0)+" Milliard DA";
			return Highcharts.numberFormat(this.value, 0, "."," ");
		}')),
        title=list(text = ""),
        reversedStacks=FALSE
        
      ) %>% hc_legend(
        
        # 
        align= 'left',
        # layout="vertical",
        # verticalAlign= 'middle',
        # itemMarginTop= 6,
        # itemMarginBottom=6,
        margin=-60,
        itemStyle=list(fontSize="15px",fontWeight= 300)
      )
    
  })
  
  tpos_reactive=reactive({
    pos %>%
      filter(id_wilaya %in% selected()) %>% 
      group_by(URBANISME) %>%
      summarise(lances=sum(Lancés),"Non Lancées"=sum(`Non Lancées`),Achevées=sum(Achevées),"En Cours"=sum(`En Cours`),approuvees=sum(Approuvées)) %>% 
      gather("etat","nb",2:5) %>% filter(etat %in% c("Non Lancées","Achevées","En Cours"))
  })
  
  
  output$urbanisme1<-renderHighchart({
    tpos_reactive() %>% 
      hchart(
        'bar', hcaes(x = URBANISME, y = nb, group = etat),
        borderColor="#404040"
      )%>%
      hc_colors(c("#77c663", "#EFC000FF","#e71919")) %>% 
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#F0F0F0",
        borderColor="#212121",
        shared = TRUE, 
        borderWidth = 3,
        sort=TRUE,
        formatter=JS(paste0("function() {
        var s = '<p>'+this.points[0].key+'</p>',
            sum = 0;

           var sortedPoints = this.points.sort(function(a, b){
                 return ((a.y > b.y) ? -1 : ((a.y < b.y) ? 1 : 0));
             });
             
             
           $.each(sortedPoints , function(i, point) {
            s += '<br/>'+'<span",paste0('style="color:'),paste0("'"),paste0('+ point.series.color +'),paste0("'"),paste0(';"')    ,"> \u25CF </span>' + point.series.name +': '+
                '<b>'+ point.y +'<b/>';
            sum += point.y;
            });

        s += '<br/> <b> Total </b>: '+ sum

        return s;
    }")),
        style=list(
          fontSize="18px")
      ) %>% 
      hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', '')
		}")),title=list(text = "")
      )%>%
      hc_xAxis(
        title=list(text = ""),
        labels=list(style=list(fontSize= "16px",fontWeight="normal"))
        
      ) %>% 
      hc_plotOptions(
        series = list(
          showInLegend = TRUE,
          pointFormat = "{point.nb}",
          dataLabels=list(style=list(fontSize= "14px",fontWeight="normal"),format="{point.nb}",enabled=TRUE)
        )
      ) %>% hc_legend(
        itemStyle=list(fontSize="15px",fontWeight= 300)
      )
    
  })
  
  selected <- reactive(
    `if`(length(getReactableState("table", "selected"))==0,1:48,getReactableState("table", "selected"))
  )
  
  output$table <- renderReactable({
    reactable(data.frame(Wilaya=unique(livraison_wilayas$waw)),defaultPageSize = 48,striped = TRUE,
              selection = "single",
              borderless = TRUE,
              sortable = FALSE,
              onClick = "select",
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#9fc693", boxShadow = "inset 2px 0 0 0 #ffa62d")
              )
    )
  })
  
  output$selected <- renderPrint({
    print(selected())
  })
  
  observe({
    print(data.frame(unique(livraison_wilayas$waw))[selected(), ])
  })
  
  output$dt_wilaya=renderDataTable({
    unique(livraison_wilayas$waw)
  })
  
  segments_reactive2=reactive({
    if(length(input$selectsegments)==0) {
      unique(livraison_wilayas0()$type_de_logement)
    } else{
      input$selectsegments
    }
  })
  
  
  
  wilaya_reactive2=reactive({
    if(length(input$selectwilayas)==0) {
      unique(livraison_wilayas0()$waw)
    } else{
      input$selectwilayas
    }  
  })
  
  livraison_wilayas0_donnees=reactive({
    livraison_wilayas0()%>%
      select(waw,type_de_logement,Livraison,annee) %>% 
      filter(waw %in% wilaya_reactive2(),type_de_logement %in% segments_reactive2(),annee>= min(input$selectannees),annee <= max(input$selectannees)) %>% 
      rename(Wilaya=waw,Segment=type_de_logement,Annee=annee)
    
  })
  
  output$donnees_excel<-renderExcel({
    `if`(input$parwilayas==FALSE & input$parsegments==FALSE & input$parperiode==TRUE,
         excelTable(editable = FALSE,
                    rbind(c("Année","Livraisons"),livraison_wilayas0_donnees() %>% 
                            group_by(Annee) %>% 
                            summarise(Livraisons=sum(Livraison)) %>%
                            rbind(c("Total",sum(round(livraison_wilayas0_donnees() %>% summarise(t=sum(Livraison)) %>% select(t))$t) )))
                    ,showToolbar = TRUE,autoFill = TRUE,
                    columns = data.frame(title=c("",""))
                    
         ),
         `if`(input$parwilayas==FALSE & input$parsegments==FALSE & input$parperiode==FALSE,
              (
                excelTable(editable = FALSE,
                           livraison_wilayas0_donnees() %>% 
                             summarise(Livraisons=sum(Livraison))
                           ,showToolbar = TRUE,columns = data.frame(title=c("Livraisons"))
                ) 
              ),
              
              `if`(input$parwilayas==FALSE & input$parsegments==TRUE & input$parperiode==TRUE,
                   excelTable(editable = FALSE,
                              rbind(colnames(livraison_wilayas0_donnees() %>%
                                               group_by(Annee,Segment) %>% 
                                               summarise(Livraisons=sum(Livraison)) %>% 
                                               spread(key=Segment,value=Livraisons) %>% 
                                               mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                              ),
                              livraison_wilayas0_donnees() %>%
                                group_by(Annee,Segment) %>% 
                                summarise(Livraisons=sum(Livraison)) %>% 
                                spread(key=Segment,value=Livraisons) %>% 
                                mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                              ),showToolbar = TRUE,
                              columns = data.frame(title=rep("",length(livraison_wilayas0_donnees() %>%
                                                                         group_by(Annee,Segment) %>% 
                                                                         summarise(Livraisons=sum(Livraison)) %>% 
                                                                         spread(key=Segment,value=Livraisons) %>% 
                                                                         mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                              )))
                   ),
                   `if`(input$parwilayas==FALSE & input$parsegments==TRUE & input$parperiode==FALSE,
                        excelTable(editable = FALSE, 
                                   rbind(c("Segment","Livraisons"),
                                         livraison_wilayas0_donnees() %>%
                                           group_by(Segment) %>% 
                                           summarise(Livraisons=sum(Livraison)) %>% 
                                           rbind(c("Total",round(livraison_wilayas0_donnees() %>% summarise(ta=sum(Livraison)) %>% select(ta))$ta))
                                   ),showToolbar = TRUE,columns = data.frame(title=c("",""))
                        ),
                        `if`(input$parwilayas==TRUE & input$parsegments==FALSE & input$parperiode==TRUE,
                             excelTable(editable = FALSE,
                                        rbind(colnames(livraison_wilayas0_donnees() %>%
                                                         group_by(Annee,Wilaya) %>% 
                                                         summarise(Livraisons=sum(Livraison)) %>% 
                                                         spread(key = Annee,value=Livraisons) %>% 
                                                         rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>%
                                                         rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                        ),
                                        livraison_wilayas0_donnees() %>%
                                          group_by(Annee,Wilaya) %>% 
                                          summarise(Livraisons=sum(Livraison)) %>% 
                                          spread(key = Annee,value=Livraisons) %>% 
                                          rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>%
                                          rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                        )
                                        ,showToolbar = TRUE,columns = data.frame(title=rep("",ncol(  livraison_wilayas0_donnees() %>%
                                                                                                       group_by(Annee,Wilaya) %>% 
                                                                                                       summarise(Livraisons=sum(Livraison)) %>% 
                                                                                                       spread(key = Annee,value=Livraisons) %>% 
                                                                                                       rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>%
                                                                                                       rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                        )))
                             ),
                             `if`(input$parwilayas==TRUE & input$parsegments==FALSE & input$parperiode==FALSE,
                                  excelTable(editable = FALSE,
                                             rbind(c("Wilaya","Livraisons"),
                                                   livraison_wilayas0_donnees() %>%
                                                     group_by(Wilaya) %>% 
                                                     summarise(Livraisons=sum(Livraison)) %>% 
                                                     rbind(c("Total",sum(round(livraison_wilayas0_donnees() %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                             )
                                             ,showToolbar = TRUE,columns = data.frame(title=c("",""))
                                  ),
                                  `if`(input$parwilayas==TRUE & input$parsegments==TRUE & input$parperiode==TRUE,
                                       excelTable(editable = FALSE,
                                                  rbind(colnames(livraison_wilayas0_donnees() %>%
                                                                   group_by(Annee,Wilaya,Segment,)%>% 
                                                                   summarise(Livraisons=sum(Livraison)) %>% 
                                                                   spread(Segment,value = Livraisons) %>% 
                                                                   mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c(" ","Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                                  ),
                                                  livraison_wilayas0_donnees() %>%
                                                    group_by(Annee,Wilaya,Segment,)%>% 
                                                    summarise(Livraisons=sum(Livraison)) %>% 
                                                    spread(Segment,value = Livraisons) %>% 
                                                    mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c(" ","Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                                  )
                                                  ,showToolbar = TRUE,columns = data.frame(title=rep("",ncol(livraison_wilayas0_donnees() %>%
                                                                                                               group_by(Annee,Wilaya,Segment,)%>% 
                                                                                                               summarise(Livraisons=sum(Livraison)) %>% 
                                                                                                               spread(Segment,value = Livraisons) %>% 
                                                                                                               mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c(" ","Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                                  )))
                                       ),
                                       excelTable(editable = FALSE,
                                                  rbind(colnames(livraison_wilayas0_donnees() %>%
                                                                   group_by(Wilaya,Segment,)%>% 
                                                                   summarise(Livraisons=sum(Livraison)) %>% 
                                                                   spread(Segment,value = Livraisons) %>% 
                                                                   rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                                  ),
                                                  livraison_wilayas0_donnees() %>%
                                                    group_by(Wilaya,Segment,)%>% 
                                                    summarise(Livraisons=sum(Livraison)) %>% 
                                                    spread(Segment,value = Livraisons) %>% 
                                                    rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                                  )
                                                  ,showToolbar = TRUE,columns = data.frame(title=rep("",ncol(livraison_wilayas0_donnees() %>%
                                                                                                               group_by(Wilaya,Segment,)%>% 
                                                                                                               summarise(Livraisons=sum(Livraison)) %>% 
                                                                                                               spread(Segment,value = Livraisons) %>% 
                                                                                                               rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                                  )))
                                       )
                                  )
                             )
                             
                             
                        )
                   )
              )
         )
    )
    
  })
  
  
  
  wilaya_reactive2_estimation=reactive({
    if(length(input$selectwilayas_wilaya)==0) {
      unique(estimation_tolpopparc0()$waw)
    } else{
      input$selectwilayas_wilaya
    }
  })
  
  estimation_tolpopparc0_donnees=reactive({
    estimation_tolpopparc0()%>%
      select(Annee,waw,Population,TOL,Parc_logement) %>%
      mutate(Population=round(Population),Parc_logement=round(Parc_logement)) %>% rowwise() %>% mutate(TOL=round(sum(Population)/sum(Parc_logement),2)) %>% 
      filter(waw %in% wilaya_reactive2_estimation(),Annee>= min(input$selectannees_wilaya),Annee <= max(input$selectannees_wilaya)) %>% 
      rename(Wilaya=waw)
  })
  
  output$donnees_excel_wilaya<-renderExcel({
    `if`(input$parwilayas_wilaya==FALSE,
         excelTable(editable = FALSE,
                    rbind(c("Année","Population","Parc Logement","TOL"),
                          estimation_tolpopparc0_donnees() %>% 
                            select(Annee,Wilaya,Population,Parc_logement,TOL) %>% 
                            group_by(Annee) %>% 
                            summarise(Population=sum(Population),'Parc Logement'=sum(Parc_logement),TOL=round(sum(Population)/sum(Parc_logement),2) )
                    ),showToolbar = TRUE,autoFill = TRUE,columns = data.frame(title=rep("",4))
         ),
         `if`(max(input$selectannees_wilaya)==min(input$selectannees_wilaya),
              excelTable(editable = FALSE,
                         rbind(c("Année","Wilaya","Population","Parc Logement","TOL"),
                               estimation_tolpopparc0_donnees() %>% 
                                 select(Annee,Wilaya,Population,TOL,Parc_logement) %>% 
                                 rbind(c("","Total",round(sum(estimation_tolpopparc0_donnees()$Population)  ),round(sum(estimation_tolpopparc0_donnees()$Population)/sum(estimation_tolpopparc0_donnees()$Parc_logement),2),round(sum(estimation_tolpopparc0_donnees()$Parc_logement)  ) )) %>% 
                                 select(Annee,Wilaya,Population,Parc_logement,TOL) %>% 
                                 rename('Parc Logement'=Parc_logement)
                         )
                         
                         ,showToolbar = TRUE,autoFill = TRUE,columns = data.frame(title=rep("",5))
              ),
              excelTable(editable = FALSE,
                         rbind(c("Année","Wilaya","Population","Parc Logement","TOL"),
                               
                               estimation_tolpopparc0_donnees() %>% 
                                 select(Annee,Wilaya,Population,Parc_logement,TOL) %>% 
                                 rename('Parc Logement'=Parc_logement)
                         )
                         ,showToolbar = TRUE,autoFill = TRUE,columns = data.frame(title=rep("",5))
              )
         ))
  })
  
  
  
  livraison_wilayas0=reactive({
    read_excel(paste0(getwd(),"/livraison_wilayas.xlsx")) %>% 
      filter(type_de_logement %in% c( "LPL","Rural","LPP","LSP","Location-Vente"))
  })
  
  estimation_tolpopparc0=reactive({
    read_excel(paste0(getwd(),"/Estimation_Population_TOL_Parc_par_Wilaya.xlsx"))
  })
  
  output$periode<-renderText({
    `if`(min(input$annees)!=max(input$annees),paste0("Periode : ",min(input$annees),"-",max(input$annees)),paste0("Annee : ",min(input$annees)) )
  })
  
  
 output$periode2<-renderText({
    `if`(input$radio_choose_leaflet=="Livraisons de Logements" & min(input$annees)!=max(input$annees),paste0("Periode : ",min(input$annees),"-",max(input$annees)),paste0("Annee : ",max(input$annees))
         )
    })
  
  
  output$excel3<-renderExcel({
    excelTable(editable = FALSE,
               rbind(c("Wilaya","Livraisons","Surface","Population","Parc Logement","TOL","Daira","Commune"),
                     data.frame(livraison_wilayas0()%>%
                                  filter(annee>= min(input$annees),annee <= max(input$annees),type_de_logement %in% segments_reactive())%>%
                                  group_by(waw)%>%
                                  summarise(Livraisons=sum(Livraison))%>%
                                  add_column(Surface=unique(livraison_wilayas0()$Surface)
                                             ,Population=round(estimation_tolpopparc0()$Population[estimation_tolpopparc0()$Annee==max(input$annees)])
                                             ,"Parc Logement"=round(estimation_tolpopparc0()$Parc_logement[estimation_tolpopparc0()$Annee==max(input$annees)])
                                             ,TOL=round(estimation_tolpopparc0()$Population[estimation_tolpopparc0()$Annee==max(input$annees)]/estimation_tolpopparc0()$Parc_logement[estimation_tolpopparc0()$Annee==max(input$annees)],2)
                                             ,Daira=livraison_wilayas0()$Daira[seq(1,4800,20*5)]
                                             ,Commune=livraison_wilayas0()$Commune[seq(1,4800,20*5)]
                                             
                                  ) %>% 
                                  rename(Wilaya=waw)
                     ) ),columns = data.frame(title=rep("",8)),showToolbar = TRUE
    )
  })
  
  output$excel1<-renderExcel({
    excelTable(editable = FALSE,
               rbind(c(colnames(daa2()%>%
                                  spread(key=type_de_logement,value = liv) %>% rename("Année"=annee) ),"Total"),
                     data.frame(daa2()%>%
                                  spread(key=type_de_logement,value = liv) %>% 
                                  mutate(annee=as.character(annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(daa2() %>% group_by(type_de_logement) %>% summarise(t=sum(liv)) %>% select(t))$t,sum(round(daa2() %>% group_by(type_de_logement) %>% summarise(t=sum(liv)) %>% select(t))$t) ))
                     )
               ),showToolbar = TRUE,columns = data.frame(title=rep("",1+ncol(data.frame(data.frame(daa2()%>%
                                                                                                     spread(key=type_de_logement,value = liv))))))
    )
  })
  
  
  output$excel2<-renderExcel({
    excelTable(editable = FALSE,
               rbind(c("Segment","Livraisons","Pourcentage %"),
                     daa()%>%
                       select(label,value,pr) %>% 
                       rename(Segment=label,Livraisons=value,"Pourcentage %"=pr)
               ),columns = data.frame(title=rep("",3)),showToolbar = TRUE
    )
  })
  output$excel_urbanisme1<-renderExcel({
    excelTable(
      data=rbind(c("Wilaya","POS","POS","POS","PDAU","PDAU","EGU","EGU","EGU"),
                 colnames(pos5),pos5),
      columns = data.frame(title=rep("",9)
                           ,width=c(200,200,200,200,200,200,200,200,200))
      ,mergeCells = list(A1=c(1,2),B1=c(3,1),E1=c(2,1),G1=c(3,1)),showToolbar = TRUE,editable = FALSE
      ,columnSorting=FALSE)
  })
  
  
  
  
  output$excel_urbanisme2<-renderExcel({
    excelTable(search=TRUE,
      showToolbar = TRUE,
      data=rbind(colnames(sit_fin[,2:6]),sit_fin[,2:6] %>% mutate_if(is.numeric,format,scientific=FALSE)),
      #nestedHeaders = list( data.frame(title=c("","Amélioration urbaine","VRD PRIMAIRES ET SECONDAIRES 2010 -2019","VRD TERTIAIRES D'HRG 2012- 2013 -2019","VRD LOTISSEMENTS SOCIAUX 2012 -2013 -2015 -2018 -2019"),colspan=c(1,3, 3, 3,3))),
      columns = data.frame(title=rep("",ncol(sit_fin)-1) )
      #"Wilaya","Notification","Inscription","Reliquat","Notification","Inscription","Reliquat","Notification","Inscription","Reliquat","Notification","Inscription","Reliquat")
      #                                                       ,width=c(200,200,200,200,200,200,200,200,200,200,200,200))
      ,editable = FALSE
    )
  })
  
  
  output$excel_urbanisme3<-renderExcel({
    excelTable(
      data=rbind(colnames(zones00)[2:12],
                 zones00[,2:12]),
      editable=FALSE,showToolbar = TRUE,
      columns = data.frame(title=rep("",11))
    )
    
  })
  
  
  output$excel_urbanisme4<-renderExcel({
    excelTable(
      data=rbind(c("Wilaya",colnames(zones00)[13:ncol(zones00)]),
                 zones00[,c(2,13:ncol(zones00))]),
      editable=FALSE,showToolbar = TRUE,
      columns = data.frame(title=rep("",8))
    )
    
  })
  
  
  output$densite<-renderText({
    
    format((round(estimation_tolpopparc0()%>%
                    filter(Annee == max(input$annees),waw %in% wilaya_reactive())%>%
                    summarise(pop=sum(Population))%>%
                    select(pop))$pop)/(round(livraison_wilayas0()%>%
                                               select(Surface)%>%
                                               unique()%>%
                                               mutate(wilaya=unique(livraison_wilayas0()$waw))%>%
                                               filter(wilaya %in% wilaya_reactive())%>%
                                               summarise(sumsurface=sum(Surface))%>%
                                               select(sumsurface),2)$sumsurface),
           big.mark = " ",trim=TRUE,digits = 3
    )
    
  })
  
  
  
  output$max_an<-renderText({
    max(input$annees)
  })
  
  output$titre_serie3<-renderText({
    `if`(length(segments_reactive())==5 | input$radio_choose_leaflet!="Livraisons de Logements",paste0(""),paste(segments_reactive(),collapse = " + "))
  })
  
  
  
  
  output$titre_serie1_hchart1<-renderText({
    `if`(input$radio_choose_line1=="TOL",paste0(""),
    `if`(input$Id027==FALSE & max(input$annees)!=min(input$annees),paste0(segments_reactive(),collapse = "+"),paste0(""))
    )
    })
  
  output$titre_serie2<-renderText({
    `if`(length(input$wilayas) %in% c(0,48),paste0(""),`if`(length(input$wilayas)>2,paste0("Pour les ",length(input$wilayas)," ","Wilayas selectionnees"),paste(input$wilayas,collapse = " + ")))
  })
  
  
  output$titre_serie<-renderText({
    `if`(length(input$wilayas) %in% c(0,48),paste0(""),`if`(length(input$wilayas)>2,paste0("Pour les ",length(input$wilayas)," ","Wilayas selectionnees"),paste(input$wilayas,collapse = "+")))
  })
  
  #output$tdaa3<-renderTable({
  #  livraison_wilayas0()
  #})
  
  output$dernier_an<-renderText({                    #Livraisons
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'annee ",min(input$annees)),paste0("par rapport a l'annee ",min(input$annees)-1))
  })
  
  
  output$dernier_an2<-renderText({                       #Parc logement
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'annee ",min(input$annees)),paste0("par rapport a l'annee ",min(input$annees)-1))
  })
  
  
  output$dernier_an3<-renderText({        # POPULATION
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'annee ",min(input$annees)),paste0("par rapport a l'annee ",min(input$annees)-1))
  })
  
  
  output$dernier_an4<-renderText({                     #TOL
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'annee ",min(input$annees)),paste0("par rapport a l'annee ",min(input$annees)-1))
  })
  
  output$dernier_an8<-renderText({                     #densite
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'annee ",min(input$annees)),paste0("par rapport a l'annee ",min(input$annees)-1))
  })
  
  
  
  output$taux_livraisons<-renderText({
    ag=`if`(min(input$annees)!=max(input$annees),
            sprintf("%+3.1f %%",100*(
              
              (
                round(livraison_wilayas0()%>%
                        filter(annee <= max(input$annees),type_de_logement %in% segments_reactive(),waw %in% wilaya_reactive())%>%
                        summarise(liva2=sum(Livraison))%>%
                        select(liva2))$liva2)
              -(
                round(livraison_wilayas0()%>%
                        filter(annee<=min(input$annees),type_de_logement %in% segments_reactive(),waw %in% wilaya_reactive())%>%
                        summarise(liva2=sum(Livraison))%>%
                        select(liva2))$liva2)
            )/
              (
                round(livraison_wilayas0()%>%
                        filter(annee<=min(input$annees),type_de_logement %in% segments_reactive(),waw %in% wilaya_reactive())%>%
                        summarise(liva2=sum(Livraison))%>%
                        select(liva2))$liva2)
            )
            ,
            
            sprintf("%+3.1f %%",100*
                      ((round(livraison_wilayas0()%>%
                                filter(annee==(max(input$annees)),type_de_logement %in% segments_reactive(),waw %in% wilaya_reactive())%>%
                                summarise(liva2=sum(Livraison))%>%
                                select(liva2))$liva2)-(round(livraison_wilayas0()%>%
                                                               filter(annee==(min(input$annees)-1),type_de_logement %in% segments_reactive(),waw %in% wilaya_reactive())%>%
                                                               summarise(liva2=sum(Livraison))%>%
                                                               select(liva2))$liva2))/(round(livraison_wilayas0()%>%
                                                                                               filter(annee==(min(input$annees)-1),type_de_logement %in% segments_reactive(),waw %in% wilaya_reactive())%>%
                                                                                               summarise(liva2=sum(Livraison))%>%
                                                                                               select(liva2))$liva2)
            )
    )
    paste0('<p style=color:',ifelse(as.numeric(substr(ag,1,nchar(ag)-2))>0,'green','red'),';>',ag,'</p>')
  })
  
  
  
  output$titre_livraison<-renderText({
    `if`(min(input$annees)!=max(input$annees),
         paste0("(",min(input$annees)," au ",max(input$annees),")"),
         paste0("(en ",min(input$annees),")")
    )
  })
  
  
  output$max_annee_population<-renderText({
    
    paste0("Population en ",max(input$annees))
  })
  
  
  output$max_annee_parclogement<-renderText({
    
    paste0("Parc Logements en ",max(input$annees))
  })
  
  
  
  output$max_annee_densite<-renderText({
    
    paste0("Densite en ",max(input$annees))
  })
  
  output$max_annee_tol<-renderText({
    
    paste0("TOL en ",max(input$annees))
  })
  
  
  ##############  Info Box Livraisons #########################
  ##############  Info Box Livraisons #########################
  
  format_reactive_livraisons<-reactive({
    daa()%>%
      filter(label %in% segments_reactive())%>%
      summarize(liva=sum(value))%>%
      mutate(ab=format(liva,big.mark = " ",trim=TRUE))%>%
      select(ab)
  })
  
  output$livraisons<-renderText({
    
    format_reactive_livraisons()$ab
  })
  ##############  Info Box Livraisons #########################
  
  format_reactive_livraisons<-reactive({
    daa()%>%
      filter(label %in% segments_reactive())%>%
      summarize(liva=sum(value))%>%
      mutate(ab=format(liva,big.mark = " ",trim=TRUE))%>%
      select(ab)
  })
  
  output$livraisons<-renderText({
    
    format_reactive_livraisons()$ab
  })
  
  ##############  Info Box Lancement #########################
  ##############  Info Box Lancement #########################
  
  ##############  Info Box Lancement #########################
  
  
  
  ##############  Info Box Parc logement #########################
  ##############  Info Box Parc logement #########################
  
  
  format_reactive_parclogements<-reactive({
    estimation_tolpopparc0()%>%
      filter(waw %in% wilaya_reactive(),Annee %in% c(min(input$annees),max(input$annees)))%>%
      group_by(Annee)%>%
      summarise(pop=sum(Parc_logement))%>%
      mutate(ab=format(round(pop),big.mark = " ",trim=TRUE))
  })
  
  format_reactive_parclogements_avantan<-reactive({
    estimation_tolpopparc0()%>%
      filter(waw %in% wilaya_reactive(),Annee %in% c(max(input$annees)-1,max(input$annees)))%>%
      group_by(Annee)%>%
      summarise(pop=sum(Parc_logement))%>%
      mutate(ab=format(round(pop),big.mark = " ",trim=TRUE))
  })
  
  
  
  output$parclogements<-renderText({
    format_reactive_parclogements()$ab[length(format_reactive_parclogements()$ab)]
  })
  
  
  output$taux_parclogement<-renderText({
    `if`(min(input$annees)!=max(input$annees),
         sprintf("%+3.1f %%",100*((format_reactive_parclogements()$pop[length(format_reactive_parclogements()$pop)])-(format_reactive_parclogements()$pop[1]))/(format_reactive_parclogements()$pop[1]) )
         ,
         sprintf("%+3.1f %%",100*((format_reactive_parclogements_avantan()$pop[2])-(format_reactive_parclogements_avantan()$pop[1]))/(format_reactive_parclogements()$pop[1]) )
    )
  })
  
  
  output$taux_densite<-renderText({
    `if`(min(input$annees)!=max(input$annees),
         sprintf("%+3.1f %%",100*((format_reactive_population()$pop[length(format_reactive_population()$pop)])-(format_reactive_population()$pop[1]))/(format_reactive_population()$pop[1]) )
         ,
         sprintf("%+3.1f %%",100*((format_reactive_population_avantan()$pop[2])-(format_reactive_population_avantan()$pop[1]))/(format_reactive_population_avantan()$pop[1]) )
    )
         })
  
  
  
  ##############  Info Box Parc logement #########################
  
  
  
  
  
  ##############  Info Box Population #########################
  ##############  Info Box Population #########################
  
  
  format_reactive_population<-reactive({
    estimation_tolpopparc0()%>%
      filter(waw %in% wilaya_reactive(),Annee %in% c(min(input$annees),max(input$annees)))%>%
      group_by(Annee)%>%
      summarize(pop=sum(Population))%>%
      mutate(ab=format(round(pop),big.mark = " ",trim=TRUE))
  })
  
  
  format_reactive_population_avantan<-reactive({
    estimation_tolpopparc0()%>%
      filter(waw %in% wilaya_reactive(),Annee %in% c(max(input$annees)-1,max(input$annees)))%>%
      group_by(Annee)%>%
      summarize(pop=sum(Population))%>%
      mutate(ab=format(round(pop),big.mark = " ",trim=TRUE))
  })
  
  
  output$populations<-renderText({
    format_reactive_population()$ab[length(format_reactive_population()$ab)]
  })
  
  output$taux_population<-renderText({
    `if`(min(input$annees)!=max(input$annees),
         sprintf("%+3.1f %%",100*((format_reactive_population()$pop[length(format_reactive_population()$pop)])-(format_reactive_population()$pop[1]))/(format_reactive_population()$pop[1]) )
         ,
         sprintf("%+3.1f %%",100*((format_reactive_population_avantan()$pop[2])-(format_reactive_population_avantan()$pop[1]))/(format_reactive_population_avantan()$pop[1]) )
    )     
  })
  
  
  ##############  Info Box Population #########################
  
  
  
  ##############  Info Box TOL #########################
  ##############  Info Box TOL #########################
  
  
  
  format_reactive_tol<-reactive({
    estimation_tolpopparc0()%>%
      filter(waw %in% wilaya_reactive(),Annee %in% c(min(input$annees),max(input$annees)))%>%
      group_by(Annee)%>%
      summarize(pop=sum(Population)/sum(Parc_logement))%>%
      mutate(ab=format(pop,big.mark = " ",trim=TRUE,digits = 3))
  })
  
  format_reactive_tol_dernieran<-reactive({
    estimation_tolpopparc0()%>%
      filter(waw %in% wilaya_reactive(),Annee %in% c(max(input$annees)-1,max(input$annees)))%>%
      group_by(Annee)%>%
      summarize(pop=sum(Population)/sum(Parc_logement))%>%
      mutate(ab=format(pop,big.mark = " ",trim=TRUE,digits = 3))
  })
  
  
  output$tols<-renderText({
    format_reactive_tol()$ab[length(format_reactive_tol()$ab)]
  })
  
  output$taux_tol<-renderText({
    `if`(min(input$annees)!=max(input$annees),
         sprintf("%+3.1f %%",100*((format_reactive_tol()$pop[length(format_reactive_tol()$pop)])-(format_reactive_tol()$pop[1]))/(format_reactive_tol()$pop[1]) )
         ,
         sprintf("%+3.1f %%",100*((format_reactive_tol_dernieran()$pop[2])-(format_reactive_tol_dernieran()$pop[1]))/(format_reactive_tol_dernieran()$pop[1]) )
    )
  })
  
  
  
  ##############  Info Box TOL #########################
  
  
  
  
  
  
  wilaya_reactive=reactive({
    if(length(input$wilayas)==0) {
      unique(livraison_wilayas0()$waw)
    } else{
      input$wilayas
    }  
  })
  
  output$table_wilayas_reactive=renderTable({
    wilaya_reactive()
  })
  
  segments_reactive=reactive({
    if(length(input$segments)==0) {
      unique(livraison_wilayas0()$type_de_logement)
    } else{
      input$segments
    }
  })
  
  
  output$table_segments_reactive=renderTable({
    segments_reactive()
  })
  
  daa=reactive({
    livraison_wilayas0()%>%
      filter(
        #type_de_logement %in% segments_reactive(),
        annee>= min(input$annees),annee<=max(input$annees),
        waw %in% wilaya_reactive()  ) %>% #,waw %in% input$wilayas,annee>input$annee[1],annee<input$annee[2])%>%
      
      group_by(type_de_logement)%>%
      summarise(liv=sum(Livraison))%>%
      rename(label=type_de_logement,value=liv)%>%
      #arrange(`if`(length(input$segments) %in% c(0,5),c(4,1,5,3,2),c("") ))%>%
      arrange(c(4,1,5,3,2))%>%
      mutate(pr=round(100*value/sum(value),2),value2=format(value, big.mark=" ", trim=TRUE))
  })
  
  inprea=reactive({
    paste0(input$radio_choose_leaflet)
  })
  
  #
  
  livraison_map<-reactive({
    `if`(input$radio_choose_leaflet == c("TOL"),
      round(estimation_tolpopparc0()%>%
        select(Annee,waw,Population,TOL,Parc_logement) %>%
        filter(Annee == max(input$annees)) %>% 
        mutate(Population=round(Population),Parc_logement=round(Parc_logement),TOL=Population/Parc_logement) %>% 
        rename(Wilaya=waw,"Parc Logements"=Parc_logement) %>% 
        arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
        select(TOL),2)$TOL
      ,
    `if`(input$radio_choose_leaflet == c("Livraisons de Logements"),
    round(livraison_wilayas0()%>%
            filter(annee>= min(input$annees),annee<=max(input$annees),type_de_logement %in% segments_reactive())%>%
            group_by(waw)%>%
            summarise(liv=sum(Livraison))%>%
            arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
            select(liv))$liv,
    `if`(input$radio_choose_leaflet == c("Parc Logements"),
         round(estimation_tolpopparc0()%>%
                 select(Annee,waw,Population,TOL,Parc_logement) %>%
                 filter(Annee == max(input$annees)) %>% 
                 mutate(Population=round(Population),Parc_logement=round(Parc_logement),TOL=Population/Parc_logement) %>% 
                 rename(Wilaya=waw,"Parc Logements"=Parc_logement) %>% 
                 arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                 select(`Parc Logements`),2)$`Parc Logements`
    ,
    round(estimation_tolpopparc0()%>%
            select(Annee,waw,Population,TOL,Parc_logement) %>%
            filter(Annee == max(input$annees)) %>% 
            mutate(Population=round(Population),Parc_logement=round(Parc_logement),TOL=Population/Parc_logement) %>% 
            rename(Wilaya=waw,"Parc Logements"=Parc_logement) %>% 
            arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
            select(Population),2)$Population
    )
    )
    )
    
      })
  
  output$distPlot2<-renderLeaflet({
    mapdz
  })
  
  twoevent=reactive({
    list(input$annees,input$segments,input$radio_choose_leaflet)
  })
  
  
  # | | |  OR operation
  
  popolo=reactive({
  })

  observeEvent(twoevent(),{
    livrason_maps=livraison_map()
    poloo=
      `if`(input$radio_choose_leaflet==c("Livraisons de Logements"),colorNumeric("YlGnBu",livraison_map()),
           `if`(input$radio_choose_leaflet==c("TOL"),colorNumeric("RdYlBu",livraison_map(),reverse=TRUE),
                `if`(input$radio_choose_leaflet==c("Population"),colorNumeric("YlOrBr",livraison_map()),
                     colorNumeric("BuPu",livraison_map())
                )  
           )
      )
    inpr=input$radio_choose_leaflet
    maxaa=max(input$annees)
    leafletProxy('distPlot2',data=algeria)%>%
      clearControls()%>%
      addLegend(
        position = "topright",
        title=`if`(inpr== c("Livraisons de Logements"),"Livraisons",paste0(inpr," (",maxaa,")")),
        pal=poloo,
        opacity = 1,
        values=livrason_maps
      ) 
  }
  )
  
  observeEvent(twoevent(),{
    maxa=max(input$annees)
    mixa=min(input$annees)
    livrason_maps=livraison_map()
    poloo=`if`(input$radio_choose_leaflet==c("Livraisons de Logements"),colorNumeric("YlGnBu",livraison_map()),
               `if`(input$radio_choose_leaflet==c("TOL"),colorNumeric("RdYlBu",livraison_map(),reverse=TRUE),
                    `if`(input$radio_choose_leaflet==c("Population"),colorNumeric("YlOrBr",livraison_map()),
                         colorNumeric("BuPu",livraison_map())
                    )  
               )
    )
    
    leafletProxy('distPlot2',data=algeria)%>%
      
      addPolygons(weight=1,fillColor =poloo(livrason_maps),color ="black",
                  label=sprintf(
                    "<strong style='font-size:16px;'>%s</strong><br/> <p style='font-size:14px;font-weight:normal;display:inline;'> <p style='font-size:15px;font-weight:normal;display:inline;'>Livraisons</p>  <h6 style='font-size:11px;display:inline;'>(%s)</h6>&nbsp;&nbsp;&nbsp;:&nbsp; %s <br/>Surface &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;:&nbsp; %s <br/>Nombre de Daira &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; :&nbsp; %s <br/>Nombre de Communes  :&nbsp; %s <br/>Population <h6 style='font-size:11px;display:inline;'>(%g)</h6> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; :&nbsp; %s<br/>Parc logements <h6 style='font-size:11px;display:inline;'>(%g)</h6>&nbsp;&nbsp;&nbsp; :&nbsp; %s<br/>TOL <h6 style='font-size:11px;display:inline;'>(%g)</h6>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;:&nbsp; %s</p>",
                    algeria@data$wilayas,
                    
                    
                    `if`(mixa!=maxa,
                         paste0(mixa," au ",maxa),
                         paste0("en    ",mixa)
                    ),
                    
                    format(round(livraison_wilayas0()%>%
                                   filter(annee>= mixa,annee<=maxa,type_de_logement %in% segments_reactive())%>%
                                   group_by(id_wilaya)%>%
                                   summarise(liv=sum(Livraison))%>%
                                   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                                   select(liv))$liv,big.mark = " ",trim=TRUE),
                    
                    
                    format(round(livraison_wilayas0()%>%
                                   select(Surface)%>%
                                   unique()%>%
                                   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                                   select(Surface),1)$Surface,trim=TRUE,big.mark=" "),
                    
                    
                    format(round(livraison_wilayas0()%>%
                                   group_by(id_wilaya)%>%
                                   summarise(nbd=min(Daira))%>%
                                   select(nbd)%>%
                                   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                                   select(nbd),1)$nbd,trim=TRUE,big.mark=" "),
                    
                    format(round(livraison_wilayas0()%>%
                                   group_by(id_wilaya)%>%
                                   summarise(nbc=min(Commune))%>%
                                   select(nbc)%>%
                                   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                                   select(nbc),1)$nbc,trim=TRUE,big.mark=" "),
                    
                    maxa,format(round(
                      estimation_tolpopparc0()%>%
                        filter(Annee==maxa)%>%
                        arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                        select(Population))$Population,big.mark = " ",trim=TRUE),
                    maxa,
                    format(round(
                      estimation_tolpopparc0()%>%
                        filter(Annee==maxa)%>%
                        arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                        select(Parc_logement))$Parc_logement,big.mark = " ",trim=TRUE),
                    maxa,
                    format(round(
                      estimation_tolpopparc0()%>%
                        filter(Annee==maxa)%>%
                        mutate(tol1=Population/Parc_logement)%>%
                        arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                        select(tol1),2)$tol1,big.mark = " ",trim=TRUE)
                    
                    
                  ) %>% lapply(htmltools::HTML),
                  fillOpacity = 0.7,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "left",
                    offset = c(-130,100)
                  ),
                  
                  highlight=highlightOptions(
                    weight=5,fillOpacity = 0.7,bringToFront=TRUE
                  ))
    
  })
  
  zoomlevel<-reactive({
    input$distPlot2_zoom
  })
  
  observeEvent(wilaya_reactive(),{
    wi=wilaya_reactive()
    w=as.character(1:48)
    
    `if`(length(wi) %in% c(1:47),
         leafletProxy('distPlot2',data=algeria)%>%
           addAwesomeMarkers(layerId = as.character(algeria@data$id_wilaya[which(algeria@data$wilayas %in% wi)]),
                             lng=algeria@data$longitude[which(algeria@data$wilayas %in% wi)],
                             lat=algeria@data$latitude[which(algeria@data$wilayas %in% wi)],
                             label=algeria@data$nam[which(algeria@data$wilayas %in% wi)],
                             labelOptions=labelOptions(noHide = `if`(length(wi)<6,T,F),textsize = `if`(length(wi) %in% c(4,5) ,"10px","15px"),direction = "bottom",offset = `if`(length(wi)<6,c(0,0),c(-180,-40) ))
           )%>%   
           removeMarker(layerId = as.character(w[-(algeria@data$id_wilaya[which(algeria@data$wilayas %in% wi)])]))
         ,
         
         leafletProxy('distPlot2',data=algeria)%>%
           removeMarker(layerId = w)
    )
  })
  
  
  
  output$pie <- renderHighchart({
    highchart() %>%
      hc_add_series(
        daa(),
        "pie",
        hcaes(
          name = label,
          y = value
        ),
        name = "Livraison"
      )%>%
      hc_chart(type = "pie",
               
               # var arr=document.getElementsByClassName('highcharts-data-label-connector')
               # arr[1].style.stroke="#ffffff"             for arrow
               
               
               
               events=list(load=JS("function() {
               var vaz=$('#segments').val();
               if(vaz.length==1 || vaz.length==2 || vaz.length==3 || vaz.length==4){
                if(vaz.includes('LPL')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[0].children[0].style.fontSize='0px';
                  arr[0].style.stroke='#ffffff';
                }
                if(vaz.includes('Rural')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[1].children[0].style.fontSize='0px';
                  arr[1].style.stroke='#ffffff';
                }
                if(vaz.includes('LSP')==false ) {
                
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[2].children[0].style.fontSize='0px';
                  arr[2].style.stroke='#ffffff';}
                  
                  
                if(vaz.includes('Location-Vente')==false ) {
                  
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[3].children[0].style.fontSize='0px';
                  arr[3].style.stroke='#ffffff';
                }
                if(vaz.includes('LPP')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[4].children[0].style.fontSize='0px';
                  arr[4].style.stroke='#ffffff';
                }
               
               }
                  }
                                   ")
               )
               
               
               #    ,options3d = list(enabled = TRUE
               #                    , beta = 10
               #                   , alpha = 38
               #                  , depth = 400
               #                 , viewDistance = 8)
      )%>%
      #hc_title(text="Répartition de livraisons de logements par Segments")%>%
      #hc_subtitle(text="de l'année 2000-2019")%>%
      hc_plotOptions(
        series = list(
          alignValue="left",
          showInLegend = TRUE,
          pointFormat = "{point.y}%",
          colorByPoint = TRUE,
          size="180px",
          dataLabels=list(style=list(fontSize= "14px",fontWeight="normal"),format="<p>{point.label}: {point.pr:.1f}%<br />( {point.value2} )</p>"),
          #depth=40,
          allowPointSelect=TRUE
        )
      )%>%
      hc_legend(
        align= 'right',
        layout="vertical",
        verticalAlign= 'middle',
        itemMarginTop= 6,
        itemMarginBottom=6,
        itemStyle=list(fontSize="15px"),
        margin=-60
      )%>%
      hc_add_theme(hc_theme_smpl(    #hc_theme_elementary  hc_theme_ffx #hc_theme_flat
        colors=`if`(length(input$segments) %in% c(0,5),
                    c("#d35400","#2ecc71","#2980b9","#f1c40f","#2c3e50"),
                    
                    c(
                      `if`("LPL" %in% input$segments,"#d35400","#bababa"),
                      `if`("Rural" %in% input$segments,"#2ecc71","#bababa"),
                      `if`("LSP" %in% input$segments,"#2980b9","#bababa"),
                      `if`("Location-Vente" %in% input$segments,"#f1c40f","#bababa"),
                      `if`("LPP" %in% input$segments,"#2c3e50","#bababa")
                    )   ),
        chart = list(backgroundColor = "transparent")
        ,        title=list(align="right",
                            style=list(fontSize="14px",fontWeight="normal")),
        subtitle=list(align="center",
                      style=list(fontWeight="normal",fontFamily="Roboto Condensed")))
      )
    
  })
  
  daa2=reactive({
    livraison_wilayas0()%>%
      filter(type_de_logement %in% segments_reactive(),
             annee>= min(input$annees),annee<=max(input$annees),
             waw %in% wilaya_reactive()  ) %>% #,waw %in% input$wilayas,annee>input$annee[1],annee<input$annee[2])%>%
      #filter(type_de_logement %in% c("LSP","Rural","Location-Vente", "LPP","LPL"))%>% #,waw %in% input$wilayas,annee>input$annee[1],annee<input$annee[2])%>%
      group_by(annee,type_de_logement)%>%
      summarise(liv=sum(Livraison))
  })
  
  daa3=reactive({
    daa2()%>%
      group_by(annee)%>%
      summarise(liv2=sum(liv))
    
  })
  
  line_color0<-reactive({
    data.frame(ns=c("Location-Vente","LPL","LPP","LSP","Rural"),couleur=c("#f1c40f","#d35400","#2c3e50","#2980b9","#2ecc71"))
  })
  
  line_color1<-reactive({
    line_color0() %>% filter(ns %in% unique(daa2()$type_de_logement)) %>% select(couleur)
  })
  
  output$hchart1<-renderHighchart({
    
    `if`(input$radio_choose_line1=="TOL",
      `if`(min(input$annees)!=max(input$annees),
         highchart() %>%
  
         hc_xAxis(
             labels=list(style=list(fontSize= "11px",fontWeight="normal"),rotation=0),
             tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      
      if(xMax-xMin>11 && xMax-xMin<15){
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 2 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else if(xMax-xMin>14) {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 4 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
      } 
    }")
           ) %>%
           
      
           hc_yAxis_multiples(
             list(title=list(text="Population"),opposite=FALSE,
                  min=0,max=max(hchart12_data()$Population),
                  labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}"))
             ),
             list(title=list(text="Parc Logements"),opposite=FALSE,
                  min=0,max=max(hchart12_data()$Population)/3,
                  labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}"))),
             list(title=list(text="TOL"),opposite=TRUE,min=0,max=9)
             #create_yaxis(naxis = 4, title = list(text = NULL))
           ) %>%
           hc_add_series(hchart12_data(),type="line",hcaes(x=Annee,y=Population),name="Population") %>%
           hc_add_series(hchart12_data(),type="line",hcaes(x=Annee,y=Parc_logement),yAxis=1,name="Parc Logements") %>%
           hc_add_series(hchart12_data(),type="line",hcaes(x=Annee,y=TOL),yAxis=2,name="TOL") %>% 
           hc_tooltip(
             crosshairs = TRUE,
             backgroundColor = "#F0F0F0",
             borderColor="#212121",
             shared = TRUE, 
             borderWidth = 3,
             sort=TRUE
           ) %>% 
           hc_legend(align= 'right') %>% 
           hc_add_theme(hc_theme_google()),
       
         
         ##### when min an == max an ::::
         
    highchart() %>%
        hc_xAxis(
          labels=list(style=list(fontSize= "10px",fontWeight="normal"),rotation=0),
          tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      
      if(xMax-xMin>11 && xMax-xMin<15){
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 2 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else if(xMax-xMin>14) {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 4 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
      }
      
                       
                      }")
        ) %>%
           hc_yAxis_multiples(
             list(title=list(text="Population"),opposite=FALSE,
                  min=0,max=max(hchart12m_data()$Population),
                  labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}"))
             ),
             list(title=list(text="Parc Logements"),opposite=FALSE,
                  min=0,max=max(hchart12m_data()$Population)/3,
                  labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}"))),
             list(title=list(text="TOL"),opposite=TRUE,min=0,max=9)
             #create_yaxis(naxis = 4, title = list(text = NULL))
           ) %>%
           hc_add_series(hchart12m_data(),type="line",hcaes(x=Annee,y=Population),name="Population") %>%
           hc_add_series(hchart12m_data(),type="line",hcaes(x=Annee,y=Parc_logement),yAxis=1,name="Parc Logements") %>%
           hc_add_series(hchart12m_data(),type="line",hcaes(x=Annee,y=TOL),yAxis=2,name="TOL") %>% 
           hc_tooltip(
             crosshairs = TRUE,
             backgroundColor = "#F0F0F0",
             borderColor="#212121",
             shared = TRUE, 
             borderWidth = 3,
             sort=TRUE
           ) %>% 
           hc_legend(align= 'right') %>% 
           hc_add_theme(hc_theme_google()),
         
         
         
           
      ),
    
    `if`((input$Id027==TRUE) | (min(input$annees)==max(input$annees)),
         `if`(min(input$annees)!=max(input$annees),
              `if`(length(segments_reactive())!=1,
              hchart(daa2(),"line",hcaes(x=annee,y=liv,group=type_de_logement ))%>%
                hc_tooltip(
                  crosshairs = TRUE,
                  backgroundColor = "#F0F0F0",
                  borderColor="#212121",
                  shared = TRUE, 
                  borderWidth = 3,
                  formatter=JS(paste0("function() {
        var s = '<b>'+ this.x +'</b>',
            sum = 0;

           var sortedPoints = this.points.sort(function(a, b){
                 return ((a.y > b.y) ? -1 : ((a.y < b.y) ? 1 : 0));
             });
           $.each(sortedPoints , function(i, point) {
            s += '<br/>'+'<span",paste0('style="color:'),paste0("'"),paste0('+ point.series.color +'),paste0("'"),paste0(';"')    ,"> \u25CF </span>' + point.series.name +': '+
                '<b>'+ Highcharts.numberFormat(point.y,0) +'<b/>';
            sum += point.y;
            });


        s += '<br/> <b> Total </b>: '+ Highcharts.numberFormat(sum,0)
        

        return s;
    }")),sort=TRUE
                )%>%
                hc_chart(
                  backgroundColor = "#ffffff"
                )%>%
                hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}")),title=list(text = "")
                )%>%
                hc_xAxis(
                  title=list(text = ""),
                  tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      
      if(xMax-xMin>11 && xMax-xMin<15){
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 2 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else if(xMax-xMin>14) {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 4 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
      }
      
                       
                      }")
                  
                )%>%
                
                hc_legend(
                  # align = "right",
                  #  verticalAlign = "right",
                  # backgroundColor='transparent',
                  #  borderColor="#000000",
                  # borderWidth=2,
                  #  layout = "vertical",shadow=TRUE,
                  #  x = 17, y = 0
                  itemStyle=list(fontSize="15px",fontWeight= 300)
                )%>%
                hc_add_theme(hc_theme_flat(
                  colors = 
                    #c("#f1c40f", "#2ecc71", "#9b59b6", "#e74c3c","#004182")
                    line_color1()$couleur
                    #c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9")
                  
                    # 
                    # colors=`if`(length(input$segments) %in% c(0,5),
                    #             c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9"),
                    #             
                    #             c(
                    #               `if`("LPL" %in% input$segments,"#d35400"),
                    #               `if`("Rural" %in% input$segments,"#2980b9"),
                    #               `if`("LSP" %in% input$segments,"#2ecc71"),
                    #               `if`("Location-Vente" %in% input$segments,"#f1c40f"),
                    #               `if`("LPP" %in% input$segments,"#2c3e50")
                    #             )   )
                    # 
                    # 
                
                  )),
              hchart(daa2(),"line",hcaes(x=annee,y=liv,group=type_de_logement))%>%
                hc_tooltip(
                  crosshairs = TRUE,
                  backgroundColor = "#F0F0F0",
                  borderColor="#212121",
                  shared = TRUE, 
                  borderWidth = 3
                  ,sort=TRUE
                )%>%
                hc_colors(
                  c(as.character(line_color1()$couleur))
                ) %>%
                hc_chart(
                  backgroundColor = "#ffffff"
                )%>%
                hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}")),title=list(text = "")
                )%>%
                hc_xAxis(
                  title=list(text = ""),
                  tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      
      if(xMax-xMin>11 && xMax-xMin<15){
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 2 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else if(xMax-xMin>14) {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 4 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
      }
      
                       
                      }")
                  
                )%>%
                
                hc_legend(
                  # align = "right",
                  #  verticalAlign = "right",
                  # backgroundColor='transparent',
                  #  borderColor="#000000",
                  # borderWidth=2,
                  #  layout = "vertical",shadow=TRUE,
                  #  x = 17, y = 0
                  itemStyle=list(fontSize="15px",fontWeight= 300)
                )%>%
                hc_add_theme(hc_theme_flat(
                  #colors = 
                    #c("#f1c40f", "#2ecc71", "#9b59b6", "#e74c3c","#004182")
                   # c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9")
                  
                  
                  
                ))
              )
              
              ,
              hchart(daa2()
                     ,"column",colorByPoint=TRUE,hcaes(y=liv,x=type_de_logement))%>% #,color=c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9")))%>%
                hc_tooltip(
                  crosshairs = FALSE,
                  backgroundColor = "#F0F0F0",
                  borderColor="#212121",
                  shared = TRUE, 
                  borderWidth = 3,
                  sort=TRUE,
                  pointFormat='Livraisons: <b>{point.y}</b><br/>'
                  )%>%
                hc_colors(
                  c(as.character(line_color1()$couleur))
                ) %>%
                hc_chart(
                  margin=c(0,0,30,0),
                  backgroundColor = "#ffffff",
                  options3d=list(
                    enabled=TRUE,
                    alpha=15,
                    beta=20,
                    depth=10,
                    viewDistance=110
                  )
                )%>%
                hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}")),title=list(text = "")
                )%>%
                hc_xAxis(
                  title=list(text = ""),
                  labels=list(style=list(fontSize= "13px",fontWeight="normal"),rotation=9)
                )%>%
                
                hc_legend(
                  # align = "right",
                  #  verticalAlign = "right",
                  # backgroundColor='transparent',
                  #  borderColor="#000000",
                  # borderWidth=2,
                  #  layout = "vertical",shadow=TRUE,
                  #  x = 17, y = 0
                  itemStyle=list(fontSize="15px",fontWeight= 300)
                ) %>% 
                hc_add_theme(hc_theme_flat(
                  #colors = 
                    #c("#f1c40f", "#2ecc71", "#9b59b6", "#e74c3c","#004182")
                    #line_color1()$couleur
                  #c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9")
                  
                ))
         ),
         
         
         
         hchart(daa3(),"line",hcaes(x=annee,y=liv2))%>%
           hc_tooltip(
             crosshairs = TRUE,
             backgroundColor = "#F0F0F0",
             borderColor="#212121",
             shared = TRUE, 
             borderWidth = 3,
             sort=TRUE,
             pointFormat='Livraisons : <b>{point.y}</b><br/>'
           )%>%
           hc_chart(
             backgroundColor = "#ffffff"
           )%>%
           hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}")),title=list(text = "")
           )%>%
           hc_xAxis(
             title=list(text = ""),
             tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      
      if(xMax-xMin>11 && xMax-xMin<15){
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 2 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else if(xMax-xMin>14) {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 4 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
      }
      
                       
                      }")
             
           )
         
         
         
         
         
    )
    )
  })
  
}

shinyApp(ui = ui, server = server)
