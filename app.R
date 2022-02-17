library(shiny)
library(shinydashboard)
library(purrr)
library(shinyWidgets)
library(shinymanager)
library(plotrix)
library(ggpubr)
library(shinyMatrix)
library(leaflet)
library(googleway)
library(googlesheets4)
library(data.table)
library(png)
library(shinydashboardPlus)
library(DT)
library(gt)
library(shinyjs)
library(lubridate)
library(scales)
library(dplyr)
library(plotly)
library(summarytools)
library(reshape2)
library(rhandsontable)
library(ggforce)
library(magick)
library(png)
library(scales)
library(gridExtra)
library(gganimate)
library(cluster)    # Algoritma klastering
library(factoextra) # Algoritma klastering dan visualisasi
library(tidyverse)
library(mclust)
library(forecast)
library(lmtest)
require(vegan)
library(apcluster)
library(tmap)
library(tmaptools)
require(maptools)  
require(mapdata) 
library(ggplot2) 
library(ggthemes) #package for ggplot2 theme
library(rgdal)
library(fpc)
library(sf)
library(rgeos)
options(gargle_oauth_cache = ".secrets", email = "tabitayuni100@gmail.com") #email disesuaikan dengan yang menjalankan program
gs4_auth(cache = ".secrets", email = "tabitayuni100@gmail.com")

as_sheets_id("https://docs.google.com/spreadsheets/d/18z7gwrWYckF9-0RBsCNONztat2HX2Zv9LcSnVZeaZF0/edit?usp=sharing") 
sheet_id <- "18z7gwrWYckF9-0RBsCNONztat2HX2Zv9LcSnVZeaZF0"

data <- as.data.table(read_sheet(ss = sheet_id,sheet = "2020_ID_Region_Mobility_Report"))

icdrate = function(Data, nc, c)
{  n = dim(Data)[1]
p = dim(Data)[2]
X = Data[,1:(p-1)]
Group = Data[,p]
p = dim(X)[2]
Mean.X = matrix(ncol = p, nrow = (nc+1))
for (i in 1:nc)
{
  for (j in 1:p)
  {
    Mean.X[i,j] = mean(X[which(Group==i),j])
    Mean.X[(nc+1),j] = mean(X[,j])
  }}
SST = matrix(ncol=p, nrow=n)
for (i in 1:n)
{
  for (j in 1:p)
  {
    SST[i,j] = (X[i,j] - Mean.X[(nc+1),j])^2
  }
}
SST = sum(sum(SST))
SSE = matrix(ncol=p, nrow=n)
for (i in 1:n)
{
  for (j in 1:p)
  {
    for (k in 1:nc)
    {
      if (Group[i]==k)
      {
        SSE[i,j] = (X[i,j] - Mean.X[k,j])^2
      } } } }
SSE = sum(sum(SSE))
Rsq = (SST-SSE)/SST
icdrate = 1-Rsq
Pseudof = (Rsq/(c-1))/((icdrate)/(nc-c))
ssb=SST-SSE
list(SSW=SSE, SST=SST, SSB=ssb, Rsq=Rsq, icdrate=icdrate, pseudof=Pseudof) }

dataclus=function(no)
{ if(no==1){
  data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
  data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
  data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
  data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
  data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
  data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
  
  sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                          data$parks,data$transit_stations,
                          data$workplaces,data$residential),
                     by=list(data$sub_region_1),FUN=mean)
  colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
  datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
  dataclus=datafix1[,-1]
  rownames(dataclus)=datafix1$Provinsi
  datafix <- scale(dataclus) #standarisasi data
}
  return(datafix)
}

download.file("https://www.googleapis.com/drive/v3/files/1HlpBDyNIo7E45fMFwDaFk8lEQnX_xzRy?alt=media&key=AIzaSyCOkkld3wgsNa645UU3XcF8v0HO-UdQpew",
              destfile="world_shape_file.zip",mode='wb')
unzip("world_shape_file.zip")
file.remove("world_shape_file.zip")
#mr=readOGR(dsn = getwd(),layer = "BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL")
shapefile <- readShapeSpatial("BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")
#shapefile=read_sf(dsn = getwd(),layer = "BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL")
#st_is_valid(shapefile, reason = TRUE)
mr <- st_make_valid(st_as_sf(shapefile))
#st_is_valid(mr, reason = TRUE)



shinyApp( ui =fluidPage( setBackgroundColor( color = c("#F7FBFF", "	#F0F8FF"), gradient = "linear", direction = "bottom"),
                         useShinydashboard(),
                         titlePanel(fluidRow(
                           column(1, htmlOutput("picture")),
                           column(1, htmlOutput("picture1")),
                           column(8,offset=1,h1(strong("LAPORAN MOBILITAS MASYARAKAT INDONESIA"),align="center")))
                           
                           
                         ),
                         
                         tags$style(HTML("
    .tabbable > .nav > li > a {background-color: 		#AFEEEE;  color:black; width: 200PX;}
  ")),
                         tabsetPanel(
                           
                           tabPanel(id="eda", "EKSPLORASI DATA", 
                                    tags$style(HTML("    .box.box-solid.box-primary>.box-header {
                    color:	#fff;
                    background:	#8B008B }
                   .box.box-primary{ border-bottom-color:		#FFE4E1; border-left-color:		#FFE4E1; border-right-color:		#FFE4E1;
                border-top-color:		#FFE4E1; background:		#FFE4E1 } ")),
                                    
                                    fluidRow(
                                      
                                      box(title= "Pilih di sini", solidHeader = TRUE, status = "primary", width = 2, collapsible = FALSE, 
                                          uiOutput("checkprov"),
                                          uiOutput("checkmont"))  ,
                                      div(style="width:1500px;", box(title = strong("Statistika Deskriptif Setiap Variabel"), 
                                                                     verbatimTextOutput("summary",placeholder = TRUE)) ),
                                      box(title = strong("Densitas Retail_Recreation"),plotOutput("graf1"),width =4),
                                      box(title = strong("Densitas Grocery_Pharmacy"),plotOutput("graf2"),width =3),
                                      box(title = strong("Densitas Parks"),plotOutput("graf3"),width =3),
                                      box(title = strong("Densitas Transit_stations"),plotOutput("graf4"),width =3),
                                      box(title = strong("Densitas Workplaces"),plotOutput("graf5"),width =3),
                                      box(title = strong("Densitas Residental"),plotOutput("graf6"),width =3)
                                    )),
                           
                           
                           tabPanel(id="cl", "CLUSTERING",
                                    h3("Pengelompokkan Provinsi di Indonesia Berdasarkan Nilai GMI", style = "font-family: 'times'; font-si11pt",align="justify"),
                                    column(2, h4("Metode Clustering"),helpText("Non Hierarchical Clustering"),actionButton("kmeans","K-Means   "),
                                           helpText("Agglomerative Hierarchical Clustering"),
                                           actionButton("single","   Single Linkage    "), helpText(" "), 
                                           actionButton("complete","Complete Linkage"), helpText(" "),actionButton("average","Average Linkage")
                                    ),
                                    useShinyjs() ,
                                    
                                    conditionalPanel( condition = "input.kmeans != 0",
                                                      mainPanel(  tabsetPanel( id="km",
                                                                               tabPanel("Jumlah Cluster Optimum",
                                                                                        box(id="g1", title = strong("Silhouette Score (Penentuan Jumlah Cluster Optimum)"),plotOutput("g1"),width =6),
                                                                                        box(id="k1", title = strong("Calinski Criterion (Jumlah Cluster Optimum)"),plotOutput("k1"),
                                                                                            textOutput("calinski"), width =6),
                                                                                        box(id="k2", title = strong("Optimum Average Silhouette Width"),plotOutput("k2"),
                                                                                            textOutput("affinity"), width =12)),
                                                                               tabPanel("Hasil Clustering",
                                                                                        box(id="pfk",title = strong("Performa Model Cluster"),dataTableOutput("pfk"),width =4),
                                                                                        box(id="gp",title = strong("Clustering"),plotOutput("graf1performa4"),width =7),
                                                                                        box(id="tb",title = strong("Tabel Anggota Cluster"),dataTableOutput("tbk") ,width =4),
                                                                                        box(id="rk",title = strong("Rata-Rata Cluster"),dataTableOutput("rk") ,width =5)
                                                                               ),
                                                                               tabPanel("Peta Hasil Clustering",
                                                                                        box(id="pt",title=strong("Hasil Pengelompokan"), tmapOutput("pt"),width =9),
                                                                                        box(id="tb1",title = strong("Tabel Anggota Cluster"),dataTableOutput("tbk1") ,width =3)
                                                                               ))))  ,
                                    
                                    conditionalPanel( condition = "input.single != 0",
                                                      mainPanel( selectInput("dist1","Jarak / Distance",choices = c("euclidean","manhattan","minkowski") ),
                                                                 box(id="g2",title = strong("Dendogram Single Linkage"),plotOutput("g2",width = "100%", height = "500px"),width =8   ),
                                                                 box(id="tbs",title = strong("Tabel Anggota Cluster"),dataTableOutput("tbs") ,width =4),
                                                                 box(id="pfs",title = strong("Performa Model Cluster"),dataTableOutput("pfs"),width =11),
                                                                 box(id="cls1",title = strong("Clustering Model Terbaik"),plotOutput("cls1"),width =6),
                                                                 box(id="rs",title = strong("Rata-Rata Cluster"),dataTableOutput("rs") ,width =6)
                                                      )),
                                    
                                    conditionalPanel( condition = "input.complete != 0",
                                                      mainPanel( selectInput("dist2","Jarak / Distance",choices = c("euclidean","manhattan","minkowski") ),
                                                                 box(id="c1",title = strong("Dendogram Complete Linkage"),plotOutput("c1",width = "100%", height = "500px"),width =8   ),
                                                                 box(id="tbc",title = strong("Tabel Anggota Cluster"),dataTableOutput("tbc") ,width =4),
                                                                 box(id="pfc",title = strong("Performa Model Cluster"),dataTableOutput("pfc"),width =11),
                                                                 box(id="clc1",title = strong("Clustering Model Terbaik"),plotOutput("clc1"),width =7),
                                                                 box(id="rc",title = strong("Rata-Rata Cluster"),dataTableOutput("rc") ,width =5)
                                                      )),
                                    
                                    conditionalPanel( condition = "input.average != 0",
                                                      mainPanel( selectInput("dist3","Jarak / Distance",choices = c("euclidean","manhattan","minkowski") ),
                                                                 box(id="a1",title = strong("Dendogram Average Linkage"),plotOutput("a1", height = "500px"),width =8   ),
                                                                 box(id="tba",title = strong("Tabel Anggota Cluster"),dataTableOutput("tba") ,width =4),
                                                                 box(id="pfa",title = strong("Performa Model Cluster"),dataTableOutput("pfa"),width =11),
                                                                 box(id="cla1",title = strong("Clustering Model Terbaik"),plotOutput("cla1"),width =7),
                                                                 box(id="ra",title = strong("Rata-Rata Cluster"),dataTableOutput("ra") ,width =5)
                                                      )) ),
                           
                           tabPanel(id="fore", "FORECASTING",
                                    h3("PERAMALAN NILAI GMI (GOOGLE MOBILITY INDEX) DI INDONESIA" ,style = "font-family: 'times'; font-si11pt",align="justify"),
                                    p("Berikut hasil peramalan 30 hari kedepan di masing-masing variabel yaitu Nilai GMI berdasarkan jenis tempat tertentu di Indonesia.
                   Data yang digunakan adalah data dari 15 Februari 2020 sampai 31 Desember 2020. 
                   Model peramalan yang digunakan adalah ARIMA dengan menggunakan function auto.arima di R.
                   Garis warna hitam menunjukkan data asli, sedangkan garis warna biru menunjukkan hasil peramalan selama 30 hari kedepan.",
                                      style = "font-family: 'times'; font-si11pt",align="justify"),
                                    p("Silahkan pilih wilayah Provinsi untuk menampilkan hasil peramalan nilai GMI di provinsi Tersebut", style = "font-family: 'times'; font-si11pt",align="justify"),
                                    uiOutput("selectprov"),
                                    box(title = strong("Peramalan Nilai GMI di Tempat Perbelanjaan dan Rekreasi"),
                                        plotOutput("graf7"),width =6, div(style= "height:500px;", verbatimTextOutput("rmse1",placeholder = TRUE ))),
                                    box(title = strong("Peramalan Nilai GMI di Grocery dan Farmasi"),
                                        plotOutput("graf8"),width =6, div(style= "height:500px;", verbatimTextOutput("rmse2",placeholder = TRUE ))),
                                    box(title = strong("Peramalan Nilai GMI di Taman Kota, Taman Nasional, Taman Umum"),
                                        plotOutput("graf9"),width =6, div(style= "height:500px;", verbatimTextOutput("rmse3",placeholder = TRUE ))),
                                    box(title = strong("Peramalan Nilai GMI di di Tempat Transit"),
                                        plotOutput("graf10"),width =6, div(style= "height:500px;", verbatimTextOutput("rmse4",placeholder = TRUE ))),
                                    box(title = strong("Peramalan Nilai GMI di di Tempat Kerja"),
                                        plotOutput("graf11"),width =6, div(style= "height:500px;", verbatimTextOutput("rmse5",placeholder = TRUE ))),
                                    box(title = strong("Peramalan Nilai GMI di Area Pemukiman"),
                                        plotOutput("graf12"),width =6, div(style= "height:500px;", verbatimTextOutput("rmse6",placeholder = TRUE )))),
                           
                           
                           tabPanel(id="data", "RAW DATA",
                                    dataTableOutput("res")),
                           
                           
                           tabPanel(id="dd", "DESKRIPSI DATA", 
                                    titlePanel(h3(id="big-heading", strong("Deskripsi Data Mobilitas Indonesia"),align = "center")),
                                    tags$style(HTML("#big-heading{color: BLUE;}")),
                                    
                                    tags$ul(
                                      tags$li(tags$b("GMI (Google Mobility Index)"), " - Persentase perubahan jumlah/lama kunjungan seseorang ke beberapa tempat dalam 
              suatu hari dibandingkan baseline days. Nilai baseline berupa median per hari yang dihitung menggunakan data kunjungan 
              pada periode 3 Januari - 6 Februari 2020. Nilai GMI positif berarti aktivitas lebih tinggi dibandingkan dengan periode
              baseline, nilai GMI negatif berarti aktivitas lebih rendah dibandingkan dengan periode baseline. 
              Sumber :", a("https://www.google.com/covid19/mobility/",href="https://www.google.com/covid19/mobility/"), style = "font-family: 'times'; font-si12pt", align="justify"),
                                      
                                      
                                      tags$li(tags$b("country_region"), " - Negara", style = "font-family: 'times'; font-si12pt", align="justify"),
                                      tags$li(tags$b("sub_region_1"), " - Provinsi", style = "font-family: 'times'; font-si12pt", align="justify"),
                                      tags$li(tags$b("iso_3166_2_code"), " - Kode Provinsi", style = "font-family: 'times'; font-si12pt", align="justify"),
                                      tags$li(tags$b("island"), " - Pulau", style = "font-family: 'times'; font-si12pt", align="justify"),
                                      tags$li(tags$b("date"), " - Tanggal Mobilitas", style = "font-family: 'times'; font-si12pt", align="justify"),
                                      tags$li(tags$b("retail_and_recreation"), " - Nilai GMI di tempat perbelanjaan retail dan rekreasi", style = "font-family: 'times'; font-si12pt", align="justify"),
                                      tags$li(tags$b("grocery_and_pharmacy"), " - Nilai GMI di tempat berbelanjaan kebutuhan sehari-hari dan farmasi", style = "font-family: 'times'; font-si12pt", align="justify"),
                                      tags$li(tags$b("parks"), " - Nilai GMI di taman kota, taman nasional, taman umum", style = "font-family: 'times'; font-si12pt", align="justify"), 
                                      tags$li(tags$b("transit_stations"), " - Nilai GMI di tempat transit seperti terminal, halte bis, stasiun kereta api", style = "font-family: 'times'; font-si12pt", align="justify"),
                                      tags$li(tags$b("workplaces"), " - Nilai GMI di tempat kerja", style = "font-family: 'times'; font-si12pt", align="justify"),
                                      tags$li(tags$b("residential"), " - Nilai GMI di area pemukiman", style = "font-family: 'times'; font-si12pt", align="justify") ) ) )),
          
          server = function(input, output, session) {
            
            dat <- as.data.table(read_sheet(ss = sheet_id,sheet = "Sheet2"))
            datin=function(no)
            { if(no==1){
              dat$retail_and_recreation[is.na(dat$retail_and_recreation)] <- median(dat$retail_and_recreation, na.rm = TRUE)
              dat$grocery_and_pharmacy[is.na(dat$grocery_and_pharmacy)] <- median(dat$grocery_and_pharmacy, na.rm = TRUE)
              dat$parks[is.na(dat$parks)] <- mean(dat$parks, na.rm = TRUE)
              dat$transit_stations[is.na(dat$transit_stations)] <- median(dat$transit_stations, na.rm = TRUE)
              dat$workplaces[is.na(dat$workplaces)] <- median(dat$workplaces, na.rm = TRUE)
              dat$residential[is.na(dat$residential)] <- median(dat$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(dat$retail_and_recreation,dat$grocery_and_pharmacy,
                                      dat$parks,dat$transit_stations,
                                      dat$workplaces,dat$residential),
                                 by=list(dat$reg1,dat$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","Provinsi1", "retail","grocery","parks","transit","workplaces","residental")
              sum_dt1=sum_dt[order(sum_dt$Provinsi),]
              datafix1=sum_dt1[!grepl("Indonesia", sum_dt1$Provinsi),]
              dataclus=datafix1[,-(1:2)]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) }
              return(datafix)
            }
            
            
            output$picture<-renderUI({
              div(id = "myImage", 
                  tags$a(  href="https://www.its.ac.id/id/beranda/",
                           tags$img(height = 90, width = 90, src = "https://www.its.ac.id/wp-content/uploads/2020/07/Lambang-ITS-2-320x320.png")
                  ) )})
            output$picture1<-renderUI({
              div(id = "myImage1", 
                  tags$a(  href="https://www.bps.go.id/",
                           tags$img(height = 90, width = 90, src = " https://upload.wikimedia.org/wikipedia/commons/2/28/Lambang_Badan_Pusat_Statistik_%28BPS%29_Indonesia.svg")
                  ) )})
            
            output$res <- renderDataTable({
              dtDatasofar <- data
              dt1=datatable(dtDatasofar)
              dt1%>%formatStyle('sub_region_1',target = 'row',background =  "white")
            })
            
            output$checkprov<-renderUI({
              dtDatasofar <- data
              checkboxGroupInput("pul","Wilayah Administratif",choices = unique(dtDatasofar$island),selected = unique(dtDatasofar$island))}) 
            
            output$checkmont<-renderUI({
              dtDatasofar <- data
              checkboxGroupInput("bul","Bulan",choices = unique(dtDatasofar$Month),selected = unique(dtDatasofar$Month))}) 
            
            output$selectprov<-renderUI({
              dtDatasofar <- data
              selectInput("prov","Provinsi",choices = unique(dtDatasofar$sub_region_1))}) 
            
            output$summary <- renderPrint({
              data1=data
              a=data1$island %in% input$pul
              b=data1[a,]
              b1=b$Month %in% input$bul
              b2=b[b1,]
              dataset <- b2[,7:12]
              summary(dataset)
            })
            
            output$graf1 <- renderPlot({
              data1=data
              a=data1$island %in% input$pul
              b=data1[a,]
              b1=b$Month %in% input$bul
              b2=b[b1,]
              dataset <- b2[,7:12]
              ggplot(dataset,aes(x=retail_and_recreation  ))+
                geom_histogram(aes(y=..density..),colour="black", fill="white")+
                geom_density(alpha=0.1,fill="red")+theme_minimal()+theme(panel.grid.major = element_blank(), 
                                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
            })
            output$graf2 <- renderPlot({
              data1=data
              a=data1$island %in% input$pul
              b=data1[a,]
              b1=b$Month %in% input$bul
              b2=b[b1,]
              dataset <- b2[,7:12]
              ggplot(dataset,aes(x=grocery_and_pharmacy  ))+
                geom_histogram(aes(y=..density..),colour="black", fill="white")+
                geom_density(alpha=0.1,fill="red")+theme_minimal()+theme(panel.grid.major = element_blank(), 
                                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
            })
            output$graf3 <- renderPlot({
              data1=data
              a=data1$island %in% input$pul
              b=data1[a,]
              b1=b$Month %in% input$bul
              b2=b[b1,]
              dataset <- b2[,7:12]
              ggplot(dataset,aes(x=parks  ))+
                geom_histogram(aes(y=..density..),colour="black", fill="white")+
                geom_density(alpha=0.1,fill="red")+theme_minimal()+theme(panel.grid.major = element_blank(), 
                                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
            })
            output$graf4 <- renderPlot({
              data1=data
              a=data1$island %in% input$pul
              b=data1[a,]
              b1=b$Month %in% input$bul
              b2=b[b1,]
              dataset <- b2[,7:12]
              ggplot(dataset,aes(x=transit_stations  ))+
                geom_histogram(aes(y=..density..),colour="black", fill="white")+
                geom_density(alpha=0.1,fill="red")+theme_minimal()+theme(panel.grid.major = element_blank(), 
                                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
            })
            output$graf5 <- renderPlot({
              data1=data
              a=data1$island %in% input$pul
              b=data1[a,]
              b1=b$Month %in% input$bul
              b2=b[b1,]
              dataset <- b2[,7:12]
              ggplot(dataset,aes(x=workplaces  ))+
                geom_histogram(aes(y=..density..),colour="black", fill="white")+
                geom_density(alpha=0.1,fill="red")+theme_minimal()+theme(panel.grid.major = element_blank(), 
                                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
            })
            output$graf6 <- renderPlot({
              data1=data
              a=data1$island %in% input$pul
              b=data1[a,]
              b1=b$Month %in% input$bul
              b2=b[b1,]
              dataset <- b2[,7:12]
              ggplot(dataset,aes(x=residential  ))+
                geom_histogram(aes(y=..density..),colour="black", fill="white")+
                geom_density(alpha=0.1,fill="red")+theme_minimal()+theme(panel.grid.major = element_blank(), 
                                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
            })
            
            output$graf7 <- renderPlot({
              data1=data
              a=data1$sub_region_1 %in% input$prov
              b=data1[a,]
              train_data <- b %>%
                filter(Month != "Desember") 
              test_data <- b %>%
                filter(Month == "Desember")
              retailtrain=as.ts(train_data$retail_and_recreation)
              retailtest=as.ts(test_data$retail_and_recreation)
              modelARIMA=auto.arima(retailtrain)
              fore.ARIMA=predict(modelARIMA, 31)$pred         #define forecast value for testing data
              se.fore.ARIMA=predict(modelARIMA, 31)$se       #define standard error for forecasting result
              fits.ARIMA=as.ts(fitted(modelARIMA)) 
              fore.ARIMA1=predict(modelARIMA, 61)$pred
              plot(as.ts(retailtrain),ylab="Yt (Nilai GMI Retail_Recreation)",xlab="Hari",lwd=2,xlim=c(1,351))
              lines(ts(retailtest,start = 291 ),col="black",lwd=2)
              lines(as.ts(fits.ARIMA),col="red2",lwd=2)
              lines(as.ts(fore.ARIMA),col="red2",lwd=2)
              lines(ts(as.vector(fore.ARIMA1)[32:61],start= 322),col="blue",lwd=2)
              legend("bottomright",c("Data aktual","Data ramalan", "Ramalan 30 Hari Kedepan"),
                     col=c("black","red2","blue"),lwd=2,cex=0.7)
            })
            
            output$rmse1 <- renderPrint({
              data1=data
              a=data1$sub_region_1 %in% input$prov
              b=data1[a,]
              train_data <- b %>%
                filter(Month != "Desember") 
              test_data <- b %>%
                filter(Month == "Desember")
              retailtrain=as.ts(train_data$retail_and_recreation)
              retailtest=test_data$retail_and_recreation
              modelARIMA=auto.arima(retailtrain)
              cof=coeftest(modelARIMA)
              fore.ARIMA=predict(modelARIMA, 31)$pred         #define forecast value for testing data
              se.fore.ARIMA=predict(modelARIMA, 31)$se       #define standard error for forecasting result
              fits.ARIMA=as.ts(fitted(modelARIMA)) 
              akurasi=matrix(0,1,6)
              colnames(akurasi)=c("RMSE_training","MAE_training","MAPE_training","RMSE_testing","MAE_testing","MAPE_testing")
              for (i in 1:1)
              {akurasi[i,1]=accuracy(fits.ARIMA,retailtrain)[1,2]
              akurasi[i,2]=accuracy(fits.ARIMA,retailtrain)[1,3]
              akurasi[i,3]=accuracy(fits.ARIMA,retailtrain)[1,5]
              akurasi[i,4]=accuracy(as.vector(fore.ARIMA),retailtest)[1,2]
              akurasi[i,5]=accuracy(as.vector(fore.ARIMA),retailtest)[1,3]
              akurasi[i,6]=accuracy(as.vector(fore.ARIMA),retailtest)[1,5]}
              akurasi=list(cof,akurasi)
              akurasi
            })
            
            output$graf8 <- renderPlot({
              data1=data
              a=data1$sub_region_1 %in% input$prov
              b=data1[a,]
              train_data <- b %>%
                filter(Month != "Desember") 
              test_data <- b %>%
                filter(Month == "Desember")
              retailtrain=as.ts(train_data$grocery_and_pharmacy)
              retailtest=as.ts(test_data$grocery_and_pharmacy)
              modelARIMA=auto.arima(retailtrain)
              fore.ARIMA=predict(modelARIMA, 31)$pred         #define forecast value for testing data
              se.fore.ARIMA=predict(modelARIMA, 31)$se       #define standard error for forecasting result
              fits.ARIMA=as.ts(fitted(modelARIMA)) 
              fore.ARIMA1=predict(modelARIMA, 61)$pred
              plot(as.ts(retailtrain),ylab="Yt (Nilai GMI Grocery_Pharmacy)",xlab="Hari",lwd=2,xlim=c(1,351))
              lines(ts(retailtest,start = 291 ),col="black",lwd=2)
              lines(as.ts(fits.ARIMA),col="red2",lwd=2)
              lines(as.ts(fore.ARIMA),col="red2",lwd=2)
              lines(ts(as.vector(fore.ARIMA1)[32:61],start= 322),col="blue",lwd=2)
              legend("bottomright",c("Data aktual","Data ramalan", "Ramalan 30 Hari Kedepan"),
                     col=c("black","red2","blue"),lwd=2,cex=0.7) })
            
            output$rmse2 <- renderPrint({
              data1=data
              a=data1$sub_region_1 %in% input$prov
              b=data1[a,]
              train_data <- b %>%
                filter(Month != "Desember") 
              test_data <- b %>%
                filter(Month == "Desember")
              retailtrain=as.ts(train_data$grocery_and_pharmacy)
              retailtest=test_data$grocery_and_pharmacy
              modelARIMA=auto.arima(retailtrain)
              cof=coeftest(modelARIMA)
              fore.ARIMA=predict(modelARIMA, 31)$pred         #define forecast value for testing data
              se.fore.ARIMA=predict(modelARIMA, 31)$se       #define standard error for forecasting result
              fits.ARIMA=as.ts(fitted(modelARIMA)) 
              akurasi=matrix(0,1,6)
              colnames(akurasi)=c("RMSE_training","MAE_training","MAPE_training","RMSE_testing","MAE_testing","MAPE_testing")
              for (i in 1:1)
              {akurasi[i,1]=accuracy(fits.ARIMA,retailtrain)[1,2]
              akurasi[i,2]=accuracy(fits.ARIMA,retailtrain)[1,3]
              akurasi[i,3]=accuracy(fits.ARIMA,retailtrain)[1,5]
              akurasi[i,4]=accuracy(as.vector(fore.ARIMA),retailtest)[1,2]
              akurasi[i,5]=accuracy(as.vector(fore.ARIMA),retailtest)[1,3]
              akurasi[i,6]=accuracy(as.vector(fore.ARIMA),retailtest)[1,5]}
              akurasi=list(cof,akurasi)
              akurasi })
            
            output$graf9 <- renderPlot({
              data1=data
              a=data1$sub_region_1 %in% input$prov
              b=data1[a,]
              train_data <- b %>%
                filter(Month != "Desember") 
              test_data <- b %>%
                filter(Month == "Desember")
              retailtrain=as.ts(train_data$parks)
              retailtest=as.ts(test_data$parks)
              modelARIMA=auto.arima(retailtrain)
              fore.ARIMA=predict(modelARIMA, 31)$pred         #define forecast value for testing data
              se.fore.ARIMA=predict(modelARIMA, 31)$se       #define standard error for forecasting result
              fits.ARIMA=as.ts(fitted(modelARIMA)) 
              fore.ARIMA1=predict(modelARIMA, 61)$pred
              plot(as.ts(retailtrain),ylab="Yt (Nilai GMI Parks)",xlab="Hari",lwd=2,xlim=c(1,351))
              lines(ts(retailtest,start = 291 ),col="black",lwd=2)
              lines(as.ts(fits.ARIMA),col="red2",lwd=2)
              lines(as.ts(fore.ARIMA),col="red2",lwd=2)
              lines(ts(as.vector(fore.ARIMA1)[32:61],start= 322),col="blue",lwd=2)
              legend("bottomright",c("Data aktual","Data ramalan", "Ramalan 30 Hari Kedepan"),
                     col=c("black","red2","blue"),lwd=2,cex=0.7) })
            
            output$rmse3 <- renderPrint({
              data1=data
              a=data1$sub_region_1 %in% input$prov
              b=data1[a,]
              train_data <- b %>%
                filter(Month != "Desember") 
              test_data <- b %>%
                filter(Month == "Desember")
              retailtrain=as.ts(train_data$parks)
              retailtest=test_data$parks
              modelARIMA=auto.arima(retailtrain)
              cof=coeftest(modelARIMA)
              fore.ARIMA=predict(modelARIMA, 31)$pred         #define forecast value for testing data
              fits.ARIMA=as.ts(fitted(modelARIMA)) 
              akurasi=matrix(0,1,6)
              colnames(akurasi)=c("RMSE_training","MAE_training","MAPE_training","RMSE_testing","MAE_testing","MAPE_testing")
              for (i in 1:1)
              {akurasi[i,1]=accuracy(fits.ARIMA,retailtrain)[1,2]
              akurasi[i,2]=accuracy(fits.ARIMA,retailtrain)[1,3]
              akurasi[i,3]=accuracy(fits.ARIMA,retailtrain)[1,5]
              akurasi[i,4]=accuracy(as.vector(fore.ARIMA),retailtest)[1,2]
              akurasi[i,5]=accuracy(as.vector(fore.ARIMA),retailtest)[1,3]
              akurasi[i,6]=accuracy(as.vector(fore.ARIMA),retailtest)[1,5]}
              akurasi=list(cof,akurasi)
              akurasi })
            
            output$graf10 <- renderPlot({
              data1=data
              a=data1$sub_region_1 %in% input$prov
              b=data1[a,]
              train_data <- b %>%
                filter(Month != "Desember") 
              test_data <- b %>%
                filter(Month == "Desember")
              retailtrain=as.ts(train_data$transit_stations)
              retailtest=as.ts(test_data$transit_stations)
              modelARIMA=auto.arima(retailtrain)
              fore.ARIMA=predict(modelARIMA, 31)$pred         #define forecast value for testing data
              se.fore.ARIMA=predict(modelARIMA, 31)$se       #define standard error for forecasting result
              fits.ARIMA=as.ts(fitted(modelARIMA)) 
              fore.ARIMA1=predict(modelARIMA, 61)$pred
              plot(as.ts(retailtrain),ylab="Yt (Nilai GMI Transit_Stations)",xlab="Hari",lwd=2,xlim=c(1,351))
              lines(ts(retailtest,start = 291 ),col="black",lwd=2)
              lines(as.ts(fits.ARIMA),col="red2",lwd=2)
              lines(as.ts(fore.ARIMA),col="red2",lwd=2)
              lines(ts(as.vector(fore.ARIMA1)[32:61],start= 322),col="blue",lwd=2)
              legend("bottomright",c("Data aktual","Data ramalan", "Ramalan 30 Hari Kedepan"),
                     col=c("black","red2","blue"),lwd=2,cex=0.7) 
            })
            
            output$rmse4 <- renderPrint({
              data1=data
              a=data1$sub_region_1 %in% input$prov
              b=data1[a,]
              train_data <- b %>%
                filter(Month != "Desember") 
              test_data <- b %>%
                filter(Month == "Desember")
              retailtrain=as.ts(train_data$transit_stations)
              retailtest=test_data$transit_stations
              modelARIMA=auto.arima(retailtrain)
              cof=coeftest(modelARIMA)
              fore.ARIMA=predict(modelARIMA, 31)$pred         #define forecast value for testing data
              fits.ARIMA=as.ts(fitted(modelARIMA)) 
              akurasi=matrix(0,1,6)
              colnames(akurasi)=c("RMSE_training","MAE_training","MAPE_training","RMSE_testing","MAE_testing","MAPE_testing")
              for (i in 1:1)
              {akurasi[i,1]=accuracy(fits.ARIMA,retailtrain)[1,2]
              akurasi[i,2]=accuracy(fits.ARIMA,retailtrain)[1,3]
              akurasi[i,3]=accuracy(fits.ARIMA,retailtrain)[1,5]
              akurasi[i,4]=accuracy(as.vector(fore.ARIMA),retailtest)[1,2]
              akurasi[i,5]=accuracy(as.vector(fore.ARIMA),retailtest)[1,3]
              akurasi[i,6]=accuracy(as.vector(fore.ARIMA),retailtest)[1,5]}
              akurasi=list(cof,akurasi)
              akurasi 
            })
            
            output$graf11 <- renderPlot({
              data1=data
              a=data1$sub_region_1 %in% input$prov
              b=data1[a,]
              train_data <- b %>%
                filter(Month != "Desember") 
              test_data <- b %>%
                filter(Month == "Desember")
              retailtrain=as.ts(train_data$workplaces)
              retailtest=as.ts(test_data$workplaces)
              modelARIMA=auto.arima(retailtrain)
              fore.ARIMA=predict(modelARIMA, 31)$pred         #define forecast value for testing data
              se.fore.ARIMA=predict(modelARIMA, 31)$se       #define standard error for forecasting result
              fits.ARIMA=as.ts(fitted(modelARIMA)) 
              fore.ARIMA1=predict(modelARIMA, 61)$pred
              plot(as.ts(retailtrain),ylab="Yt (Nilai GMI Workplaces)",xlab="Hari",lwd=2,xlim=c(1,351))
              lines(ts(retailtest,start = 291 ),col="black",lwd=2)
              lines(as.ts(fits.ARIMA),col="red2",lwd=2)
              lines(as.ts(fore.ARIMA),col="red2",lwd=2)
              lines(ts(as.vector(fore.ARIMA1)[32:61],start= 322),col="blue",lwd=2)
              legend("bottomright",c("Data aktual","Data ramalan", "Ramalan 30 Hari Kedepan"),
                     col=c("black","red2","blue"),lwd=2,cex=0.7) })
            
            output$rmse5 <- renderPrint({
              data1=data
              a=data1$sub_region_1 %in% input$prov
              b=data1[a,]
              train_data <- b %>%
                filter(Month != "Desember") 
              test_data <- b %>%
                filter(Month == "Desember")
              retailtrain=as.ts(train_data$workplaces)
              retailtest=test_data$workplaces
              modelARIMA=auto.arima(retailtrain)
              cof=coeftest(modelARIMA)
              fore.ARIMA=predict(modelARIMA, 31)$pred         #define forecast value for testing data
              fits.ARIMA=as.ts(fitted(modelARIMA)) 
              akurasi=matrix(0,1,6)
              colnames(akurasi)=c("RMSE_training","MAE_training","MAPE_training","RMSE_testing","MAE_testing","MAPE_testing")
              for (i in 1:1)
              {akurasi[i,1]=accuracy(fits.ARIMA,retailtrain)[1,2]
              akurasi[i,2]=accuracy(fits.ARIMA,retailtrain)[1,3]
              akurasi[i,3]=accuracy(fits.ARIMA,retailtrain)[1,5]
              akurasi[i,4]=accuracy(as.vector(fore.ARIMA),retailtest)[1,2]
              akurasi[i,5]=accuracy(as.vector(fore.ARIMA),retailtest)[1,3]
              akurasi[i,6]=accuracy(as.vector(fore.ARIMA),retailtest)[1,5]
              }
              akurasi=list(cof,akurasi)
              akurasi 
            })
            
            output$graf12 <- renderPlot({
              data1=data
              a=data1$sub_region_1 %in% input$prov
              b=data1[a,]
              train_data <- b %>%
                filter(Month != "Desember") 
              test_data <- b %>%
                filter(Month == "Desember")
              retailtrain=as.ts(train_data$residential)
              retailtest=as.ts(test_data$residential)
              modelARIMA=auto.arima(retailtrain)
              fore.ARIMA=predict(modelARIMA, 31)$pred         #define forecast value for testing data
              se.fore.ARIMA=predict(modelARIMA, 31)$se       #define standard error for forecasting result
              fits.ARIMA=as.ts(fitted(modelARIMA)) 
              fore.ARIMA1=predict(modelARIMA, 61)$pred
              plot(as.ts(retailtrain),ylab="Yt (Nilai GMI Residential)",xlab="Hari",lwd=2,xlim=c(1,351))
              lines(ts(retailtest,start = 291 ),col="black",lwd=2)
              lines(as.ts(fits.ARIMA),col="red2",lwd=2)
              lines(as.ts(fore.ARIMA),col="red2",lwd=2)
              lines(ts(as.vector(fore.ARIMA1)[32:61],start= 322),col="blue",lwd=2)
              legend("bottomright",c("Data aktual","Data ramalan", "Ramalan 30 Hari Kedepan"),
                     col=c("black","red2","blue"),lwd=2,cex=0.7) })
            
            output$rmse6 <- renderPrint({
              data1=data
              a=data1$sub_region_1 %in% input$prov
              b=data1[a,]
              train_data <- b %>%
                filter(Month != "Desember") 
              test_data <- b %>%
                filter(Month == "Desember")
              retailtrain=as.ts(train_data$residential)
              retailtest=test_data$residential
              modelARIMA=auto.arima(retailtrain)
              cof=coeftest(modelARIMA)
              fore.ARIMA=predict(modelARIMA, 31)$pred         #define forecast value for testing data
              fits.ARIMA=as.ts(fitted(modelARIMA)) 
              akurasi=matrix(0,1,6)
              colnames(akurasi)=c("RMSE_training","MAE_training","MAPE_training","RMSE_testing","MAE_testing","MAPE_testing")
              for (i in 1:1)
              {akurasi[i,1]=accuracy(fits.ARIMA,retailtrain)[1,2]
              akurasi[i,2]=accuracy(fits.ARIMA,retailtrain)[1,3]
              akurasi[i,3]=accuracy(fits.ARIMA,retailtrain)[1,5]
              akurasi[i,4]=accuracy(as.vector(fore.ARIMA),retailtest)[1,2]
              akurasi[i,5]=accuracy(as.vector(fore.ARIMA),retailtest)[1,3]
              akurasi[i,6]=accuracy(as.vector(fore.ARIMA),retailtest)[1,5]
              }
              akurasi=list(cof,akurasi)
              akurasi
            })
            
            output$graf1performa4 <- renderPlot({
              dat$retail_and_recreation[is.na(dat$retail_and_recreation)] <- median(dat$retail_and_recreation, na.rm = TRUE)
              dat$grocery_and_pharmacy[is.na(dat$grocery_and_pharmacy)] <- median(dat$grocery_and_pharmacy, na.rm = TRUE)
              dat$parks[is.na(dat$parks)] <- mean(dat$parks, na.rm = TRUE)
              dat$transit_stations[is.na(dat$transit_stations)] <- median(dat$transit_stations, na.rm = TRUE)
              dat$workplaces[is.na(dat$workplaces)] <- median(dat$workplaces, na.rm = TRUE)
              dat$residential[is.na(dat$residential)] <- median(dat$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(dat$retail_and_recreation,dat$grocery_and_pharmacy,
                                      dat$parks,dat$transit_stations,
                                      dat$workplaces,dat$residential),
                                 by=list(dat$reg1,dat$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","Provinsi1", "retail","grocery","parks","transit","workplaces","residental")
              sum_dt1=sum_dt[order(sum_dt$Provinsi),]
              datafix1=sum_dt1[!grepl("Indonesia", sum_dt1$Provinsi),]
              dataclus=datafix1[,-(1:2)]
              datafix <- scale(dataclus) #standarisasi data
              theme_set(theme_classic())
              km_fit = kmeans(datafix,centers = 2,iter.max = 300 )
              fviz_cluster(km_fit, data = datafix)+ theme_set(theme_classic())
            })   
            
            output$pt <- renderTmap({
              dat$retail_and_recreation[is.na(dat$retail_and_recreation)] <- median(dat$retail_and_recreation, na.rm = TRUE)
              dat$grocery_and_pharmacy[is.na(dat$grocery_and_pharmacy)] <- median(dat$grocery_and_pharmacy, na.rm = TRUE)
              dat$parks[is.na(dat$parks)] <- mean(dat$parks, na.rm = TRUE)
              dat$transit_stations[is.na(dat$transit_stations)] <- median(dat$transit_stations, na.rm = TRUE)
              dat$workplaces[is.na(dat$workplaces)] <- median(dat$workplaces, na.rm = TRUE)
              dat$residential[is.na(dat$residential)] <- median(dat$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(dat$retail_and_recreation,dat$grocery_and_pharmacy,
                                      dat$parks,dat$transit_stations,
                                      dat$workplaces,dat$residential),
                                 by=list(dat$reg1,dat$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","Provinsi1", "retail","grocery","parks","transit","workplaces","residental")
              sum_dt1=sum_dt[order(sum_dt$Provinsi),]
              datafix1=sum_dt1[!grepl("Indonesia", sum_dt1$Provinsi),]
              dataclus1=datafix1[,-(1:2)]
              datafix <- scale(dataclus1) #standarisasi data
              km_fit = kmeans(datafix,centers = 2,iter.max = 300 )
              mr$kelompok=km_fit$cluster
              tmap_mode("view")
              tm_shape(mr) +tm_fill("kelompok", palette = "Set2")
            })   
            
            
            output$g1<- renderPlot({
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              datafix <- scale(dataclus) #standarisasi data
              fviz_nbclust(datafix, kmeans, method='silhouette')
            })
            
            output$k1<- renderPlot({
              datafix <- dataclus(1)
              fit <- cascadeKM(scale(datafix, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
              plot(fit, sortg = TRUE, grpmts.plot = TRUE)
            })
            output$k2<- renderPlot({
              datafix <- dataclus(1)
              pamk.best <- pamk(datafix)
              par(mfrow=c(1,2))
              plot(pam(datafix, pamk.best$nc),main=NULL)
            })
            
            output$g2<- renderPlot({
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist1)
              hc1 <- hclust(d, method = "single" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              k=as.numeric(str_sub(a$nama, -1, -1))
              plot(hc1, cex = 0.6, hang = -1)
              rect.hclust(hc1, k = k, border = 2:5)
            })
            
            output$c1<- renderPlot({
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist2)
              hc1 <- hclust(d, method = "complete" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              k=as.numeric(str_sub(a$nama, -1, -1))
              plot(hc1, cex = 0.6, hang = -1)
              rect.hclust(hc1, k = k, border = 2:5)
            })
            
            output$a1<- renderPlot({
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist3)
              hc1 <- hclust(d, method = "average" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              k=as.numeric(str_sub(a$nama, -1, -1))
              plot(hc1, cex = 0.6, hang = -1)
              rect.hclust(hc1, k = k, border = 2:5)
            })
            
            observeEvent(input$single, {
              shinyjs::show(id = "g2")
              shinyjs::show(id = "dist1")
              shinyjs::hide(id = "dist2")
              shinyjs::hide(id = "dist3")
              shinyjs::hide(id = "gp")
              shinyjs::hide(id = "g1")
              shinyjs::hide(id = "c1")
              shinyjs::hide(id = "a1")
              shinyjs::hide(id = "k1")
              shinyjs::hide(id = "km")
              shinyjs::hide(id = "k2")
              shinyjs::hide(id = "pt")
              shinyjs::hide(id = "pfk")
              shinyjs::hide(id = "tb")
              shinyjs::hide(id = "tb1")
              shinyjs::show(id = "pfs")
              shinyjs::hide(id = "pfc")
              shinyjs::hide(id = "pfa")
              shinyjs::hide(id = "clc1")
              shinyjs::show(id = "cls1")
              shinyjs::hide(id = "cla1")
              shinyjs::hide(id = "tba")
              shinyjs::hide(id = "tbc")
              shinyjs::show(id = "tbs")
              shinyjs::hide(id = "rk")
              shinyjs::show(id = "rs")
              shinyjs::hide(id = "rc")
              shinyjs::hide(id = "ra")
            })
            
            observeEvent(input$kmeans, {
              shinyjs::hide(id = "g2")
              shinyjs::hide(id = "dist1")
              shinyjs::hide(id = "dist2")
              shinyjs::hide(id = "dist3")
              shinyjs::show(id = "g1")
              shinyjs::show(id = "gp")
              shinyjs::hide(id = "c1")
              shinyjs::hide(id = "a1")
              shinyjs::show(id = "k1")
              shinyjs::show(id = "km")
              shinyjs::show(id = "k2")
              shinyjs::show(id = "pt")
              shinyjs::show(id = "pfk")
              shinyjs::show(id = "tb")
              shinyjs::show(id = "tb1")
              shinyjs::hide(id = "pfs")
              shinyjs::hide(id = "pfc")
              shinyjs::hide(id = "pfa")
              shinyjs::hide(id = "clc1")
              shinyjs::hide(id = "cls1")
              shinyjs::hide(id = "cla1")
              shinyjs::hide(id = "tba")
              shinyjs::hide(id = "tbc")
              shinyjs::hide(id = "tbs")
              shinyjs::show(id = "rk")
              shinyjs::hide(id = "rs")
              shinyjs::hide(id = "rc")
              shinyjs::hide(id = "ra")
            })
            
            observeEvent(input$complete, {
              shinyjs::hide(id = "g2")
              shinyjs::hide(id = "dist1")
              shinyjs::show(id = "dist2")
              shinyjs::hide(id = "dist3")
              shinyjs::hide(id = "gp")
              shinyjs::hide(id = "g1")
              shinyjs::show(id = "c1")
              shinyjs::hide(id = "a1")
              shinyjs::hide(id = "k1")
              shinyjs::hide(id = "km")
              shinyjs::hide(id = "k2")
              shinyjs::hide(id = "pt")
              shinyjs::hide(id = "pfk")
              shinyjs::hide(id = "tb")
              shinyjs::hide(id = "tb1")
              shinyjs::hide(id = "pfs")
              shinyjs::show(id = "pfc")
              shinyjs::hide(id = "pfa")
              shinyjs::show(id = "clc1")
              shinyjs::hide(id = "cls1")
              shinyjs::hide(id = "cla1")
              shinyjs::hide(id = "tba")
              shinyjs::show(id = "tbc")
              shinyjs::hide(id = "tbs")
              shinyjs::hide(id = "rk")
              shinyjs::hide(id = "rs")
              shinyjs::show(id = "rc")
              shinyjs::hide(id = "ra")
            })
            
            observeEvent(input$average, {
              shinyjs::hide(id = "g2")
              shinyjs::hide(id = "dist1")
              shinyjs::hide(id = "dist2")
              shinyjs::show(id = "dist3")
              shinyjs::hide(id = "gp")
              shinyjs::hide(id = "g1")
              shinyjs::hide(id = "c1")
              shinyjs::show(id = "a1")
              shinyjs::hide(id = "k1")
              shinyjs::hide(id = "k2")
              shinyjs::hide(id = "km")
              shinyjs::hide(id = "pt")
              shinyjs::hide(id = "pfk")
              shinyjs::hide(id = "tb")
              shinyjs::hide(id = "tb1")
              shinyjs::hide(id = "pfs")
              shinyjs::hide(id = "pfc")
              shinyjs::show(id = "pfa")
              shinyjs::hide(id = "clc1")
              shinyjs::hide(id = "cls1")
              shinyjs::show(id = "cla1")
              shinyjs::show(id = "tba")
              shinyjs::hide(id = "tbc")
              shinyjs::hide(id = "tbs")
              shinyjs::hide(id = "rk")
              shinyjs::hide(id = "rs")
              shinyjs::hide(id = "rc")
              shinyjs::show(id = "ra")
            })
            
            
            
            output$calinski <- renderText({ 
              datafix=datin(1)
              fit <- cascadeKM(datafix, 1, 10, iter = 1000)
              calinski.best <- as.numeric(which.max(fit$results[2,]))
              paste("Calinski criterion optimal number of clusters:",calinski.best)})
            
            output$affinity <- renderText({ 
              datafix=datin(1)
              pamk.best <- pamk(datafix)
              paste("number of clusters estimated by optimum average silhouette width:", pamk.best$nc) })
            
            output$pfk <- renderDataTable({
              
              dat$retail_and_recreation[is.na(dat$retail_and_recreation)] <- median(dat$retail_and_recreation, na.rm = TRUE)
              dat$grocery_and_pharmacy[is.na(dat$grocery_and_pharmacy)] <- median(dat$grocery_and_pharmacy, na.rm = TRUE)
              dat$parks[is.na(dat$parks)] <- mean(dat$parks, na.rm = TRUE)
              dat$transit_stations[is.na(dat$transit_stations)] <- median(dat$transit_stations, na.rm = TRUE)
              dat$workplaces[is.na(dat$workplaces)] <- median(dat$workplaces, na.rm = TRUE)
              dat$residential[is.na(dat$residential)] <- median(dat$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(dat$retail_and_recreation,dat$grocery_and_pharmacy,
                                      dat$parks,dat$transit_stations,
                                      dat$workplaces,dat$residential),
                                 by=list(dat$reg1,dat$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","Provinsi1", "retail","grocery","parks","transit","workplaces","residental")
              sum_dt1=sum_dt[order(sum_dt$Provinsi),]
              datafix1=sum_dt1[!grepl("Indonesia", sum_dt1$Provinsi),]
              dataclus1=datafix1[,-(1:2)]
              datafix <- scale(dataclus1) #standarisasi data
              km_fit = kmeans(datafix,centers = 2,iter.max = 300 )
              new_data <- cbind(dataclus1,data.frame(km_fit$cluster))
              new_data$km_fit.cluster <- as.factor(new_data$km_fit.cluster)
              a=icdrate(new_data,length(new_data),2)
              b= data.table( 
                Perhitungan= c("SSW","SST","SSB","Rsq","icdrate","Pseudo-f"),
                Nilai = c(a$SSW,a$SST,a$SSB,a$Rsq,a$icdrate,a$pseudof)
              )
              as.data.table(b)
            })
            
            output$tbk <- renderDataTable({
              dat$retail_and_recreation[is.na(dat$retail_and_recreation)] <- median(dat$retail_and_recreation, na.rm = TRUE)
              dat$grocery_and_pharmacy[is.na(dat$grocery_and_pharmacy)] <- median(dat$grocery_and_pharmacy, na.rm = TRUE)
              dat$parks[is.na(dat$parks)] <- mean(dat$parks, na.rm = TRUE)
              dat$transit_stations[is.na(dat$transit_stations)] <- median(dat$transit_stations, na.rm = TRUE)
              dat$workplaces[is.na(dat$workplaces)] <- median(dat$workplaces, na.rm = TRUE)
              dat$residential[is.na(dat$residential)] <- median(dat$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(dat$retail_and_recreation,dat$grocery_and_pharmacy,
                                      dat$parks,dat$transit_stations,
                                      dat$workplaces,dat$residential),
                                 by=list(dat$reg1,dat$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","Provinsi1", "retail","grocery","parks","transit","workplaces","residental")
              sum_dt1=sum_dt[order(sum_dt$Provinsi),]
              datafix1=sum_dt1[!grepl("Indonesia", sum_dt1$Provinsi),]
              dataclus=datafix1[,-(1:2)]
              datafix <- scale(dataclus) #standarisasi data
              km_fit = kmeans(datafix,centers = 2,iter.max = 300 )
              new_data <- cbind(rownames(datafix1),datafix1,data.frame(km_fit$cluster))
              tab=as.data.table(new_data[,-c(2,4,5,6,7,8,9)])
              colnames(tab)=c("No.Urut","Provinsi","Cluster")
              tab
            })
            output$pfs <- renderDataTable({
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist1)
              hc1 <- hclust(d, method = "single" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              datatable(b) %>%  
                formatStyle('nama',  target = 'row', backgroundColor = styleEqual(a$nama,'#66a3ff'))
            })
            
            output$pfc <- renderDataTable({
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist2)
              hc1 <- hclust(d, method = "complete" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              datatable(b) %>%  
                formatStyle('nama',  target = 'row', backgroundColor = styleEqual(a$nama,'#66a3ff'))
            })
            
            output$pfa <- renderDataTable({
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist3)
              hc1 <- hclust(d, method = "average" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              datatable(b) %>%  
                formatStyle('nama',  target = 'row', backgroundColor = styleEqual(a$nama,'#66a3ff'))
            })
            
            output$tbk1 <- renderDataTable({
              dat$retail_and_recreation[is.na(dat$retail_and_recreation)] <- median(dat$retail_and_recreation, na.rm = TRUE)
              dat$grocery_and_pharmacy[is.na(dat$grocery_and_pharmacy)] <- median(dat$grocery_and_pharmacy, na.rm = TRUE)
              dat$parks[is.na(dat$parks)] <- mean(dat$parks, na.rm = TRUE)
              dat$transit_stations[is.na(dat$transit_stations)] <- median(dat$transit_stations, na.rm = TRUE)
              dat$workplaces[is.na(dat$workplaces)] <- median(dat$workplaces, na.rm = TRUE)
              dat$residential[is.na(dat$residential)] <- median(dat$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(dat$retail_and_recreation,dat$grocery_and_pharmacy,
                                      dat$parks,dat$transit_stations,
                                      dat$workplaces,dat$residential),
                                 by=list(dat$reg1,dat$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","Provinsi1", "retail","grocery","parks","transit","workplaces","residental")
              sum_dt1=sum_dt[order(sum_dt$Provinsi),]
              datafix1=sum_dt1[!grepl("Indonesia", sum_dt1$Provinsi),]
              dataclus=datafix1[,-(1:2)]
              datafix <- scale(dataclus) #standarisasi data
              km_fit = kmeans(datafix,centers = 2,iter.max = 300 )
              new_data <- cbind(rownames(datafix1),datafix1,data.frame(km_fit$cluster))
              tab=as.data.table(new_data[,-c(1,2,4,5,6,7,8,9)])
              colnames(tab)=c("Provinsi","Cluster")
              tab
            })
            
            output$cls1=renderPlot({
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist1)
              hc1 <- hclust(d, method = "single" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              k=as.numeric(str_sub(a$nama, -1, -1))
              sub_grp1 <- cutree(hc1, k = k)
              theme_set(theme_classic())
              fviz_cluster(list(data = datafix, cluster = sub_grp1))+ theme_set(theme_classic())
            })
            
            output$clc1=renderPlot({
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist2)
              hc1 <- hclust(d, method = "complete" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              k=as.numeric(str_sub(a$nama, -1, -1))
              sub_grp1 <- cutree(hc1, k = k)
              theme_set(theme_classic())
              fviz_cluster(list(data = datafix, cluster = sub_grp1))+ theme_set(theme_classic())
            })
            output$cla1=renderPlot({
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist3)
              hc1 <- hclust(d, method = "average" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              k=as.numeric(str_sub(a$nama, -1, -1))
              theme_set(theme_classic())
              sub_grp1 <- cutree(hc1, k = k)
              fviz_cluster(list(data = datafix, cluster = sub_grp1))+ theme_set(theme_classic())
            })
            
            
            output$tba <- renderDataTable({
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist3)
              hc1 <- hclust(d, method = "average" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              k=as.numeric(str_sub(a$nama, -1, -1))
              sub_grp1 <- cutree(hc1, k = k)
              dt= dataclus %>%
                mutate(cluster = sub_grp1) 
              datatable( dt['cluster'])
            })
            output$tbs <- renderDataTable({
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist1)
              hc1 <- hclust(d, method = "single" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              k=as.numeric(str_sub(a$nama, -1, -1))
              sub_grp1 <- cutree(hc1, k = k)
              dt= dataclus %>%
                mutate(cluster = sub_grp1) 
              datatable( dt['cluster'])
            })
            output$tbc <- renderDataTable({
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist2)
              hc1 <- hclust(d, method = "complete" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              k=as.numeric(str_sub(a$nama, -1, -1))
              sub_grp1 <- cutree(hc1, k = k)
              dt= dataclus %>%
                mutate(cluster = sub_grp1) 
              datatable( dt['cluster'])
            })
            
            output$rk <- renderDataTable({ 
              dat$retail_and_recreation[is.na(dat$retail_and_recreation)] <- median(dat$retail_and_recreation, na.rm = TRUE)
              dat$grocery_and_pharmacy[is.na(dat$grocery_and_pharmacy)] <- median(dat$grocery_and_pharmacy, na.rm = TRUE)
              dat$parks[is.na(dat$parks)] <- mean(dat$parks, na.rm = TRUE)
              dat$transit_stations[is.na(dat$transit_stations)] <- median(dat$transit_stations, na.rm = TRUE)
              dat$workplaces[is.na(dat$workplaces)] <- median(dat$workplaces, na.rm = TRUE)
              dat$residential[is.na(dat$residential)] <- median(dat$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(dat$retail_and_recreation,dat$grocery_and_pharmacy,
                                      dat$parks,dat$transit_stations,
                                      dat$workplaces,dat$residential),
                                 by=list(dat$reg1,dat$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","Provinsi1", "retail","grocery","parks","transit","workplaces","residental")
              sum_dt1=sum_dt[order(sum_dt$Provinsi),]
              datafix1=sum_dt1[!grepl("Indonesia", sum_dt1$Provinsi),]
              dataclus=datafix1[,-(1:2)]
              datafix <- scale(dataclus) #standarisasi data
              km_fit = kmeans(datafix,centers = 2,iter.max = 300 )
              dt=data.frame(dataclus) %>%
                mutate(Cluster = km_fit$cluster) %>%
                group_by(Cluster) %>%
                summarise_all("mean")
              t_dt <- transpose(dt)
              colnames(t_dt) <- c("Cluster 2", "Cluster 1")
              rownames(t_dt) <- colnames(dt)
              t_dt[-1,]
              })
            
            output$rs <- renderDataTable({ 
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist1)
              hc1 <- hclust(d, method = "single" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              k=as.numeric(str_sub(a$nama, -1, -1))
              sub_grp1 <- cutree(hc1, k = k)
              dt=data.frame(datafix) %>%
                mutate(Cluster = sub_grp1) %>%
                group_by(Cluster) %>%
                summarise_all("mean")
              dt$Cluster=as.factor(dt$Cluster)
              t_dt <- transpose(dt)
              for (i in 1:k)
              {     colnames(t_dt)[i] <- paste("Cluster ",i,sep="")}
              
              
              rownames(t_dt) <- colnames(dt)
              t_dt[-1,] })
            
            output$rc <- renderDataTable({ 
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist1)
              hc1 <- hclust(d, method = "complete" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              k=as.numeric(str_sub(a$nama, -1, -1))
              sub_grp1 <- cutree(hc1, k = k)
              dt=data.frame(datafix) %>%
                mutate(Cluster = sub_grp1) %>%
                group_by(Cluster) %>%
                summarise_all("mean")
              dt$Cluster=as.factor(dt$Cluster)
              t_dt <- transpose(dt)
              colnames(t_dt) <- c("Cluster 1", "Cluster 2")
              rownames(t_dt) <- colnames(dt)
              t_dt[-1,] 
              })
            
            output$ra <- renderDataTable({ 
              data$retail_and_recreation[is.na(data$retail_and_recreation)] <- median(data$retail_and_recreation, na.rm = TRUE)
              data$grocery_and_pharmacy[is.na(data$grocery_and_pharmacy)] <- median(data$grocery_and_pharmacy, na.rm = TRUE)
              data$parks[is.na(data$parks)] <- mean(data$parks, na.rm = TRUE)
              data$transit_stations[is.na(data$transit_stations)] <- median(data$transit_stations, na.rm = TRUE)
              data$workplaces[is.na(data$workplaces)] <- median(data$workplaces, na.rm = TRUE)
              data$residential[is.na(data$residential)] <- median(data$residential, na.rm = TRUE)
              
              sum_dt <-aggregate(list(data$retail_and_recreation,data$grocery_and_pharmacy,
                                      data$parks,data$transit_stations,
                                      data$workplaces,data$residential),
                                 by=list(data$sub_region_1),FUN=mean)
              colnames(sum_dt)=c("Provinsi","retail","grocery","parks","transit","workplaces","residental")
              datafix1=sum_dt[!grepl("Indonesia", sum_dt$Provinsi),]
              dataclus=datafix1[,-1]
              rownames(dataclus)=datafix1$Provinsi
              datafix <- scale(dataclus) #standarisasi data
              d <- dist(datafix, method = input$dist1)
              hc1 <- hclust(d, method = "average" )
              sub_grp2<- cutree(hc1, k = 2) #jumlah k diganti sesuai pemotongan
              sub_grp3<- cutree(hc1, k = 3)
              sub_grp4<- cutree(hc1, k = 4)
              new_data1 <- cbind(dataclus,sub_grp2)
              a1= icdrate(new_data1,length(new_data1),2)
              new_data2 <- cbind(dataclus,sub_grp3)
              a1=as.data.frame(a1)
              a2= icdrate(new_data2,length(new_data2),3)
              new_data3 <- cbind(dataclus,sub_grp4)
              a2=as.data.frame(a2)
              a3= icdrate(new_data3,length(new_data3),4)
              a3=as.data.frame(a3)
              c= rbind(a1,a2,a3)
              b=cbind(nama=c("Jumlah Cluster 2","Jumlah Cluster 3","Jumlah Cluster 4"),c)
              a=b[which(b$pseudof == max(b$pseudof)), ]
              k=as.numeric(str_sub(a$nama, -1, -1))
              sub_grp1 <- cutree(hc1, k = k)
              dt=data.frame(datafix) %>%
                mutate(Cluster = sub_grp1) %>%
                group_by(Cluster) %>%
                summarise_all("mean")
              dt$Cluster=as.factor(dt$Cluster)
              t_dt <- transpose(dt)
              colnames(t_dt) <- c("Cluster 1", "Cluster 2")
              rownames(t_dt) <- colnames(dt)
              t_dt[-1,]
              })
          })


