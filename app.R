library(MASS)
#library('raster')

ui <- fluidPage(
  tags$head(tags$script(src="script.js")),
  tags$head(tags$link(rel="stylesheet", 
                      type="text/css",
                      href="style.css")),
  tags$head(
    tags$link(rel = "stylesheet", href = "typekit.css")),
  
  htmlTemplate("./header.html",
               doc = includeMarkdown("Dokumentasjon.Rmd")
  ),  
  sidebarLayout(
    sidebarPanel(
      
      htmlTemplate("form.html")
      
      #fileInput('file1', 'Last opp csv fil (beskrivelse på hvit område)',
      #          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
      #,
      #tags$hr(),
      #tags$br("Skriv inn lokalitetsnummer for å få tak i smittepress"),
      #textInput("loknr", "5-sifret lokalitetsnummer (eller 0)", "0"),
      #tags$br("Skriv inn enten år og ukenummer eller bruk 0 for nåværende uke. Eksempel uke 1 i 2016: 201601"),
      #textInput("tid", "6-sifret ukenummer (eller 0)", "0"),
      #tags$br("For å endre størrelsen på smittepresset, må lokalitetsnummeret først settes til 0. Smittepress tar verdier mellom 0 og 22. For eksempel 14.7 som tilsvarer 90 persentilen for alle ukentlige smittepressverdier fra aktive lokaliteter i årene 2012 til og  med 2015."),
      #numericInput("sm", "Smittepress", 15, min = 0, max = 22),
      #selectInput("hele", "merdvis behandling eller hele lokaliteten:", c("merdvis" = "Merdvis", "hele anlegget" = "Hele"), "Merdvis") 
      
    ),
    mainPanel(
    	uiOutput("mainblock")
    )
  )
)


ParameterHunnlusSann <- c(-3.22963, 0.89694)
ParametereMerdCount <- c(-0.11920679,  0.64625410,  0.09369016,  0.19196262,  0.35336360,  0.19125739, -0.11604661)
ParametereMerdZero <- c(0.4520806, -1.3697959, -0.3342084, -0.6574109, -0.2297210,  0.3067283)
thetaHele <- 1.997114
ParametereHeleCount <- c( 0.62806465,  0.24380189,  0.05246029,  0.53724600,  0.14725647,  0.29432465,  0.15317690, -0.08139975)
ParametereHeleZero <- c(-1.3095828, -0.7798661, -1.0818692, -0.2903852, -0.5847494, -0.1541673,  0.2945743)
thetaMerd <- 1.693027
d <- read.table("./MobileTotaltFra2012SisteUker.txt", dec = ",")
d0 <- read.csv("./lusedata.csv", header = T, dec = ",", sep = ";")
dAlle <- read.csv("./MobileTotaltFra2012.txt", header = T, dec = ",", sep = "\t")

tail(names(dAlle),3)

countHele <- function(MerderM1, LokM1, VektMerd, LM1, ReFis, LnSmP){
  LogAdd <- 0.1
  co <- ParametereHeleCount[1]+(ParametereHeleCount[2]*log(MerderM1+LogAdd))+ (ParametereMerdCount[3]*(log(MerderM1+LogAdd))^2)+(ParametereHeleCount[4]*log(LokM1+LogAdd))+
    (ParametereHeleCount[5]*VektMerd) + (ParametereHeleCount[6]*LM1) +(ParametereHeleCount[7]*LnSmP)+ (ParametereHeleCount[8]*ReFis)
  return(co)
}

countMerd <- function(MerderM1, VektMerd, LM1, ReFis, LnSmP){
  LogAdd <- 0.1
  co <- ParametereMerdCount[1]+(ParametereMerdCount[2]*log(MerderM1+LogAdd))+ (ParametereMerdCount[3]*(log(MerderM1+LogAdd))^2)+
    (ParametereMerdCount[4]*VektMerd) + (ParametereMerdCount[5]*LM1) +(ParametereMerdCount[6]*LnSmP)+ (ParametereMerdCount[7]*ReFis)
  return(co)
}

zeroHele <- function(MerderM1, LokM1, VektMerd, LM1,  ReFis, LnSmP){
  LogAdd <- 0.1
  ze <- ParametereHeleZero[1]+(ParametereHeleZero[2]*log(MerderM1+LogAdd))+(ParametereHeleZero[3]*log(LokM1+LogAdd))+
    (ParametereHeleZero[4]*VektMerd) + (ParametereHeleZero[5]*LM1) +(ParametereHeleZero[6]*LnSmP)+ (ParametereHeleZero[7]*ReFis)
  return(ze)
}

zeroMerd <- function(MerderM1, VektMerd, LM1,  ReFis, LnSmP){
  LogAdd <- 0.1
  ze <- ParametereMerdZero[1]+(ParametereMerdZero[2]*log(MerderM1+LogAdd))+
    (ParametereMerdZero[3]*VektMerd) + (ParametereMerdZero[4]*LM1) +(ParametereMerdZero[5]*LnSmP)+ (ParametereMerdZero[6]*ReFis)
  return(ze)
}

convData <- function(value){
	outvalue <- as.numeric(sub(",", ".", value, fixed = TRUE))
  return(outvalue)
}


server <- function(input, output){
  


	drawInfo <- function(output, mydata) {
		output$mainblock <- renderUI({
		  output$dt.output <- renderTable({mydata})
		  output$text <- renderText(paste("Smittepresset på lokalitet ", input$loknr, " denne uken (uke ", mytid(), ") er ", round(log(d[as.character(input$loknr),1] + 1),2), ". Til sammenligning er gjennomsnittlig smittepress for alle lokaliteter i Norge ", round(mean(log(d[,1] + 1)),2), " og varierer mellom ", round(quantile(log(d[,1] + 1), 0.05),2), "(5% prosentil) og ", round(quantile(log(d[,1] + 1), 0.95),2), "(95% prosentil).")) 
		  output$plot1 <- renderPlot({
		    inndata <- mydata
		    plotLok(loknr = input$loknr, valgtSm = input$sm, tid = mytid(), innlestData =  inndata, plotnr = 1, hele = myhele)
		  })
		  
		  output$plot2 <- renderPlot({          
		    inndata <- mydata
		    plotLok(loknr = input$loknr, valgtSm = input$sm, tid = mytid(), innlestData = inndata, plotnr = 2, hele =  myhele)
		  })
		  
		  output$plot3 <- renderPlot({          
		    inndata <- mydata
		    plotLok(loknr = input$loknr, valgtSm = input$sm, tid = mytid(), innlestData = inndata, plotnr = 3, hele =  myhele)
		  })
		  
		  output$plot4 <- renderPlot({          
		    inndata <- mydata
		    plotLok(loknr = input$loknr, valgtSm = input$sm, tid = mytid(), innlestData = inndata, plotnr = 4, hele =  myhele)
		  })
		  div(id="mainblock",
		    tableOutput(outputId = "table.output"),
		    textOutput("text"),
		    tags$br(""),
		    plotOutput("plot1"),
		    plotOutput("plot2"),
		    plotOutput("plot3"),
		    plotOutput("plot4")
		  )
	  })
	}
	
	


  myhele <- "Merdvis"
  
  mytid <- reactive({
    week <- input$week
    year <- input$year
    stid <- "0"
    if (!(input$week == "0")) {
      if(as.numeric(week) > 9)
        stid <- paste(year, week, sep="")
      else {
        stid <- paste(year, "0", week, sep="")
      }
    }
    return(stid)
  })
    
  
  mydata <- reactiveVal()
  #newdata = matrix(c(0, 0, 0, 0), nrow=1, ncol=4) 
  #colnames(newdata) <- c("Mobile.lus.pr.fisk.merd","Vekt.merd","Rensefisk.merd","Fastsittende.merd")
#  mydata(d0)
  
  observeEvent(input$file1,{ 
    inFile <- input$file1
    
    if (is.null(inFile)){
      tbl <- d0
    }else{
      tbl <- read.csv(inFile$datapath, header = T, sep = ";", dec = ",") #, header=input$header, sep=input$sep,  dec = input$dec)
    }

    mydata(tbl)
    drawInfo(output, mydata())
  })
  
  observeEvent({
    input$merd1_loop
    input$merd2_loop
    input$merd3_loop
    input$merd4_loop
    input$merd5_loop
    input$merd6_loop
    input$merd7_loop
    input$merd8_loop
    input$merd9_loop
    input$merd10_loop
    input$merd11_loop
    input$merd12_loop
    input$merd13_loop
    input$merd14_loop
    input$merd15_loop
    input$merd16_loop
    
    input$merd1_weight
    input$merd2_weight
    input$merd3_weight
    input$merd4_weight
    input$merd5_weight
    input$merd6_weight
    input$merd7_weight
    input$merd8_weight
    input$merd9_weight
    input$merd10_weight
    input$merd11_weight
    input$merd12_weight
    input$merd13_weight
    input$merd14_weight
    input$merd15_weight
    input$merd16_weight
    
    input$merd1_fish
    input$merd2_fish
    input$merd3_fish
    input$merd4_fish
    input$merd5_fish
    input$merd6_fish
    input$merd7_fish
    input$merd8_fish
    input$merd9_fish
    input$merd10_fish
    input$merd11_fish
    input$merd12_fish
    input$merd13_fish
    input$merd14_fish
    input$merd15_fish
    input$merd16_fish
    
    input$merd1_stubborn
    input$merd2_stubborn
    input$merd3_stubborn
    input$merd4_stubborn
    input$merd5_stubborn
    input$merd6_stubborn
    input$merd7_stubborn
    input$merd8_stubborn
    input$merd9_stubborn
    input$merd10_stubborn
    input$merd11_stubborn
    input$merd12_stubborn
    input$merd13_stubborn
    input$merd14_stubborn
    input$merd15_stubborn
    input$merd16_stubborn
    },{
      
    if (input$merddataradio == "manual"){
      newrow <- c(convData(input$merd1_loop),convData(input$merd1_weight),convData(input$merd1_fish),convData(input$merd1_stubborn))
      newdata = matrix(newrow, nrow=1, ncol=4, byrow = TRUE) 
      colnames(newdata) <- c("Mobile.lus.pr.fisk.merd","Vekt.merd","Rensefisk.merd","Fastsittende.merd")
      
      if((input$merd2_loop != "0") || (input$merd2_weight != "0") || (input$merd2_fish != "0") || (input$merd2_stubborn != "0")) {
        newdata <- rbind(newdata, c(convData(input$merd2_loop), convData(input$merd2_weight), convData(input$merd2_fish), convData(input$merd2_stubborn)))
      }
      if((input$merd3_loop != "0") || (input$merd3_weight != "0") || (input$merd3_fish != "0") || (input$merd3_stubborn != "0")) {
        newdata <- rbind(newdata, c(convData(input$merd3_loop), convData(input$merd3_weight), convData(input$merd3_fish), convData(input$merd3_stubborn)))
      }
      if((input$merd4_loop != "0") || (input$merd4_weight != "0") || (input$merd4_fish != "0") || (input$merd4_stubborn != "0")) {
        newdata <- rbind(newdata, c(convData(input$merd4_loop), convData(input$merd4_weight), convData(input$merd4_fish), convData(input$merd4_stubborn)))
      }
      if((input$merd5_loop != "0") || (input$merd5_weight != "0") || (input$merd5_fish != "0") || (input$merd5_stubborn != "0")) {
        newdata <- rbind(newdata, c(convData(input$merd5_loop), convData(input$merd5_weight), convData(input$merd5_fish), convData(input$merd5_stubborn)))
      }
      if((input$merd6_loop != "0") || (input$merd6_weight != "0") || (input$merd6_fish != "0") || (input$merd6_stubborn != "0")) {
        newdata <- rbind(newdata, c(convData(input$merd6_loop), convData(input$merd6_weight), convData(input$merd6_fish), convData(input$merd6_stubborn)))
      }
      if((input$merd7_loop != "0") || (input$merd7_weight != "0") || (input$merd7_fish != "0") || (input$merd7_stubborn != "0")) {
        newdata <- rbind(newdata, c(convData(input$merd7_loop), convData(input$merd7_weight), convData(input$merd7_fish), convData(input$merd7_stubborn)))
      }
      if((input$merd8_loop != "0") || (input$merd8_weight != "0") || (input$merd8_fish != "0") || (input$merd8_stubborn != "0")) {
        newdata <- rbind(newdata, c(convData(input$merd8_loop), convData(input$merd8_weight), convData(input$merd8_fish), convData(input$merd8_stubborn)))
      }
      if((input$merd9_loop != "0") || (input$merd9_weight != "0") || (input$merd9_fish != "0") || (input$merd9_stubborn != "0")) {
        newdata <- rbind(newdata, c(convData(input$merd9_loop), convData(input$merd9_weight), convData(input$merd9_fish), convData(input$merd9_stubborn)))
      }
      if((input$merd10_loop != "0") || (input$merd10_weight != "0") || (input$merd10_fish != "0") || (input$merd10_stubborn != "0")) {
        newdata <- rbind(newdata, c(convData(input$merd10_loop), convData(input$merd10_weight), convData(input$merd10_fish), convData(input$merd10_stubborn)))
      }
      if((input$merd12_loop != "0") || (input$merd12_weight != "0") || (input$merd12_fish != "0") || (input$merd12_stubborn != "0")) {
        newdata <- rbind(newdata, c(convData(input$merd12_loop), convData(input$merd12_weight), convData(input$merd12_fish), convData(input$merd12_stubborn)))
      }
      if((input$merd13_loop != "0") || (input$merd13_weight != "0") || (input$merd13_fish != "0") || (input$merd13_stubborn != "0")) {
        newdata <- rbind(newdata, c(convData(input$merd13_loop), convData(input$merd13_weight), convData(input$merd13_fish), convData(input$merd13_stubborn)))
      }
      if((input$merd14_loop != "0") || (input$merd14_weight != "0") || (input$merd14_fish != "0") || (input$merd14_stubborn != "0")) {
        newdata <- rbind(newdata, c(convData(input$merd14_loop), convData(input$merd14_weight), convData(input$merd14_fish), convData(input$merd14_stubborn)))
      }
      if((input$merd15_loop != "0") || (input$merd15_weight != "0") || (input$merd15_fish != "0") || (input$merd15_stubborn != "0")) {
        newdata <- rbind(newdata, c(convData(input$merd15_loop), convData(input$merd15_weight), convData(input$merd15_fish), convData(input$merd15_stubborn)))
      }
      if((input$merd16_loop != "0") || (input$merd16_weight != "0") || (input$merd16_fish != "0") || (input$merd16_stubborn != "0")) {
        newdata <- rbind(newdata, c(convData(input$merd16_loop), convData(input$merd16_weight), convData(input$merd16_fish), convData(input$merd16_stubborn)))
      }            
      mydata(newdata)
      drawInfo(output, mydata())
    }
  })
  
  trekk <- function(p0, pC, Theta){
    e0 <- rbinom(1, 1, p0)
    if(e0 == 1){
      return(0)
    } else {
      return(rnegbin(1,pC,Theta))
    }
  }
  
  plotLok <- function(loknr, valgtSm, tid, innlestData, plotnr, hele){
    if(loknr == "0"){
      LnSmP <- valgtSm
    }   else {
      if(as.character(loknr) %in% rownames(d)){
        if(tid == 0){
          LnSmP  <- log(d[as.character(loknr),1] + 1)
        } else {
          if(paste("X", tid, sep = "") %in% colnames(dAlle)){
            LnSmP <- log(dAlle[as.character(loknr), paste("X",tid, sep = "")])
          }
        }
      } 
    }  
    MerderM1 <- innlestData[,1]
    VektMerd <- innlestData[,2]
    ReFis <- innlestData[,3]
    LM1 <- innlestData[,4]
    LokM1 <- mean(MerderM1) 
    antMerder <- length(MerderM1)
    HeleMerd <- switch(myhele, 
                       "Hele" = TRUE, 
                       "Merdvis" = FALSE) 
    
    
    if(HeleMerd == TRUE){
      yCount <- countHele(MerderM1 = MerderM1, LokM1 = LokM1, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
      yZero <- zeroHele(MerderM1 = MerderM1, LokM1 = LokM1, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
    } else {
      yCount <- countMerd(MerderM1 = MerderM1, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
      yZero <- zeroMerd(MerderM1 = MerderM1, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
    }        
    pCount <- exp(yCount)
    pZero <- exp(yZero)/(1+exp(yZero))
    
    MerderUke1T <- rep(NA, length(MerderM1))
    MUke1upT <- rep(NA,length(MerderM1))
    MUke1lowT <- rep(NA,length(MerderM1))
    
    for(i in 1:length(MerderM1)){
      obs <- replicate(1000, trekk(p0 = pZero[i], pC = pCount[i], Theta = thetaHele))
      MerderUke1T[i] <- mean(obs)/30
      MUke1upT[i] <- quantile(obs, 0.25)/30
      MUke1lowT[i] <- quantile(obs, 0.75)/30
    } 
    
    if(HeleMerd == TRUE){
      yCount <- countHele(MerderM1 = MerderUke1T, LokM1 = mean(MerderUke1T), VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
      yZero <- zeroHele(MerderM1 = MerderUke1T, LokM1 = mean(MerderUke1T), VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
    } else {
      yCount <- countMerd(MerderM1 = MerderUke1T, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
      yZero <- zeroMerd(MerderM1 = MerderUke1T, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
    }
    pCount <- exp(yCount)
    pZero <- exp(yZero)/(1+exp(yZero))
    
    MerderUke2T <- rep(NA, length(MerderM1))
    MUke2upT <- rep(NA,length(MerderM1))
    MUke2lowT <- rep(NA,length(MerderM1))
    for(i in 1:length(MerderM1)){
      obs <- replicate(1000, trekk(p0 = pZero[i], pC = pCount[i], Theta = thetaHele))
      MerderUke2T[i] <- mean(obs)/30
      MUke2upT[i] <- quantile(obs, 0.25)/30
      MUke2lowT[i] <- quantile(obs, 0.75)/30
    } 
    
    
    if(HeleMerd == TRUE){
      yCount <- countHele(MerderM1 = MerderUke2T, LokM1 = mean(MerderUke2T), VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
      yZero <- zeroHele(MerderM1 = MerderUke2T, LokM1 = mean(MerderUke2T), VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
    } else {
      yCount <- countMerd(MerderM1 = MerderUke2T, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
      yZero <- zeroMerd(MerderM1 = MerderUke2T, VektMerd = VektMerd, LM1 = LM1, ReFis = ReFis, LnSmP = LnSmP)
    }
    pCount <- exp(yCount)
    pZero <- exp(yZero)/(1+exp(yZero))
    
    MerderUke3T <- rep(NA, length(MerderM1))
    MUke3upT <- rep(NA,length(MerderM1))
    MUke3lowT <- rep(NA,length(MerderM1))
    for(i in 1:length(MerderM1)){
      obs <- replicate(1000, trekk(p0 = pZero[i], pC = pCount[i], Theta = thetaHele))
      MerderUke3T[i] <- mean(obs)/30
      MUke3upT[i] <- quantile(obs, 0.25)/30
      MUke3lowT[i] <- quantile(obs, 0.75)/30
    } 
    
    pdat <- rbind(MerderM1, MerderUke1T, MUke1upT, MUke1lowT, MerderUke2T, MUke2upT ,MUke2lowT, MerderUke3T, MUke3upT, MUke3lowT)
    xtid <- c(0, 1, 2, 3)
    Min <- 0
    Max <- 7
    p50U1 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MerderUke1T)
    p75U1 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MUke1lowT)
    p25U1 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MUke1upT)
    p50U2 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MerderUke2T)
    p75U2 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MUke2lowT)
    p25U2 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MUke2upT)
    p50U3 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MerderUke3T)
    p75U3 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MUke3lowT)
    p25U3 <- 100*plogis(ParameterHunnlusSann[1] + ParameterHunnlusSann[2]*MUke3upT)
    par(mfrow = c(1,4))
    if(antMerder > (plotnr - 1)*4){
      i <- (plotnr - 1)*4 + 1
      plot (xtid, pdat[c(1,2,5,8),i], type = 'l', ylim = c(Min,Max), lwd = 3, col = "red", ylab = "Lus.pr.fisk.merd", xlab = "uker framover", main = paste("Merd",  i, sep = " "), axes = F, cex.lab = 1.8, cex.main = 1.8)
      axis(1, c(0,1,2,3), c(0,1,2,3))
      axis(2)
      lines(xtid[-1], pdat[c(3,6,9),i], type = 'l')
      lines(xtid[-1], pdat[c(4,7,10),i], type = 'l')
      
      legend(x = "topleft", c(paste("75%: ", round(p75U1[i],1), ", ", round(p75U2[i],1), ", ", round(p75U3[i],1)), paste("50%: ", round(p50U1[i],1), ", ", round(p50U2[i],1), ", ", round(p50U3[i],1)), paste("25%: ", round(p25U1[i],1), ", ", round(p25U2[i],1), ", ", round(p25U3[i],1))))
      if(antMerder > (plotnr - 1)*4 + 1){
        for(i in 2:min(antMerder, 4*plotnr)){
          plot (xtid, pdat[c(1,2,5,8),i], type = 'l', ylim = c(Min,Max), lwd = 3, col = "red", ylab = "", xlab = "uker framover", main = paste("Merd",  i, sep = " "), axes = F, cex.lab = 1.8, cex.main = 1.8)
          axis(1, c(0,1,2,3), c(0,1,2,3))
          axis(2)
          lines(xtid[-1], pdat[c(3,6,9),i], type = 'l')
          lines(xtid[-1], pdat[c(4,7,10),i], type = 'l')
          legend(x = "topleft", c(paste("75%: ", round(p75U1[i],1), ", ", round(p75U2[i],1), ", ", round(p75U3[i],1)), paste("50%: ", round(p50U1[i],1), ", ", round(p50U2[i],1), ", ", round(p50U3[i],1)), paste("25%: ", round(p25U1[i],1), ", ", round(p25U2[i],1), ", ", round(p25U3[i],1))))
          
        }            
      }
    }
  }

	observeEvent(input$calculate, {
	  drawInfo(output, mydata())
	})
  
#  drawInfo
  
}

shinyApp(ui = ui, server = server)


