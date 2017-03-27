library(MASS)
ui <- fluidPage(
    
    sidebarLayout(
        sidebarPanel(
            fileInput('file1', 'Last opp csv fil (beskrivelse på hvit område)',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
            ,
            tags$hr(),
            tags$br("Skriv inn lokalitetsnummer for å få tak i smittepress"),
            textInput("loknr", "5-sifret lokalitetsnummer (eller 0)", "0"),
            tags$br("Skriv inn enten år og ukenummer eller bruk 0 for nåværende uke. Eksempel uke 1 i 2016: 201601"),
            textInput("tid", "6-sifret ukenummer (eller 0)", "0"),
            tags$br("For å endre størrelsen på smittepresset, må lokalitetsnummeret først settes til 0. Smittepress tar verdier mellom 0 og 22. For eksempel 14.7 som tilsvarer 90 persentilen for alle ukentlige smittepressverdier fra aktive lokaliteter i årene 2012 til og  med 2015."),
            numericInput("sm", "Smittepress", 15, min = 0, max = 22),
            selectInput("hele", "merdvis behandling eller hele lokaliteten:", c("merdvis" = "Merdvis", "hele anlegget" = "Hele"), "Merdvis"), 
            tags$br(" ")
        )
        ,
        mainPanel(
            tags$br("Tabellen med merddata kan bestå av inntil 16 merder (rader) med samme rekkefølge av kolonner som vist i tabellen under, der:"),
            tags$br(""),
            tags$p("•	Mobile.lus.pr.fisk.merd = summen av hunnlus pr. fisk og andre mobile lus pr. fisk"),
            tags$p("•	Vekt.merd = gjennomsnittsvekt pr. fisk i kilo"),
            tags$p("•	Rensefisk.merd = rensefisk benyttes(1), hvis ikke (0)"),
            tags$p("•	Fastsittende.merd = Observasjon av fastsittende i merden (1) ellers (0)"),
            tags$html("Tabellen kan opparbeides i excel hvor desimalskilletegnet må være komma. Trykk så 'lagre som' og velg filtypen semikolon-separert csv-fil, eller åpne og fyll ut "),
            tags$a("eksempelfilen.", href ="http://odin.vetinst.no/ta/pd/smittepress/lusedata.csv"), 
            tags$html(" Denne må lagres et kjent sted, for så og lastes inn ved å trykke Browse i øvre venstre hjørne"),
            tableOutput(outputId = "table.output"),
            textOutput("text"),
            tags$html("For mer informasjon om smittepress "),
            tags$a("se", href = "https://vetinst.shinyapps.io/kartapp"),
            tags$br(""),
            textOutput("tekst3"),
            tags$br(""),
            textOutput("tekst2"),
            plotOutput("plot1"),
            plotOutput("plot2"),
            plotOutput("plot3"),
            plotOutput("plot4")
          
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
d <- read.table("http://odin.vetinst.no/ta/pd/smittepress/MobileTotaltFra2012SisteUker.txt", dec = ",")
d0 <- read.csv("http://odin.vetinst.no/ta/pd/smittepress/lusedata.csv", header = T, dec = ",", sep = ";")
dAlle <- read.csv("http://odin.vetinst.no/ta/pd/smittepress/MobileTotaltFra2012.txt", header = T, dec = ",", sep = "\t")

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




server <- function(input, output){
        mydata <- reactive({    
            inFile <- input$file1
            
            if (is.null(inFile)){
                tbl <- d0
                }else{
            
            tbl <- read.csv(inFile$datapath, header = T, sep = ";", dec = ",") #, header=input$header, sep=input$sep,  dec = input$dec)
        }
            return(tbl)
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
            HeleMerd <- switch(input$hele, 
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
        
        
               
        output$table.output <- renderTable({mydata()})
        output$text <- renderText(paste("Smittepresset på lokalitet ", input$loknr, " denne uken (uke ", substr(colnames(d)[1],2,7), ") er ", round(log(d[as.character(input$loknr),1] + 1),2), ". Til sammenligning er gjennomsnittlig smittepress for alle lokaliteter i Norge ", round(mean(log(d[,1] + 1)),2), " og varierer mellom ", round(quantile(log(d[,1] + 1), 0.05),2), "(5% prosentil) og ", round(quantile(log(d[,1] + 1), 0.95),2), "(95% prosentil).")) 
        output$tekst3 <- renderText(paste("Smittepresset på lokalitet ", input$loknr, " uke ", input$tid, " er ", round(log(dAlle[as.character(input$loknr),paste("X", input$tid, sep = "")] + 1),2), ". Til sammenligning var gjennomsnittlig smittepress for alle lokaliteter i Norge ", round(mean(log(dAlle[,paste("X", input$tid, sep = "")] + 1)),2), " og varierte mellom ", round(quantile(log(dAlle[,paste("X", input$tid, sep = "")] + 1), 0.05),2), "(5% prosentil) og ", round(quantile(log(dAlle[,paste("X", input$tid, sep = "")] + 1), 0.95),2), "(95% prosentil).")) 
        output$tekst2 <- renderText("Figurene viser forventet utvikling av Mobile.lus.pr.fisk.merd (rød kurve) og 25 – 75% prosentiler for variasjon rundt forventningen (svarte kurver). Merk at modellen er utviklet basert på data fra uke 17 til 52, den vil derfor være gyldig kun i dette tidsrommet.")
        output$plot1 <- renderPlot({         
            inndata <- mydata()
            plotLok(loknr = input$loknr, valgtSm = input$sm, tid = input$tid, innlestData =  inndata, plotnr = 1, hele = input$hele)
            })
         
        output$plot2 <- renderPlot({          
            inndata <- mydata()
            plotLok(loknr = input$loknr, valgtSm = input$sm, tid = input$tid, innlestData = inndata, plotnr = 2, hele =  input$hele)
        })

        output$plot3 <- renderPlot({          
            inndata <- mydata()
            plotLok(loknr = input$loknr, valgtSm = input$sm, tid = input$tid, innlestData = inndata, plotnr = 3, hele =  input$hele)
        })
        
        output$plot4 <- renderPlot({          
            inndata <- mydata()
            plotLok(loknr = input$loknr, valgtSm = input$sm, tid = input$tid, innlestData = inndata, plotnr = 4, hele =  input$hele)
        })
        
}

shinyApp(ui = ui, server = server)


