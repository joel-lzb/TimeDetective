library(shiny)
library("ggplot2")
options(shiny.maxRequestSize=60*1024^2) # increase upload size limit of 5MB to 60MB.

addMonths= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2] # function to add months.

shinyServer(function(input, output,session) {
	
	dattab1 <- reactive ({ # prepare the table.
		inFile <- input$file1
		if (is.null(inFile)) return(NULL)

		# only accepts .tab or .csv:
		if(grepl(".csv",inFile)==TRUE){
			datatab <- read.table(inFile$datapath, header=TRUE, sep=",")
		}else{
			datatab <- read.table(inFile$datapath, header=TRUE, sep="\t")
		}
		if (datatab[["Date"]]) {
			datatab[,"Date"] <-as.Date(datatab$Date)
		}
		
		# prepare DateTime column:
		if (is.null(datatab[["Hour"]])) { # for hourly tab:
		}else{
			if (nchar(datatab$Hour[1]) > 2) # if the Hour format is in HHMM format.
				datatab[,"DateTime"]<-as.POSIXct(paste(datatab$Date,format(strptime(sprintf("%04d",datatab$Hour),"%H%M"),"%H:%M"),sep=" "),"%R")
			else { # if it is only HH integers (0 - 23) format.
				datatab[,"DateTime"]<-as.POSIXct(paste(datatab$Date,paste(datatab$Hour,"00",sep=":"),sep=" "),"%R")
			}
		}

		if (is.null(datatab[["Time"]])) { # for hourly tab in HH:mm:ss format (00:00:00 - 23:59:59). Time superceeds Hour.
		}else{
			datatab[,"DateTime"]<-as.POSIXct(paste(datatab$Date,datatab$Time,sep=" "),"%R")
		}

		datatab
	})


	output$gotFile <- reactive({
		if (is.null(dattab1())) return('')
		ansF <- " " # force load.
		updateDateRangeInput(session, "dates",start=min(dattab1()$Date) ,end=max(dattab1()$Date)) # load the initial dates.
		ansF
	})

	output$gotTime <- reactive({
		if (is.null(dattab1()$DateTime)) return(NULL)
		ansT <- " " # force load.
		ansT
	})


	obs1 <- observe({ # to continuously check and update the selectInput in ui.R.
		if (is.null(dattab1)) {
			return(NULL)
		}else{
			updateSelectInput(session, "params1", choices = colnames(dattab1()),selected = colnames(dattab1())[2]) # only after Date column.
			updateSelectInput(session, "params2", choices = colnames(dattab1()),selected = colnames(dattab1())[3]) # only after Date column.
			updateCheckboxGroupInput(session, "show_vars", choices = colnames(dattab1()))
			updateSelectInput(session, "groupdata", choices = colnames(dattab1()),selected = colnames(dattab1())[1])
		}
	})
	obs2 <- observe({
			if (is.null(dattab1()$DateTime)){
				return(NULL)
			}else{
				# Start Time.
				timeChoices1 <- dattab1()[dattab1()$Date==as.Date(input$dates[1]),]
				timeChoices1[,"DateTime"]<-as.character(timeChoices1$DateTime) # had to force as.character here in order for times to load the 00:00:00 times.
				updateSelectInput(session, "time1", choices = as.character(timeChoices1$DateTime), selected = as.character(timeChoices1$DateTime[1]))

				# End Time.
				timeChoices2 <- dattab1()[dattab1()$Date==as.Date(input$dates[2]),]
				timeChoices2[,"DateTime"]<-as.character(timeChoices2$DateTime)
				updateSelectInput(session, "time2", choices = as.character(timeChoices2$DateTime), selected = as.character(timeChoices2$DateTime[length(timeChoices2$DateTime)]))
			}
		#}
	})
	

	output$contents <- renderDataTable({ # actual table output rendered with JavaScript library DataTables.
		if (is.null(dattab1)) {
			return(NULL)
		}else{
			dattab1()[,input$show_vars,drop=FALSE] # allow selections.
			
		}
	})

	output$otherinfo <-renderTable({ # to output the additional data before headers.
		inFile <- input$file1
		if (is.null(inFile)) return(NULL)
		
		infoOut <- data.frame(summary(dattab1()))
		colnames(infoOut)<- c("eh","Parameter","Info")
		infoOut[-1]
	})

	output$plot0 <-renderPlot({ # draws the scatterplot graph with ggplot.
		if (is.null(dattab1)) {
			return(NULL)
		}else{
			if(is.null(dattab1()$DateTime)){ # Date.
				limdattab <- dattab1()[dattab1()$Date >= as.Date(input$dates[1]),]
				limdattab <- limdattab[limdattab$Date <= as.Date(input$dates[2]),]

			# Parameter 2 will be shifted:
				# shift by Months:
				limdattab2 <- dattab1()[dattab1()$Date >= addMonths(as.Date(input$dates[1]),input$numMonths),]
				limdattab2 <- limdattab2[limdattab2$Date <= addMonths(as.Date(input$dates[2]),input$numMonths),]				

				##JLZB: may add day shifts later:	
				#limdattab2 <- dattab1()[dattab1()$Date >= as.Date(input$dates[1])+input$numdays,]
				#limdattab2 <- limdattab2[limdattab2$Date <= as.Date(input$dates[2])+input$numdays,]

			}else{ # Date and Time.
				limdattab <- dattab1()[dattab1()$DateTime >= as.POSIXct(input$time1),]
				limdattab <- limdattab[limdattab$DateTime <= as.POSIXct(input$time2),]
				##JLZB:  will need to add shift...
				limdattab2 <- limdattab
			}

			if(input$radio1 == 2){
				regx <- log10(limdattab[[input$params1]])
				regxName <- paste("log10(",input$params1,")",sep="")
			} else if (input$radio1 == 3){
				regx <- sqrt(limdattab[[input$params1]])
				regxName <- paste("sqrt(",input$params1,")",sep="")
			} else {
				regx <- limdattab[[input$params1]]
				regxName <- input$params1
			}

			if(input$radio2 == 2){
				regy <- log10(limdattab2[[input$params2]])
				regyName <- paste("log10(",input$params2,")",sep="")
			} else if (input$radio2 == 3){
				regy <- sqrt(limdattab2[[input$params2]])
				regyName <- paste("sqrt(",input$params2,")",sep="")
			} else {
				regy <- limdattab2[[input$params2]]
				regyName <- input$params2
			}

			datflm <- data.frame(regx,regy) # create new df otherwise the mix transform plot will not have a proper regression line.
			p0 <- ggplot(datflm,aes_string(x="regx",y="regy"))+geom_point()

			# regression:
			finflm <- datflm[which(is.finite(datflm$regx*datflm$regy)),] # Remove "Inf" data before lm.
			lmout <- lm(regy~regx,data=finflm)
			pointC <- lmout[[1]][[1]]
			pointM <- lmout[[1]][[2]]
			adjr <- signif(summary(lmout)$adj.r.squared,digit=2)
			textx <- max(finflm$regx)
			texty <- max(finflm$regy)
			p0 <- p0+geom_abline(intercept=pointC,slope=pointM,colour="orange")+geom_text(x=textx,y=texty,hjust=3,vjust=0,size=6,label=adjr,colour="orange") 

			# rename columns:
			p0 <- p0+xlab(regxName)+ylab(regyName)
			
			p0	
		}
	})

	output$plot1 <-renderPlot({ # draws the Parameter 1 graph with ggplot.
		if (is.null(dattab1)) {
			return(NULL)
		}else{
			if(input$smoothit1){
				if(input$groupit){
					linetype1 <- geom_smooth()
				}else{
					linetype1 <- geom_smooth(colour="blue")
				}
			}else{
				if(input$groupit){
					linetype1 <- geom_line()
				}else{
					linetype1 <- geom_line(colour="blue")
				}
			}

			if(is.null(dattab1()$DateTime)) { # Date.
				if (input$groupit) { # If grouping is selected.
					p1 <- ggplot(dattab1(),aes_string(x="Date",y=input$params1, colour=input$groupdata))+linetype1
				}else{
					p1 <- ggplot(dattab1(),aes_string(x="Date",y=input$params1))+linetype1
				}
				p1 <- p1 +scale_x_date(limits=c(as.Date(input$dates[1]),as.Date(input$dates[2])))+xlab("")
			}else { # Date and Time.
				if (input$groupit) { # If grouping is selected.
					p1 <- ggplot(dattab1(),aes_string(x="DateTime",y=input$params1, colour=input$groupdata))+linetype1
				}else{
					p1 <- ggplot(dattab1(),aes_string(x="DateTime",y=input$params1))+linetype1
				}
				p1 <- p1 +scale_x_datetime(limits=c(as.POSIXct(input$time1),as.POSIXct(input$time2)))+xlab("")
			}

			if (input$scatterp){ # to mark out data points.
				p1 <- p1+geom_point(col="blue")
			}

			if(input$radio1 == 2){
				p1 <- p1+scale_y_log10()+ylab(paste("log10(",input$params1,")",sep=""))
			} else if (input$radio1 == 3){
				p1 <- p1+scale_y_sqrt()+ylab(paste("sqrt(",input$params1,")",sep=""))
			}else{

			}

			p1 # don't use "print" or brush won't work.
		}	
	})
	
	output$plot_brushedpoints1 <- renderTable({
		res <- brushedPoints(dattab1(), input$plot_brush1)
		if (nrow(res) == 0){
			return()
		}

		if (is.null(dattab1()[["DateTime"]])) {
		}else{
			res[,"DateTime"] <- as.character(res$DateTime)
		}

		if (dattab1()[["Date"]]) {
			res[,"Date"] <- as.character(res$Date)
		}
		res
	})

	output$plot2 <-renderPlot({  # draws the Parameter 2 graph with ggplot.
		if (is.null(dattab1)) {
			return(NULL)
		}else{
			if(input$smoothit2){
				if(input$groupit){
					linetype2 <- geom_smooth()
				}else{
					linetype2 <- geom_smooth(colour="red")
				}
			}else{
				if(input$groupit){
					linetype2 <- geom_line()
				}else{
					linetype2 <- geom_line(colour="red")
				}
			}

			if(is.null(dattab1()$DateTime)) { # Date.
				if (input$groupit) { # If grouping is selected.
                                        p2 <- ggplot(dattab1(),aes_string(x="Date",y=input$params2, colour=input$groupdata))+linetype2
				}else{
					p2 <- ggplot(dattab1(),aes_string(x="Date",y=input$params2))+linetype2
				}

				#p2 <- p2 +scale_x_date(limits=c(as.Date(input$dates[1]+input$numdays),as.Date(input$dates[2]+input$numdays)))+xlab("") ##JLZB: may add day shifts later.
				
				p2 <- p2 +scale_x_date(limits=c(as.Date(addMonths(as.Date(input$dates[1]),input$numMonths)),as.Date(addMonths(as.Date(input$dates[2]),input$numMonths))))+xlab("") # shifted by months.
				
			}else{ # Date and Time
				if (input$groupit) { # If grouping is selected.
                                        p2 <- ggplot(dattab1(),aes_string(x="DateTime",y=input$params2, colour=input$groupdata))+linetype2
				}else{
					p2 <- ggplot(dattab1(),aes_string(x="DateTime",y=input$params2))+linetype2
				}
				p2 <- p2 +scale_x_datetime(limits=c(as.POSIXct(input$time1),as.POSIXct(input$time2)))+xlab("")
			}

			if (input$scatterp){ # to mark out data points.
				p2 <- p2+geom_point(col="red")
			}

			if(input$radio2 == 2){
				p2 <- p2+scale_y_log10()+ylab(paste("log10(",input$params2,")",sep=""))
			}else if (input$radio2 == 3){
				p2 <- p2+scale_y_sqrt()+ylab(paste("sqrt(",input$params2,")",sep=""))
			}else{

			}

			p2
		}
	})

	output$plot_brushedpoints2 <- renderTable({
		res <- brushedPoints(dattab1(), input$plot_brush2)
		if (nrow(res) == 0){
			return()
		}

		if (is.null(dattab1()[["DateTime"]])) {
		}else{
			res[,"DateTime"] <- as.character(res$DateTime)
		}

		if (dattab1()[["Date"]]) {
			res[,"Date"] <- as.character(res$Date)
		}
		res
	})
	
	dateminy <- reactive({ # to get the min date to inform user.
		dattab <- dattab1()
		if(grepl("^0",tryCatch(length(dattab),error = function(e) e))=="TRUE"){
			print("load file")
		}else{
			as.character(min(as.Date(dattab$Date)))
		}
	})

	 datemaxy <- reactive({ # to get the max date to inform user.
		dattab <- dattab1()
		if(grepl("^0",tryCatch(length(dattab),error = function(e) e))=="TRUE"){
			print("load file")
		}else{
			as.character(max(as.Date(dattab$Date)))
		}
	})

	output$mainheader <-reactive({ # print out the main panel header.
		if(dateminy()=="load file"){
			print("Please load file")
		}else{
			paste("Data collected from",dateminy(),"to",datemaxy(),sep=" ")
		}
	})
	

})

#written by Joel Low Zi-Bin on 20160311
