library(shiny); library(dplyr); library(plyr); library(reshape2); library(ggplot2); library(plotly); library(lubridate); library(devtools)

ui = fluidPage(
	tabsetPanel(
		tabPanel("Data Upload",fluid=TRUE,
			sidebarLayout(
				sidebarPanel(
				fileInput('file1', 'CSV File',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
  			radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
				textInput("missing","Missing Value Designator",value="NA"),
				checkboxInput('header', 'Header', TRUE),
  			checkboxInput('show', 'Show All Rows', TRUE)),
			mainPanel(tableOutput("contents")))),
		
		tabPanel("Statistics",fluid=TRUE,
			sidebarLayout(
				sidebarPanel(
					actionButton("stat","Run Analysis"),
					downloadButton("down3","Download Table")),
			mainPanel(tableOutput("results")))),
		
		tabPanel("Density Plots",fluid=TRUE,
			sidebarLayout(
				sidebarPanel(
					uiOutput("checkbox2"),
					downloadButton("down1","Download Plot")),
				mainPanel(plotlyOutput("density")))),
		
		tabPanel("Time Series Plots",fluid=TRUE,
			sidebarLayout(
				sidebarPanel(
					numericInput("date","Date Column",value=1),
					uiOutput("checkbox3"),
					downloadButton("down2","Download Plot"),
					),
				mainPanel(plotlyOutput("timegraph"))  ### remove time zone and include caption that it's Central Time (UST)
			)),
		
		tabPanel("pH Prediction",fluid=TRUE,
						 sidebarLayout(
						 	sidebarPanel(
						 		numericInput("PH1","pH (1 day prior)",value=NULL),
						 		numericInput("PH2","pH (2 days prior)",value=NULL),
						 	),
						 	mainPanel(textOutput("prediction"))
						 ))
		
		))

server = function(input,output,session) {
  FILE = reactive({ 
    req(input$file1) 
    inFile = input$file1 
    if(input$show == FALSE){
    	 df = head(read.csv(inFile$datapath, header = input$header, na.strings = input$missing,sep = input$sep))
    	 return(df)
    }
    if(input$show == TRUE){
    	df = read.csv(inFile$datapath, header = input$header, na.strings = input$missing, sep = input$sep)
    	return(df)
    }
  })
  output$contents= renderTable({
  	FILE()
  	})
  
  output$checkbox1 = renderUI({
  	checkboxGroupInput("choose","Choose Columns",choices=names(FILE()))})
  
  STATS = reactive({
  	req(input$file1)
  	inFile = input$file1
  	if(input$stat == TRUE){
  		df = read.csv(inFile$datapath, header = input$header, sep = input$sep,na.strings = input$missing)
  		df2 = melt(df,measure.vars = colnames(df))
  		df2$value = as.numeric(df2$value)  
  		df2$variable = as.character(df2$variable)
  		df3 = na.omit(df2)
  		df4 = ddply(df3,~variable,summarize,Minimum = min(value),Mean = mean(value),Median = quantile(value,0.5),Maximum = max(value))
  		return(df4)
  	}
  	if(input$stat == FALSE){
  	}
  })
  output$results = renderTable({
  	STATS()
  })
	
    output$down3 = downloadHandler(
  	filename = function(){paste(input$file1,".csv",sep="")},
  	content = function(file){write.csv(STATS(),file,row.names = FALSE)}
  	)  
  
  output$checkbox2 = renderUI({
  	selectInput("select","Select Variable",choices=names(FILE()))    # variable selection options for density plotting
  }) 
  
  GRAPH = reactive({
  	req(input$file1)
  	inFile = input$file1
  	select = input$select
  	file = read.csv(inFile$datapath, header = input$header, sep = input$sep,na.strings = input$missing)
  	file2 = melt(file,measure.vars = colnames(file),na.rm = TRUE)
  	file2$variable = as.character(file2$variable)
  	file2$value = as.numeric(file2$value)
  	file3 = file2 %>% filter(variable==as.character(select))
  	file4 = file3[,2] %>% as.data.frame()
  	graph1 = ggplot(file4,aes(x=.),fill="yellow",alpha=0.5)+geom_density()+theme_bw()+
  		labs(x="Selected Variable",y="Density",
  			caption="'Density' describes how often the variable equals a certain number, with peaks representing the most frequent values.")
  	graph2 = ggplotly(graph1)
  	print(graph2)
  	})
  
  output$density = renderPlotly({    # make the density plot appear
  	GRAPH()
  	})
  
  output$down1 = downloadHandler(
  	filename = function(){paste(input$name1,".png",sep='')},
  	content = function(){ggsave(file,plot=GRAPH(),device = png)})  # graph downloading mechanism for density plots
  
  
  output$checkbox3 = renderUI({
  	selectInput("select2","Select Variable",choices=names(FILE()))    # variable selection options for density plotting
  }) 
  
  GRAPH2 = reactive({
  	req(input$file1)
  	inFile = input$file1
  	select2 = input$select2
  	date = as.numeric(input$date)
  	file = read.csv(inFile$datapath, header = input$header, sep = input$sep,na.strings = input$missing)
  	Date = as.character(file[,date])  # date column
  	file2 = melt(file,id.vars = "Date",measure.vars = colnames(file))
  	file2$variable = as.character(file2$variable)
  	file2$value = as.numeric(file2$value)
  	file3 = file2 %>% filter(variable==as.character(select2))
  	file4 = file3[,3] %>% as.data.frame()
  	plot = plot_ly(file4,x=Date,y=file3$value,type="scatter")
  	print(plot)
  	})

  	output$timegraph = renderPlotly({
  		GRAPH2()
  	})
  	session$userData$time = reactive({format(lubridate::mdy_hms(as.character(input$localTime)), "%d/%m/%Y; %H:%M:%S")})
  	
  	output$down2 = downloadHandler(
  	filename = function(){paste(input$name2,".png",sep='')},
  	content = function(){ggsave(file,plot=GRAPH(),device = png)})  # graph downloading mechanism for density plots
  
  	output$ph = renderUI({
  	selectInput("phchoice","Choose pH Column",choices=names(FILE()))    # variable selection options for density plotting
  }) 
  	
  	output$feed = renderUI({
  	selectInput("fchoice","Choose Fish Feed Column",choices=names(FILE()))    # variable selection options for density plotting
  }) 
  	
PREDICT = reactive({
	PH1 = input$PH1
	PH2 = input$PH2
	equation = 0.6626*PH1+0.23797*PH2+0.67193
	equation2 = round(equation,2)
	print(equation2)
})
  	
  	output$prediction = renderPrint({
  		PREDICT()
  	})
  	}


shinyApp(ui, server)

