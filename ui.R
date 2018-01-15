library(shiny)

shinyUI(fluidPage(
	titlePanel("TimeDetective"),
	sidebarLayout(
		sidebarPanel(
			fileInput('file1','Upload .tab or .csv file only. Ensure there is a "Date" column, and at least 2 other column of data.', accept=c('text','.tab','.csv')),
			tags$hr(),
			textOutput('gotFile'),
			conditionalPanel( # only pops out if the Graph panel is picked and there is data loaded.
				'input.panel==="Graph" && output.gotFile', 
				dateRangeInput("dates", label = h3("Date range for graphs"),start = Sys.Date()-1460, end=Sys.Date())

				,conditionalPanel( # drop down list of times.
					'output.gotTime',
					fluidRow(
					column(6,
					selectInput('time1','Start Time',choices=c("stuff","hah"))
					),
					column(6,
					selectInput('time2','End Time',choices=c("stuff","hah"))
					)
					)
				)
				,checkboxInput('scatterp',tags$b('Mark data points'))
				,tags$hr()
				,checkboxInput('groupit',tags$b('Group data by:'))
				,conditionalPanel(
					'input.groupit'
					,selectInput('groupdata',' ',choices=c("stuff","hah"))
				)
				,tags$hr()
				,selectInput('params1',h3('Parameter 1'),choices=c("stuff","hah"))
				,checkboxInput('smoothit1',tags$b('Apply smoothing'))
				,radioButtons("radio1", label = tags$i("Transformations"),choices = list("none" = 1, "log10" = 2, "square root" = 3),selected = 1)
				,tags$hr()
				,selectInput('params2',h3('Parameter 2'),choices=c("stuff","hah"))
				,checkboxInput('smoothit2',tags$b('Apply smoothing'))
				,radioButtons("radio2", label = tags$i("Transformations"),choices = list("none" = 1, "log10" = 2, "square root" = 3),selected = 1)
				,tags$hr()
                                ,checkboxInput('shiftit',tags$b('Shift Parameter 2 data by:'))
                                ,conditionalPanel(
                                        'input.shiftit'
                                        #,numericInput("numdays", label = h4("day"), value = 0) ##JLZB: may add in later.
                                        ,numericInput("numMonths", label = h4('Month (only works for daily and monthly datasets)'), value = 0)
                                )
                                ,tags$hr()
				,plotOutput('plot0')
			),

			conditionalPanel(
				'input.panel==="Table" && output.contents',
				checkboxGroupInput('show_vars', 'Columns to show:',choices=c("foo","bar"))
			)
			,tags$hr()
		),

		mainPanel(
			h3(textOutput('mainheader')),
			conditionalPanel(
			'output.gotFile',	
			tabsetPanel(
				id = 'panel',
				tabPanel('Graph'
				,plotOutput('plot1', brush=brushOpts(id = 'plot_brush1'))
				,plotOutput('plot2', brush=brushOpts(id = 'plot_brush2')),
				conditionalPanel( # because the selection only works properly with no transformation.
					'input.radio1=="1"',
					h4("Graph 1 Selected points"),
					tableOutput('plot_brushedpoints1')
				),
				conditionalPanel( # because the selection only works properly with no transformation.
					'input.radio2=="1"',
					h4("Graph 2 Selected points"),
					tableOutput('plot_brushedpoints2')
				),
				textOutput('gotTime')
				),
				tabPanel('Table',dataTableOutput('contents')),
				tabPanel('Summary',tableOutput('otherinfo'))
			)
			)
		)
	)
))


#written by Joel Low Zi-Bin on 20160311
