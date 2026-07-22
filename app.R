#load libraries
library(shiny)
library(shinyjs)
#This library enables us to make a dashboardPage and use box() function
#could change the type of shiny app e.g to fluidPage etc, but box functions will not work (so need to remember to take those out)
library(shinydashboard)
library(ggplot2)
library(shinycssloaders)
#for Csplit
library(splitstackshape)
#for database connection
library(RPostgreSQL)
library(pool)
library(sp)
library(bslib)
user_base <- readRDS("user_base.rds")

UKCEH_theme <- bs_theme(
  bg = "#f5f9f7",
  fg = "#292C2F",
  primary = "#90A968",
  secondary = "#EAEFEC",
  #success = "#37a635",
 # info = "#477AE2" ,
  #warning = "#F49633",
  base_font = font_google("Inter")
)

UKCEH_theme <- bs_add_rules(
  UKCEH_theme,
  "
  table {
    font-size: 0.85rem;
  }"
 
)
#increase the font weight of the headings (h1, h2, h3, h4, h5, h6)
UKCEH_theme <- bs_add_variables(UKCEH_theme,
                                # low level theming
                                "headings-font-weight" = 600
                               
)

#titlePanel replacement
UKCEH_titlePanel <- function(title = "ID-TaxER Fungi-Explorer", windowTitle = title){
  div(
  style = "
    display:flex;
    align-items:center;
    justify-content:center;
    #gap:300px;transform: translateX(-250px);
   padding:30px;
  ",
  h2(
    title,
    style = "margin:0;"
  )
  )
}

#UI PAGE===================================================================================================================================================================================== 

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = UKCEH_theme,
#change login box colour
#define box class with blue outline  
  tags$head(
    tags$style(HTML("
      /* tabsetPanel tab selection */
      .nav-tabs .nav-link.active {
       background-color: #c6d3b4 !important;
        color: #000000 !important;
       }
       /*login box colour */
      .well {
        background-color: #c6d3b4;
        color: white;
      }
    "))
  ),

    fluidRow(
    #first column will have logo and buttons
     column(width=3,
      img(
        src = "UKCEH_Logo_Tagline_Stacked_Black.png",
        style = "height:131px;",
       ),      
#buttons with styling
      fluidRow(style='padding-left:45px;padding-top:-3px',actionButton("more_info_button", "More Information",icon=icon("info-circle",style = "color:#f5f9f7"), style="color: #000000; background-color:#c6d3b4; border-color: #c6d3b4;",width=210)),
      fluidRow(style='padding-left:45px;padding-top:30px',actionButton("bacteria_button", "  ID-TaxER-Bacteria  ",icon=icon("bacteria",style = "color:#f5f9f7;"),onclick ="window.open('https://shiny-apps.ceh.ac.uk/ID-TaxER/', '_blank')",  style="color: #000000; background-color:#c6d3b4; border-color: #c6d3b4;",width=210)),
      fluidRow(style='padding-left:45px;padding-top:30px',actionButton("github_button", "  Github  ",icon=icon("github",style = "color:#f5f9f7;"),onclick ="window.open('https://github.com/UKCEH-MolecularEcology/ID-TaxER-Fungi-Explorer', '_blank')",   style="color: #000000; background-color:#c6d3b4; border-color: #c6d3b4;",width=210),
  )), 
#second column with title panel and main content- e.g output tables plots etc
     column(width=8,
       div(
         style = "position:relative;text-align:center; margin-top:35px;margin-bottom:20px;",
         #App title
         HTML("<span style='font-size:24px;color:#7FA650;font-weight:600;position:relative;top:-2.5px;margin-right:5px;'>ID‑TaxER</span>
              <span style='font-size:36px;color:#000000;font-weight:600;'>  Fungi Explorer </span>
              <i class='fa fa-search' style='font-size:36px;color:#000000;'></i>
        "),
       div(
         #logout button
         style = "position:absolute;right:-50px;top:0px;",
         shinyauthr::logoutUI(id = "logout",style = "color: black; background-color: #dfefea; border-color: #dfefea;")
         )
       ),
  # login style
  tags$style(".login-ui label { color: #000000; }
            .login-ui .btn-default {
              background-color: #f5f9f7;
              border-color: #ebf5f4;
              color: #000000 !important;
        }
  "),
  div(class = "login-ui", shinyauthr::loginUI(id = "login",title="")),
  
  ###LOGINCODE
  conditionalPanel(
  condition = "output.user_auth == true",
  width = 8, 
          style="padding-left:80px; padding-right:0px;",
          
#MORE INFO
#  Hidden more information section using shinyjs
#  useShinyjs 'function must be called from a Shiny app's UI in order for all other shinyjs' https://www.rdocumentation.org/packages/shinyjs/versions/2.1.0/topics/useShinyjs
  useShinyjs(),
#have used some shiny html tags here
#tags$h is heading, tags$p is paragraph etc , tags$b is bold etc more on this here https://shiny.rstudio.com/articles/tag-glossary.html            
  shinyjs::hidden(div(id="more_info",
    tags$h4(tags$b("Summary")),
    tags$p( "This app provides an interface to explore potential soil habitat preferences of fungal taxa derived from high-throughput sequencing data of a partial fragment from the Internal transcribed spacer region (ITS2) of the rRNA gene operon. Sequence data was processed using DADA2 (Callahan et al. 2016) using the ITS pipeline workflow. Query sequences are blasted against ASV (amplicon sequence variant) sequences obtained from a large soil survey conducted across Britain (the Countryside Survey). Each sequence in the database is linked to taxonomic assignments as well as environmentally derived information about that ASV. Results are displayed as an interactive table of hits with percentage match to a CS sequence, and associated taxonomy (annotated using the UNITE version 9 All Eukaryotes database (Nilsson et al. 2018)). Upon selecting a hit, habitat preferences and spatial distribution are displayed (currently Britain only)."),
    tags$h4(tags$b("Limitations")),
    tags$p("The database encompasses the ITS2 of the ITS region, amplified with fITS7f and ITS4R (Ihrmark et al. 2012) primers. Queries which do not cover this region will obviously give incorrect results, and additionally taxa poorly amplified with these primers will be under represented. Importantly this tool is based on homology mapping to a portion of the conserved ITS rRNA gene, and so all the usual limitations apply regarding accuracy of taxonomic (and habitat preference) assignment. It is therefore for research purposes only."),
    br(),
    tags$p("Callahan BJ, McMurdie PJ, Rosen MJ, Han AW, Johnson AJ, Holmes SP. DADA2: High-resolution sample inference from Illumina amplicon data. Nat Methods. 2016 Jul;13(7):581-3. doi: 10.1038/nmeth.3869. Epub 2016 May 23. PMID: 27214047; PMCID: PMC4927377."),
    tags$p("Ihrmark, K., Bödeker, I.T., Cruz-Martinez, K., Friberg, H., Kubartova, A., Schenck, J., Strid, Y., Stenlid, J., Brandström-Durling, M., Clemmensen, K.E. and Lindahl, B.D., 2012. New primers to amplify the fungal ITS2 region–evaluation by 454-sequencing of artificial and natural communities. FEMS microbiology ecology, 82(3), pp.666-677."),
   tags$p("Nilsson RH, Larsson K-H, Taylor AFS, Bengtsson-Palme J, Jeppesen TS, Schigel D, Kennedy P, Picard K, Glöckner FO, Tedersoo L, Saar I, Kõljalg U, Abarenkov K. 2018. The UNITE database for molecular identification of fungi: handling dark taxa and parallel taxonomic classifications. Nucleic Acids Research, DOI: 10.1093/nar/gky1022")
  )),
  br(),
##INPUTS 
#define tab for sequence search
  tabsetPanel(id="Sequence_search",
    tabPanel(title="Sequence Search",
     br(),
#Sequence Input    
     textInput(inputId = "mysequence",label="Please enter a sequence",value="",width = 10000, placeholder = ''),
#add empty line
     br(),
#buttons
#blast sequence
     actionButton("blast", "Blast",style="color: #000000; background-color:#90a968 ;border-color:#90a968 "),
#clear sequence input
     actionButton("clearInput", "Clear Input",style="color: #000000; background-color:#c6d4b4 ;border-color:#c6d4b4 " ),
#enter example sequence
     actionButton("exampleSequence", "Example Sequence",style="color: #000000; ; background-color:#c6d4b4  ;border-color:#c6d4b4 "),
     br(),
#area where any warning messages appear                
     span(textOutput("Warning"),style="color:#477AE2;font-size:17px"),
     br()
    ),
    tabPanel(title="Taxonomy Search",
     br(),
#Taxonomy input Input    
     textInput(inputId = "mytaxonomy",label="Please enter a taxonomic name",value="",width = 10000, placeholder = ''),
     br(),
     actionButton("taxonomysearch", "Search",style="color: #000000; background-color:#90a968 ;border-color:#90a968 "),
     actionButton("clearTaxonomyInput", "Clear Input",style="color: #000000; background-color:#c6d4b4 ;border-color:#c6d4b4 " ),
#enter example sequence
     actionButton("exampleTaxonomy", "Example Taxonomy",style="color: #000000; background-color:#c6d4b4 ;border-color:#c6d4b4 ")
    )
  ),
  br(),
### OUTPUTS
# 'This function must be called from a Shiny app's UI in order for all other shinyjs' https://www.rdocumentation.org/packages/shinyjs/versions/2.1.0/topics/useShinyjs
# useShinyjs(),
#hidden results section
  shinyjs::hidden(div(id="Results",
#ok lets add some structure for the outputs (we dont just want plots and tables stacked), here im using fluid rows and columns (although many other options for laying out app)
#create fluid row
#first column for main output table
  fluidRow(
    column(width=8,br(),
    br(),
    HTML('<center><h4>Top Hits</h4></center>'),
#box basically creates a white box around the output
#only works in shiny dashboards not fluid page etc
#some dataTable aesthetic options (e.g number of rows to display/pagination of large tables, colnames displayed etc) are controlled from renderDataTable in server.R                 
    box(DT::dataTableOutput('Main_output_table'),width=500,height=425)
  #end of collumn
    ),
#second col for outputs-plots
    column(width=4,
      hr(style = "border: none;"),
#Tabset for plots for neatness
#'Tabsets are useful for dividing output into multiple independently viewable sections.' https://www.rdocumentation.org/packages/shiny/versions/1.7.3/topics/tabsetPanel
      tabsetPanel(id="plotTabset",
#Tab plot 1  
#Include loading spinner for map
      tabPanel(title="GB Maps",withSpinner(plotOutput('OTU_map')),width=350,height=425),
#Tab plot 2
      tabPanel(title="Habitats",plotOutput('OTU_avc_boxplot'),width=350,height=425),
#Tab plot 3
      tabPanel(title="pH LOESS",plotOutput('OTU_pH_LOESS'),width=350,height=425)
#end of tabset
      )
#end of column
    ),
#end of fluid row
  ), 
 hr(style = "border: none;"),
 hr(style = "border: none;"),
  fluidRow(column(width=12,HTML('<h4><center>Blast Output </center></h4>'),
  box(DT::dataTableOutput("OTU_blast_match"),width=1000
#end of box
  )
#end of column
    )
    #end of fluid row
  )
    #end of div function
  )
    #end of shinyjs::hidden(e.g results)
  ),#link to privacy privacy
  hr(style = "border-color: #f5f9f7;"),
  div(style = "text-align: center;",
    img(src="logos_combined2.png",style="height: 80px")),
    br(),
  #hr(style = "border-color: #f5f9f7;"),
    div(style = "text-align: center;", a("UKCEH Privacy Policy", href = "https://www.ceh.ac.uk/privacy-notice",class="custom_link")),
    #end of conditional panel
    )
    #end of second column
    )
))
   

server <- function(input, output,session) {
  #LOGINCODE (shiny authorisation not psql)
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  output$user_auth <- reactive({
    credentials()$user_auth
  })
  outputOptions(output, "user_auth", suspendWhenHidden = FALSE)
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
#postgres database connection
  con <- dbPool(
    drv = RPostgreSQL::PostgreSQL(max.con=140),
    dbname = Sys.getenv('SQL_DB'),
    host = Sys.getenv('SQL_HOST'),
    port=Sys.getenv('SQL_PORT'),
    user=Sys.getenv('SQL_USER'),
    password = Sys.getenv('SQL_PWD')
  )
  #store last action
  last_action<- reactiveVal("none") 
#ShinyJS commanda to make outputa visible
  shinyjs::onclick("taxonomysearch",shinyjs::show(id="Results",anim=TRUE))  
  shinyjs::onclick("blast",shinyjs::show(id="Results",anim=TRUE))  
#ShinyJS command to get more info        	
  onclick("more_info_button",toggle(id="more_info",anim=TRUE))
#get all sample environment information going to use this for habitat box plots
  SQL_command=paste("select * from env_attributes.env_attributes_all;")
#env consists of sample avc_code (habitat code), avc (habitat description) and pH
  env <- dbGetQuery(con, SQL_command)    
#get map outline to use later
  #for purpose of loading serialised objects 
  dbGetQuery(con, "set standard_conforming_strings to 'on'")
  #get uk map outline to plot uk mapping objects  
  SQL_command=paste("select plot_object from plotting_tools.map_tools where description= 'map_outline';")
  uk.line <-unserialize(postgresqlUnescapeBytea( dbGetQuery(con, SQL_command))) 
    
# Blast function    
  make.comparison <- function(query){
    #check query isnt empty
    if (query!=""){ 
        #blast_command for aligning sequences returns top 20 hits
        #using echo as a way to pass sequence not in a file to blastn #https://www.biostars.org/p/17265/
### BLAST_DB ####        
      cmd<- paste('echo -e ">Query seq\n',query,'"', '|blastn -db Blast_DB/Blast_DB -num_alignments 20 -evalue 0.001 -outfmt  7')
        #run system command and capture output
        #shQuote Quotes a string to be passed to an operating system shell
      blast_capture<- system(paste("/bin/bash -c", shQuote(cmd)),intern=TRUE)
        #check there are hits
        #first few lines are not results if there are 0 hits this will be displayed on fourth line
      if(blast_capture[4]!="# 0 hits found"){
          #this variable will be used later to identify that output should be displayed (as hits have been returned from blast command)      
        output_switch <-"on"
          #remove all descriptive lines of output that are not results (first five lines and last line of output)
        blast_capture<-blast_capture[6:(length(blast_capture)-1)]
          #split output by tabs
        blast_capture_df=as.data.frame(cSplit(as.data.frame(blast_capture),"blast_capture",sep="\t"))
#get rid of first column with query ID  (ln 147)        
        blast_capture_df=blast_capture_df[,-1]
          #now lets start getting some wider information about these taxa stored in the SQL database
###POSTGRES ###         
          #using a join(similar to r merge) to get information in taxonomy and abundance_stats tables using WHERE statement to get relevant ASV/OTUs from blast output
          #tx and abs are SQL aliases for taxonomic and abundance_stats tables to reduce length of command
        SQL_command=paste("SELECT tx.*, abs.abundance_rank, abs.occupancy_proportion FROM fungal_otu_attributes.fungal_taxonomy tx JOIN fungal_otu_attributes.fungal_abundance_stats abs ON tx.hit = abs.hit WHERE tx.hit IN ('",paste(blast_capture_df$blast_capture_02,collapse="', '"),"');",sep="")
        relevant_tax_and_stats=dbGetQuery(con, SQL_command)
        blast_relevant_tax_and_stats=merge(blast_capture_df,relevant_tax_and_stats,by=1)
        #remove columns we dont want for this table
        blast_relevant_tax_and_stats=blast_relevant_tax_and_stats[,c(2,1,12:20)]
        #change first two colnames to more meaningful names
        colnames(blast_relevant_tax_and_stats)[1:2]=c("identity","hit")
        #now order like original blast output
    ##CHANGED
        #row.names(blast_relevant_tax_and_stats)=blast_relevant_tax_and_stats[,2]
        #this code allows multiple hits between a query and a reference sequence to appear in a dataframe
        row.names(blast_relevant_tax_and_stats)=make.unique(as.character(blast_relevant_tax_and_stats[,2]),sep = "_")
        
        
        blast_relevant_tax_and_stats=blast_relevant_tax_and_stats[blast_capture_df$blast_capture_02,]
        #add colnames to full blast output as we want to return this too
        colnames(blast_capture_df)= c("subject id","% identity","alignment length","mismatches","gap opens","q.start","q.end","s.start","s.end","evalue","bit score")
         #get abundance data in one go here
  #####   
          #get abundance data for all otus here , could do this at point when rows are selected (e.g in output$AVC_box_plot code) but as a slow command this would slow things down as navigating the app which would be frustrating
          #could consider getting map data within this function too
          #need to execute join command as data in two parts
        SQL_command=paste0("SELECT * FROM abund_tables.fungal_abund_1 JOIN abund_tables.fungal_abund_2 ON abund_tables.fungal_abund_1.hit=abund_tables.fungal_abund_2.hit WHERE abund_tables.fungal_abund_1.hit IN('",paste(blast_capture_df$`subject id`,collapse="', '"),"');")
        OTU_abund=dbGetQuery(con,SQL_command)
        #two hit columns need to remove one
        OTU_abund=OTU_abund[,unique(colnames(OTU_abund))]
        #make remaining hit collumn rownames
        row.names(OTU_abund)=OTU_abund[,1]
        #remove first column
        OTU_abund=OTU_abund[,-1]
        #order in the same order as blast output for consistency and in order to access correct otu data in output$AVC_box_plot code
        OTU_abund= OTU_abund[blast_capture_df$`subject id`,] 
        #transpose 
        OTU_abund=data.frame(t(OTU_abund))
        #return blast_relevant_tax_and_stats, full blast output and output_switch , signalling output should be displayed
        return(list(tax_and_stats=blast_relevant_tax_and_stats,full_blast_output=blast_capture_df,OTU_abundance=OTU_abund,output_switch="on"))
      }
      else{
          #if query has no hits, switch variable is given value off to identify that output should not be displayed         
        return(list("output_switch"="off"))       
      }
    }
    else{
        #if query empty,switch variable is given value off to identify that output should not be displayed     
      return(list("output_switch"="off"))
    }
  }   
   # Tax search function    
  tax.comparison <- function(query){
      #check query isnt empty
    if (query!=""){ 
      #within our database spaces are denoted with an underscore
      #therefore if query includes spaces we will replace them with underscore prior to query
      query=gsub(" ","_",query)
      #search across all taconomic fields apart from kingdom to find all partial string matches
        SQL_command=paste0("SELECT * FROM fungal_otu_attributes.fungal_taxonomy AS tx
          WHERE EXISTS (
            SELECT 1
            FROM unnest(ARRAY[tx.phylum, tx.class, tx.order,tx.family,tx.genus,tx.species]) AS x(col)
            WHERE col ILIKE '%",query,"%'
            );"
        )
        relevant_tax=dbGetQuery(con, SQL_command)
          if(nrow(relevant_tax)>1){
          #using a join(similar to r merge) to get information in taxonomy and abundance_stats tables using WHERE statement to get relevant ASV/OTUs from blast output
          #tx and abs are SQL aliases for taxonomic and abundance_stats tables to reduce length of command
            SQL_command=paste("SELECT tx.*, abs.abundance_rank, abs.occupancy_proportion FROM fungal_otu_attributes.fungal_taxonomy tx JOIN fungal_otu_attributes.fungal_abundance_stats abs ON tx.hit = abs.hit WHERE tx.hit IN ('",paste(relevant_tax$hit,collapse="', '"),"');",sep="")
            relevant_tax_and_stats=dbGetQuery(con, SQL_command)
            relevant_tax_and_stats$abundance_rank_numeric= as.numeric(gsub("/.*","",relevant_tax_and_stats$abundance_rank))
            relevant_tax_and_stats<-relevant_tax_and_stats[order(relevant_tax_and_stats$abundance_rank_numeric),]
            relevant_tax_and_stats$abundance_rank_numeric=NULL
          #get abundance data for all otus here , could do this at point when rows are selected (e.g in output$AVC_box_plot code) but as a slow command this would slow things down as navigating the app which would be frustrating
          #could consider getting map data within this function too
          #need to excute join command as data in two parts
            SQL_command=paste0("SELECT * FROM abund_tables.fungal_abund_1 JOIN abund_tables.fungal_abund_2 ON abund_tables.fungal_abund_1.hit=abund_tables.fungal_abund_2.hit WHERE abund_tables.fungal_abund_1.hit IN('",paste(relevant_tax_and_stats$hit,collapse="', '"),"');")
            OTU_abund=dbGetQuery(con,SQL_command)
            row.names(OTU_abund)=OTU_abund[,"hit"]
            OTU_abund=OTU_abund[relevant_tax_and_stats$hit,]
          #two hit columns need to remove one
            OTU_abund=OTU_abund[,unique(colnames(OTU_abund))]
          #make remaining hit collumn rownames
          #remove first column
            OTU_abund=OTU_abund[,-1]
          #order in the same order as blast output for consistency and in order to access correct otu data in output$AVC_box_plot code
         # OTU_abund= OTU_abund[relevant_tax$hit,] 
          #transpose 
            OTU_abund=data.frame(t(OTU_abund))
          #return blast_relevant_tax_and_stats, full blast output and output_switch , signalling output should be displayed
            return(list(tax_and_stats=relevant_tax_and_stats,OTU_abundance=OTU_abund,output_switch="on"))
          }else{list("output_switch"="off")}
    }else{
          #if query has no hits, switch variable is given value off to identify that output should not be displayed         
          return(list("output_switch"="off"))       
    }
  }
 
#make blast run when you click blast button using eventReactive Function
#"The function eventReactive() is used to compute a reactive value that only updates in response to a specific event."-https://campus.datacamp.com/courses/building-web-applications-with-shiny-in-r/reactive-programming-3?ex=11
#apply same principle to taxonomic search button 
  observeEvent(input$taxonomysearch, {
    last_action("tax")
  })
  observeEvent(input$blast, {
    last_action("seq")
  })
  run_sequence<-eventReactive(input$blast,{
    make.comparison(input$mysequence)}
  )
  run_taxonomy<-eventReactive(input$taxonomysearch,{
    tax.comparison(input$mytaxonomy)}
  )
  
#renderDataTable selection mode='single' ensures only one row can be selected at a time
#selected=1 means first row will be selected as default
#in options scrollX = TRUE means horixontal scrolling is enabled 
#pageLength=7 refers to how many rows to show per page    
# dom refers to which elements of datatable to include , here dom='tp', refers to table and pagination control (other options include a search bar etc)
#see https://datatables.net/reference/option/dom
#row.names not included and colnames set here too (dont need to do this if happy with original dataframes rownames )
  output$Main_output_table=DT::renderDataTable({
    #output dependent on whether blast or taxoomic search bittons pressed
    if(last_action() =="seq"){
      run_sequence_output=run_sequence() 
      #run_sequence_output$tax_and_stats
      return( run_sequence_output$tax_and_stats)
    }
    if(last_action() =="tax"){
      run_taxonomy_output=run_taxonomy()
       # run_taxonomy_output$tax_and_stats
      return(run_taxonomy_output$tax_and_stats)
    }  
    return(NULL)
    }
    ,selection = list(mode='single',selected=1),options=list(scrollX=TRUE,pageLength=7,dom='tp'),rownames=FALSE)

###MAP###
  output$OTU_map=renderPlot({
    if(last_action() =="seq"){
      query_output<-run_sequence()
    }
    if(last_action()=="tax"){
      query_output<-run_taxonomy()
    }
      #if output_switch on      
    if ( query_output$output_switch=="on"){
        #get selected row       
        s=input$Main_output_table_rows_selected
        #if row has been selected        
        if(length(s)){
          #get OTU from  run_sequence output          
          OTU=query_output$tax_and_stats[s,"hit"]
          par(mar = c(4, 4, 1, 4))
          dbGetQuery(con, "set standard_conforming_strings to 'on'")
          ###DB SPECIFIC          
          #get relevant map       
          unescape_bytea_map=dbGetQuery(con, paste("SELECT map_object FROM fungal_otu_attributes.fungal_maps WHERE hit='",toString(OTU),"';",sep=""))
          #convert from bytea to r object          
          bytea_map<-postgresqlUnescapeBytea(unescape_bytea_map) 
          r_object_map<-unserialize( bytea_map)
          #plot       		
          spplot(r_object_map[[1]]["var1.pred"],at=unlist(r_object_map[2:11]),xlab=toString(OTU),ylab.pos=c(5,10,100),sp.layout=list("sp.lines",uk.line,lwd=2,col="black"))
        }
      }   
    })
 ###AVCPlot
  output$OTU_avc_boxplot = renderPlot({  
    if(last_action() =="seq"){
        query_output<-run_sequence()
    }
    if(last_action()=="tax"){
        query_output<-run_taxonomy()
    }
    if ( query_output$output_switch=="on"){
      #get selected row       
      s=input$Main_output_table_rows_selected
      #if row has been selected        
      if(length(s)){
      #get OTU from  run_sequence output     
        abund=query_output$OTU_abundance[,s,drop=FALSE]
        #get ASV name from the colname
        colnames(abund)[1]=strsplit(colnames(abund)[1],"\\.")[[1]][1]
        #if otu has occupancy of  greater than 30
        # if(length(which(abund!=0))>20){
        #merge with env
        abund_env=merge(abund,env,by.x=0,by.y=1)
        #order avc factor levels so x axis represents habitat gradient
        ord<-reorder(abund_env$avc,X=as.numeric(abund_env$avc_code),FUN=mean)
        par(mar=c(11,5.8,2,0.4)+0.1)
        #outline =TRUE show outliers
        boxplot(abund_env[,2]~ord,las=2,ylab="",xlab="",ylim=c(0,1.1*max(abund_env[,2])),cex=0.5,outline=TRUE)
        title(ylab=paste("Relative Abundance (",colnames(abund_env)[2],")",sep=""), line=4.5, cex.lab=0.9)
        }
      }else{plot.new()}
    })   
###pHLOESS###
    
    output$OTU_pH_LOESS=renderPlot({
      ###LOGINCODE
      #req(credentials()$user_auth)
      ###LOGINCODEEND
      
      if(last_action() =="seq"){
        query_output<-run_sequence()
      }
      if(last_action()=="tax"){
        query_output<-run_taxonomy()
      }

    if ( query_output$output_switch=="on"){
      #get selected row       
      s=input$Main_output_table_rows_selected
      #if row has been selected        
      if(length(s)){
        #get OTU from  run_sequence output     
        abund=query_output$OTU_abundance[,s,drop=FALSE]
        colnames(abund)[1]=strsplit(colnames(abund)[1],"\\.")[[1]][1]
        #merge with env
        abund_env=merge(abund,env,by.x=0,by.y=1)
        #order df by ph
        abund_env<-abund_env[order(abund_env[,5]),]
        ggplot(data=abund_env,aes(ph,.data[[colnames(abund_env)[2]]])) +geom_point()+
          theme_bw()+
          theme(axis.line = element_line(colour = "black"),
                text=element_text(size=15),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                plot.margin=margin(t=20,r=20,b=20,l=20,"pt"))+
          geom_smooth(method = "loess")+
          ylab("pH")+xlab(colnames(abund_env)[2])
      }
    }
    })  
    
    #selection='none' means no rows can be selected     
    # options dom='t' is telling renderDataTable just to show table and not include fancy extras
    #No rownames
    output$OTU_blast_match=DT::renderDataTable({
      ###LOGINCODE
      #req(credentials()$user_auth)
      ###LOGINCODEEND
      if(last_action() =="seq"){
        run_sequence_output<-run_sequence()
      #if output_switch on      
       if (run_sequence_output$output_switch=="on"){
        #get selected row       
          s=input$Main_output_table_rows_selected
        #if row has been selected        
          if (length(s)){
          #get relevant         
           return(run_sequence_output$full_blast_output[s,])
          }
       }
      }
      if(last_action() =="tax"){
        validate(
          need(FALSE, 
              "Taxonomy search does not produce blast output"
        ))
      
      }
      return(NULL)
    },selection='none',options=list(dom='t'),rownames=FALSE,colnames= c("subject id","% identity","alignment length","mismatches","gap opens","q.start","q.end","s.start","s.end","evalue","bit score"))
  observeEvent(input$clearInput, {
      reset("mysequence")
  })    
  observeEvent(input$clearTaxonomyInput, {
      reset("mytaxonomy")
  }) 
###SEQUENCE###    
#if example sequence is button is pressed, example sequence entered into query box
  observeEvent(input$exampleSequence, {
    updateTextInput(session,"mysequence",value="CTACCTGATCCGAGGTCAACCTTGGTGCCGCCGGAGCGGGCTTGAGGGGGGTTTAGAGGC
CGGATAGCCCGCAGGCTCCCGATGCGAGGCAGATGTTACTACGCAAAGGAAGGGCCCAAC
GGGTCCGCCACTGGTTTTCGGGGACTGCCTGGGCAGATCCCCAACGCCGGGCCACGGGGG
CTCGAGGGTTGAAACGACGCTCGGACAGGCATGCCTCCCAGGATAC")
  })   
  observeEvent(input$exampleTaxonomy, {
    updateTextInput(session,"mytaxonomy",value="Ascomycota")
  }) 
#display warning if switch set to off  
  output$Warning <- renderText({
    # Always react to last_action()
      if (last_action() == "seq") {
        query_output <- run_sequence()
        if (query_output$output_switch == "off") {
          return("No Hits Found!")
        } else {
          return("")   # no message
        }
      }
      if (last_action() == "tax") {
        query_output <- run_taxonomy()
          if (query_output$output_switch == "off") {
            return("No Hits Found!")
          } else {
            return("")
          }
      }
        
      #return("")  # default empty
  })
      
}

# Run the application 
shinyApp(ui = ui, server = server)
