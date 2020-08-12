
require(shiny)
require(digest) 
require(grid)
require(MASS) 
require(stats) 
require(shinyjs) 

library(ggplot2)
library(plotly)
library(ggthemes)

if( !'www' %in% list.files() ){
    print("Downloading web resource files")
    dir.create("www")
    download.file(url="https://storage.googleapis.com/bakar-data/10k/www/Banner.jpeg",
                  destfile="www/Banner.jpeg", method="wget")
    download.file(url="https://storage.googleapis.com/bakar-data/10k/www/Figure_1.png",
                  destfile="www/Figure_1.png", method="wget")
    download.file(url="https://storage.googleapis.com/bakar-data/10k/www/Table_1.jpg",
                  destfile="www/Table_1.jpg", method="wget")
    print("Done")
}

if( !'analytics' %in% list.files() ){
    print("Downloading analytics files")
    dir.create("analytics")
    download.file(url="https://storage.googleapis.com/bakar-data/10k/analytics/analytics.org.js",
                  destfile="analytics/analytics.org.js", method="wget")
    download.file(url="https://storage.googleapis.com/bakar-data/10k/analytics/analytics.ucsf.js",
                  destfile="analytics/analytics.ucsf.js", method="wget")
    print("Done")
}

if( !'data' %in% list.files() ){
    print("Downloading data files")
    dir.create("data")
    download.file(url="https://storage.googleapis.com/bakar-data/10k/data_app/newtenkdata.rdata",
                  destfile="data/newtenkdata.rdata", method="wget") # Kelly's 10k dataset
    download.file(url="https://storage.googleapis.com/bakar-data/10k/data_app/Guinea_RNA.csv",
                  destfile="data/Guinea_RNA.csv", method="wget") # New Guinea RNA data
    download.file(url="https://storage.googleapis.com/bakar-data/10k/data_app/png_metabolomics.725331.csv",
                  destfile="data/png_metabolomics.725331.csv", method="wget") # New Guinea Metabolomics dataset
    print("Done")
}

addResourcePath("www", paste(getwd() , "/www", sep="") )
addResourcePath("analytics", paste(getwd() , "/analytics", sep="") )
addResourcePath("data", paste(getwd() , "/data", sep="") )

addResourcePath("data_raw", paste(getwd() , "/data_raw", sep="") )

gambia_rna = read.table( "data/Guinea_RNA.csv",  stringsAsFactors=FALSE, header=TRUE, sep="," )
guinea_mass_spec = read.csv( "data/png_metabolomics.725331.csv",  stringsAsFactors=FALSE, header=TRUE, sep="," )

guinea_mass_spec[guinea_mass_spec=="M"] = "Male"
guinea_mass_spec[guinea_mass_spec=="F"] = "Female"
guinea_mass_spec[guinea_mass_spec=="DelayedGroup"] = "Delayed Group"
guinea_mass_spec[guinea_mass_spec=="VaccinatedGroup"] = "Vaccinated Group"

subjects=c("P14_D0","P14_D3","P17_D0","P17_D7","P21_D0","P21_D3","P23_D0","P23_D7","P26_D0","P26_D3","P30_D0","P30_D1","P32_D0","P32_D7","P33_D0","P33_D1","P35_D0","P35_D3")
gambia_rna = gambia_rna[,c("ensembl","hgnc",subjects)]


if( !exists("elisaAnalytes") ){ 
    load('data/newtenkdata1.RData') 
    load('data/newtenkdata2.RData') 
}


whole_blood_counts  =  read.csv( file = 'data/whole_blood_rna_tpm.csv', stringsAsFactors=FALSE ) 
whole_blood_subjects = read.csv( file = 'data/subjects_summary_rna_whole_blood.csv', stringsAsFactors=FALSE ) 

#dim(whole_blood_counts)
#names(whole_blood_counts)

#dim(rna_counts)
#rna_counts[1:10,1:10]
#head(adjusted_matrix)
#dim(rna_counts)
#head(gambia_rna)
#dim(rna_counts)
#head(rna_counts)
#whole_blood_subjects$Subject == names(whole_blood_counts)[c(-1,-2)]

#whole_blood_counts[1:10,1:10]

pbmc_counts   =  read.csv( file = 'data/pbmc_rna_counts.csv', stringsAsFactors=FALSE ) 
pbmc_subjects =  read.csv( file = 'data/subjects_summary_rna_pbmc.csv', stringsAsFactors=FALSE ) 

#all(names(pbmc_counts)[-1] ==  pbmc_subjects$Subject)

ui = div() 

server <- function(input, output, session) { }

ui = tagAppendChild(ui, tags$head(includeScript("analytics/analytics.ucsf.js")) )  
ui = tagAppendChild(ui, tags$head(includeScript("analytics/analytics.org.js"))  )

ui = tagAppendChild(ui,  hidden( selectInput('page', choices = c('home','transcriptomics','proteomics', 'immunoassays','lab'), selected = 'home', label=NULL )) )


callConcat = function(...) {
    ll <- list(...)
    ll <- lapply(ll, function(X) as.list(X)[-1])
    ll <- do.call("c", ll)
    as.call(c(as.symbol("{"), ll))
}

tempHtml = conditionalPanel(title = "homeAbout", condition = "input.page == 'home'",
                                           
        div( class="row", style="background-color: rgb(217,234,248,.5)", div( class="col-xs-12 col col-md-10 col-md-offset-1",                                
            img(src='www/Banner.jpeg', class="img-responsive")
        )),
        div( class="row", div( class="jumbotron img-responsive",  style="padding: .9em; background-color: rgb(250,250,250)",    #rgb(225, 243, 252)
            #p(" In scientific experiments it’s important to compare experimental results against a control dataset. However, sometimes it’s not possible for a lab to easily obtain data from healthy control subjects. This is especially true in immunology, the study of the immune system, where scientists may run multiple costly tests. We built 10k immunomes so that anyone can get high quality data from healthy subjects using any of the latest scientific methods. In minutes, you will have a graph ready for your paper!"),
            p("In science it’s important to compare experimental results against a control dataset. However, sometimes it’s not possible obtain data from healthy control subjects. This is especially true when studying the immune system, where scientists run multiple costly tests. 10k immunomes allows anyone to obtain high quality data and create graphics using the latest scientific methods."),
            p(tags$small( strong('What is 10k Immunomes?'),
               'The 10,000 Immunomes Project is a reference dataset for human immunology, \r derived from over 10,000 control subjects in the', 
                a(href="http://www.immport.org", "NIAID ImmPort Database",inline = T, target = "_blank"), 
                '. Available data include flow cytometry, CyTOF, multiplex ELISA, gene expression, RNA-Seq, Mass Spectrometry, HAI titers,\r clinical lab tests, HLA type, and others. Click one of the buttons below to view visualizations of all the datasets from that type of study. More information about the website can be found in our', 
                a(href = 'https://www.cell.com/cell-reports/fulltext/S2211-1247(18)31451-7', 'Cell Reports Publication',inline = T, target = "_blank"),'.', 
                a(href = 'mailto:BISC_Helpdesk@niaid.nih.gov', 'Contact us', inline = T, target = "_blank"), 
                'with queries and bug reports. All code is openly available on',
                 a(href = 'https://github.com/pupster90/10k_Immunomes', 'Github', inline = T, target = "_blank"), "and",
                 a(href = 'https://hub.docker.com/r/pupster90/io', 'Docker', inline = T, target = "_blank") ) #'~Last updated 7/25/2019.'
          )
       ))
 )

ui = tagAppendChild(ui, tempHtml )
tempHtml


tempHtml= conditionalPanel(title = "graph_page_title", condition = "input.page != 'home'",
                              
  div( class="row",  div( class="jumbotron", style="padding: .9em; background-size: cover ",
       div( style="display: flex;justify-content: center; position: relative; ",
           h1("Graph title text" , style="text-align: center;", id = "graph_page_header"),
           actionButton("homeBtn", icon = icon("home"), label="", class="btn btn-link", style="  outline: none; padding: 15px; border: 0px; background-color: transparent; font-size: 260%;    ") 
       ), 
       p( tags$small( id="graph_page_text" , "Graph title text" , style="text-align:center;"))
  ))
)

ui = tagAppendChild( ui , tempHtml )
#tempHtml



temp = quote({
  observeEvent( input$page , {
      
      if( input$page == 'transcriptomics' ){
          html(id="graph_page_header", html= "Transcriptomics" ) 
          html(id="graph_page_text", html= "Transcriptomics studies gene expression through the analysis of RNA molecules. The amount of RNA molecules recorded for a gene is used to determine the level of expression. This section contains data collected from bulk RNAseq and Microarrays. Until recently Microarrays were the primary method for analyzing gene expression. RNAseq is a newer technique that provides a wider dynamic range and higher sensitivity, but at a higher cost." ) 
      }
      if( input$page == 'proteomics' ){
          html(id="graph_page_header", html= "Proteomics" ) 
          html(id="graph_page_text", html= "Proteomics is the study of proteins. Proteins are molecular machines that execute the functions of a cell. This section contains CyTOF, flow cytometry, and mass spectrometry data. Flow cytometry is a popular laser-based technology to identify cells. The CyTOF and flow cytometry data contain information on relative abundance of cell phenotypes. Mass spectrometry identifies molecules  through their mass-to-charge ratio, which is measured by analyzing the ‘time-of-flight’ of charged particles. Mass spectrometry data provides scientists with an in depth look at an individual’s metabolome at the time of the sample." )   
      }
      if( input$page == 'immunoassays' ){
          html(id="graph_page_header", html= "Immunoassays" ) 
          html(id="graph_page_text", html= "Immunoassays are a useful tool for obtaining precise measurements on the concentration of molecules, bacteria, and viruses inside of a sample.  This technique relies on the strong bonding affinity between an antibody and it’s corresponding antigen. Specific antibodies are chosen to ‘grab’ the particle of interest. Immunoassays can also be used to measure the relative effectiveness of an antibody on a specific virus of interest. We provide immunoassays from ELISA, Multiplex ELISA, HAI Titer, and Virus Neutralization studies." )    
      }
      if( input$page == 'lab' ){
          html(id="graph_page_header", html= "Lab Tests" ) 
          html(id="graph_page_text", html= "Lab tests give important information about the current status of a person's metabolism including the health of essential organs, electrolyte and acid/base balance as well as levels of blood glucose and blood proteins. We provide a diverse set of lab tests on blood count, a metabolic panel, and a lipid profile. Blood Count specifies the various amounts of different cell types in the blood. Lipid profiles measure the amount of different types of lipids like cholesterol or triglycerides. Metabolic panels measure the amount to glucose and the electrolyte balance in blood.")
      }
  })
})
body(server) = callConcat( body(server), temp )

      #if( input$page == 'rna' ){
      #    html(id="graph_page_header", html= "RNA-Seq" ) 
      #    html(id="graph_page_text", html= "Flow cytometry is a popular laser-based technology to analyze cells or particles. It detects and measures physical and chemical characteristics of a population of cells.  In this immunophenotyping technique suspensions of living cells are stained with specific, fluorescently labeled antibodies and then analyzed with a flow cytometer. In the flow cytometer a laser beam is focused on the cell and the light scattered from it is analyzed." )     
      #}
      #if( input$page == 'cytometry' ){
      #    html(id="graph_page_header", html= "Cytometry" ) 
      #    html(id="graph_page_text", html= "Proteomics is the study of proteins. Proteins are molecular machines that execute the functions of a cell. This section contains flow CyTOF, flow cytometry, and mass spectrometry data. The CyTOF and flow cytometry data provides information on relative abundance of cell phenotypes.  The mass spectrometry data provides scientists with an in depth look at an individual’s metabolome at the time of the sample." )   
      #}

tempHtml= div(
    column(3, actionButton("transcriptomicsBtn", icon = icon("dna"), label="Transcriptomics", class="btn btn-success btn-lg text-center", style="width: 100%"), style="padding: 2px; "),
    column(3, actionButton("proteomicsBtn", icon = icon("microscope"), label="Proteomics", class="btn-primary btn-lg text-center",style="width: 100%"), style="padding: 2px;"),
    column(3, actionButton("immunoassaysBtn", icon = icon("yahoo"), label="Immunoassays", class="btn btn-warning btn-lg text-center", style="width: 100%"), style="padding: 2px;"),
    column(3, actionButton("labBtn", icon = icon("vial"), label="Lab Tests", class="btn btn-danger btn-lg text-center", style="width: 100%"),  style="padding: 2px; "),
    p(".", style="color:white;"),
    HTML("<hr>")
)

ui = tagAppendChild(ui, tempHtml )
#tempHtml


temp = quote({
  observeEvent(input$homeBtn, {
    updateSelectInput(session, "page", selected = 'home')
  })
  observeEvent(input$transcriptomicsBtn, {
    updateSelectInput(session, "page", selected = 'transcriptomics')
    updateSelectInput(session, "dataType", selected = 'Microarray: PBMC', choices = c('Microarray: PBMC','Microarray: Whole Blood','RNAseq: T cells','RNAseq: Whole Blood', 'RNAseq: PBMC'))#, 'Cibersort: Whole Blood'))
    #updateSelectInput(session, "dataType", selected = 'Gene Expression: PBMC', choices = c('Gene Expression: PBMC','Gene Expression: Whole Blood'))   
  })
  observeEvent(input$proteomicsBtn, {
    updateSelectInput(session, "page", selected = 'proteomics')
    #updateSelectInput(session, "dataType", selected = 'Mass-Spec: Newborns', choices = c('CyTOF: PBMC','Flow Cytometry: Whole Blood', 'Flow Cytometry: PBMC','Mass-Spec: Newborns'))
    updateSelectInput(session, "dataType", selected = 'CyTOF: PBMC', choices = c('CyTOF: PBMC','Flow Cytometry: Whole Blood', 'Flow Cytometry: PBMC','Mass Spectrometry: Newborns'))  
  })
  observeEvent(input$immunoassaysBtn, {
    updateSelectInput(session, "page", selected = 'immunoassays')
    updateSelectInput(session, "dataType", selected = 'HAI Titer', choices = c('HAI Titer','ELISA', 'Multiplex ELISA', 'Virus Neutralization Titer'))
  })   
  observeEvent(input$labBtn, {
    updateSelectInput(session, "page", selected = 'lab')
    updateSelectInput(session, "dataType", selected = 'Blood Count', choices = c('Blood Count','Metabolic Panel','Lipid Profile'))
  })
})
body(server) = callConcat( body(server), temp )

  #observeEvent(input$cytometryBtn, {
  #  updateSelectInput(session, "page", selected = 'cytometry')
  #  #updateSelectInput(session, "dataType", selected = 'CyTOF: PBMC', choices = c('CyTOF: PBMC','Flow Cytometry: Whole Blood', 'Flow Cytometry: PBMC'))  
  #  updateSelectInput(session, "dataType", selected = 'CyTOF: PBMC', choices = c('RNAseq: T cells','RNAseq: Newborns'))
  #})

tempHtml= conditionalPanel(title = "homeMain", condition = "input.page == 'home'",
   div(class="jumbotron", style="background-color: white; padding: .9em",

        #column(12, column(7,tags$u( h2("Made with ImmPort", class="text-center")  ))),
        column( 7, a(href="http://www.immport.org", img(src='www/Figure_1.png', class="img-responsive"), inline = T, target = "_blank")  ),
        column( 5, 
                p(tags$small("Data from 242 studies and 44,775 subjects was collected from the NIAID Immunology Data and Analysis Portal,",a(href="http://www.immport.org", "ImmPort", inline = T, target = "_blank"),". It includes flow cytometry, CyTOF, mRNA expression, secreted protein levels, clinical lab tests, HAI titers, HLA type, and others. We hand curated the entire contents of ImmPort to filter for normal healthy human control subjects. Each of the data types were systematically processed and harmonized. This data constitutes the largest compendium to date of cellular and molecular immune measurements on healthy normal human subjects."))
              ),
       p(".", style="color:white; padding:0; margin:0; font-size:50%;"), # makes a vertical space,
       HTML("<hr>"),
       
       column( 5,
              tags$u( h2("10,000 Subjects")  ),
              p(tags$small("Below is a table displaying the number of subjects in each dataset. Counts of distinct subjects for whom raw data of each type is represented in the initial release of the 10KIP. Because many subjects contributed multiple measurement types, the totals across all measurement types substantially exceed the number of distinct subjects."))
            ),
       column( 5,
              a(href="http://www.immport.org", img(src='www/Table_1.jpg', class="img-responsive"), inline = T, target = "_blank")
            ),
      # Youtube Video
       div( class="col-xs-12 col-sm-7 col-md-6", style="padding-top: 5px;", 
         div( class="embed-responsive embed-responsive-16by9",
              HTML('<iframe width="200" height="100" src="https://www.youtube.com/embed/pwBs4J4xDOw" class="embed-responsive-item"></iframe>')
             )
          ),
        # Links for external sites and Datasets
        div( class="col-xs-12 col-sm-12 col-md-6 text-center",
                p(".", style="color:white"), # makes a vertical space
                h3("Learn More"), 
                p( #tags$small(
                  a(href = 'http://www.immport.org/immport-open/public/home/home', 'ImmPort Homepage'), br(),
                  a(href = 'https://www.cell.com/cell-reports/fulltext/S2211-1247(18)31451-7', '10,000 Immunomes Paper', target = "_blank"), br(),
                  a(href = 'https://www.cell.com/cell-reports/fulltext/S2211-1247(18)31080-5', 'MetaCyto Cytometry Analysis Paper', target = "_blank"), br(),
                  a(href = 'https://bioconductor.org/packages/release/bioc/html/MetaCyto.html', 'MetaCyto Cytometry Analysis Code', target = "_blank"), br(),
                  a(href = 'https://www.bu.edu/jlab/wp-assets/ComBat/Abstract.html', 'ComBat Batch Correction Algorithm', target = "_blank"), br(),
                  a(href = 'https://storage.googleapis.com/bakar-data/10k/data_raw/Questionnaire.zip', '10k Questionnaire Dataset', target = "_blank"), br(),
                  a(href = 'https://storage.googleapis.com/bakar-data/10k/data_raw/hla.csv', '10k HLA Dataset', target = "_blank"), br(),  
                  a(href = 'mailto:BISC_Helpdesk@niaid.nih.gov','Contact Us', target = "_blank")
              ) #)
        )
 



    ),
    div(                        
      column(12,
        tags$hr(),tags$hr(),     
        h3('Cite 10k Immunomes'),
        p("Zalocusky KA, Kan MJ, Hu Z, Dunn P, Thomson E, Wiser J, Bhattacharya S, Butte AJ. The 10,000 Immunomes Project: Building a Resource for Human Immunology. Cell reports. 2019 Oct 9;25(2):513-22. PMID:30304689"  )
        ),
      column(12,
        h3('REFERENCES'),
        p('1) Hu Z, Jujjavarapu C, Hughey JJ, Andorf S, Lee H, Gherardini PF, Spitzer MH, et al. Meta-analysis of Cytometry Data Reveals Racial Differences in Immune Cells. Cell Reports. 2018 Jul 31;24(5):1377-88. ' ),
        p('2) Finak G, Langweiler M, Jaimes M, Malek M, Taghiyar J, Korin Y, et al. Standardizing Flow Cytometry Immunophenotyping Analysis from the Human ImmunoPhenotyping Consortium. Scientific Reports. 2016 Aug 10;6(1):20686.' ),
        p('3) Johnson WE, Li C, Rabinovic A. Adjusting batch effects in microarray expression data using empirical Bayes methods. Biostat. 2007 Jan 1;8(1):118–27. ' ),
        p('4) Irizarry RA, Hobbs B, Collin F, Beazer‐Barclay YD, Antonellis KJ, Scherf U, et al. Exploration, normalization, and summaries of high density oligonucleotide array probe level data. Biostatistics. 2003 Apr 1;4(2):249–64. ' ),
        p('5) Zalocusky KA, Kan MJ, Hu Z, Dunn P, Thomson E, Wiser J, Bhattacharya S, Butte AJ. The 10,000 Immunomes Project: Building a Resource for Human Immunology. Cell reports. 2018 Oct 9;25(2):513-22. PMID:30304689' ),
        p('6) Lee AH, Shannon CP, Amenyogbe N, Bennike TB, Diray-Arce J, Idoko OT, et al. Dynamic molecular changes during the first week of human life follow a robust developmental trajectory. Nature Communications. 2018 March 12;10:1092.' )
        ),
      p(".", style="color:white") # makes a vertical space
   ) #<- end jumbotron
) #<-- end conidtional panel
ui = tagAppendChild(ui, tempHtml )
#tempHtml


sidebar = sidebarPanel(  #div( class="col-sm-4", 
        
        #Select which data type to visualize
        h4('Select Data Type:'),
        selectInput('dataType', label = NULL,
                    choices = c('CyTOF: PBMC','ELISA',
                                'Flow Cytometry: Whole Blood', 'Flow Cytometry: PBMC',
                                'Microarray: PBMC','Microarray: Whole Blood',
                                'HAI Titer', 
                                'Blood Count','Metabolic Panel',
                                'Lipid Profile','Multiplex ELISA', 'Virus Neutralization Titer',
                                'RNAseq: T cells', 'RNAseq: Whole Blood', 'RNAseq: PBMC', 'Mass Spectrometry: Newborns'), #'Cibersort: Whole Blood'),
                    selected = 'CyTOF: PBMC'
        ),
        
        ##Conditional panels for analyte selection
        #Which drop-down menu appears will depend on the data type the user selects.
        
        # Analyte Selection Box
        conditionalPanel(condition = "input.dataType == 'Mass Spectrometry: Newborns' ", 
                         h4('Enter Analyte Name'),
                         selectizeInput( 'mass_spec_analyte', label= NULL, choices= NULL,
                                          selected='cholesterol', options= list(maxOptions=10) )
        ),     
        conditionalPanel(condition = "input.dataType == 'CyTOF: PBMC'",
                         h4('Select Analyte:'),
                         selectInput('cytof_pbmc_analyte', label = NULL, #style="padding-bottom: 0px; margin-bottom: 0px;",
                                     choices = cytof_pbmc_analytes,
                                     selected = 'T_cells')
        ),
        conditionalPanel(condition = "input.dataType == 'ELISA'",
                         h4('Select Analyte:'),
                         selectInput('elisa_analyte', label = NULL,
                                     choices = elisaAnalytes,
                                     selected = 'CXCL10')
        ),
        conditionalPanel(condition = "input.dataType == 'Flow Cytometry: PBMC'",
                         h4('Select Analyte:'),
                         selectInput('flow_pbmc_analyte', label = NULL,
                                     choices =  flow_pbmc_analytes,
                                     selected = 'T_cells')
        ), 
    
    
        # Cytometry: Additional Analyte Information
        conditionalPanel(condition = "input.dataType == 'CyTOF: PBMC'",
                    textOutput("cytof_label"), HTML("<hr>")
        ),
    
        # Normalization Box
        conditionalPanel( condition = "['CyTOF: PBMC','ELISA','Flow Cytometry: PBMC','Microarray: PBMC','Microarray: Whole Blood','HAI Titer','Multiplex ELISA','Blood Count','Virus Neutralization Titer'].indexOf(input.dataType) >= 0",
            fluidRow(
                column( 6, checkboxInput('processing', label = 'Normalized',  value = T)  ),
                column( 6, checkboxInput('outliers', label = 'Outliers',  value = T)  )
            )
        ),
        # Outliers Box
        #conditionalPanel( condition = "['CyTOF: PBMC','ELISA','Flow Cytometry: PBMC','Microarray: PBMC','Microarray: Whole Blood','HAI Titer','Multiplex ELISA','Blood Count','Virus Neutralization Titer'].indexOf(input.dataType) >= 0",
        #                  checkboxInput('processing', label = 'Normalized',  value = T)
        #),
    
    
    
        # Analyte Selections
        conditionalPanel(condition = "input.dataType == 'Flow Cytometry: Whole Blood'",
                         h4('Select Analyte:'),
                         selectInput('flow_blood_analyte', label = NULL,
                                     choices = flow_blood_analytes,
                                     selected = 'T_cells')
        ),
        conditionalPanel(condition = "input.dataType == 'Microarray: PBMC'",
                         h4('Enter HUGO Gene Symbol:'),
                         selectizeInput( 'gene_pbmc_analyte', label= NULL, choices= NULL,
                                          selected='CD9', options= list(maxOptions=10) )
                         #textInput('gene_pbmc_analyte', label = NULL,
                         #          value = 'CD9')
                         
        ),
        conditionalPanel(condition = "input.dataType == 'Microarray: Whole Blood'",
                         h4('Enter HUGO Gene Symbol:'),
                         selectizeInput( 'gene_blood_analyte', label= NULL, choices= NULL,
                                          selected='CD9', options= list(maxOptions=10) )
                         #textInput('gene_blood_analyte', label = NULL,
                         #          value = 'CD9')
        ),
        conditionalPanel(condition = "input.dataType == 'HAI Titer'",
                         h4('Select Analyte:'),
                         selectInput('hai_analyte', label = NULL,
                                     choices = haiAnalytes,
                                     selected = haiAnalytes[1])
        ),
        conditionalPanel(condition = "input.dataType == 'Multiplex ELISA'",
                         h4('Select Analyte:'),
                         selectInput('mbaaAnalyte', label = NULL,
                                     choices = mbaaAnalytes,
                                     selected = 'CXCL5')
        ),
        conditionalPanel(condition = "input.dataType == 'Blood Count'",
                         h4('Select Analyte:'),
                         selectInput('cbcAnalyte', label = NULL,
                                     choices = cbcAnalytes,
                                     selected = 'HGB_g_per_dL')
        ),
        conditionalPanel(condition = "input.dataType == 'Lipid Profile'",
                         h4('Select Analyte:'),
                         selectInput('flpAnalyte', label = NULL,
                                     choices = flpAnalytes,
                                     selected = flpAnalytes[1])
        ),
        conditionalPanel(condition = "input.dataType == 'Metabolic Panel'",
                         h4('Select Analyte:'),
                         selectInput('cmpAnalyte', label = NULL,
                                     choices = cmpAnalytes,
                                     selected = cmpAnalytes[1])
        ),
        conditionalPanel(condition = "input.dataType == 'Virus Neutralization Titer'",
                         h4('Select Analyte:'),
                         selectInput('vntAnalyte', label = NULL,
                                     choices = vntAnalytes,
                                     selected = 'Measles_Edmonston')
        ),
        conditionalPanel(condition = "input.dataType == 'RNAseq: T cells' ", 
                         h4('Enter HUGO Gene Symbol:'),
                         selectizeInput( 'T_cell_analyte', label= NULL, choices= NULL,
                                          selected='CD9', options= list(maxOptions=10) )
                         #textInput('T_cell_analyte', label = NULL,
                         #          value = 'CD9')
        ),
        conditionalPanel(condition = "input.dataType == 'RNAseq: Whole Blood' ", 
                         h4('Enter HUGO Gene Symbol:'),
                         selectizeInput( 'RNAseq_newborns', label= NULL, choices= NULL,
                                          selected='CD9', options= list(maxOptions=10) )
                         #textInput('T_cell_analyte', label = NULL,
                         #          value = 'CD9')
        ),
        conditionalPanel(condition = "input.dataType == 'RNAseq: PBMC' ", 
                         h4('Enter HUGO Gene Symbol:'),
                         selectizeInput( 'RNAseq_pbmc', label= NULL, choices= NULL,
                                          selected='CD4', options= list(maxOptions=10) )
                         #textInput('T_cell_analyte', label = NULL,
                         #          value = 'CD9')
        ),
        conditionalPanel( condition = "input.dataType == 'Mass Spectrometry: Newborns' || input.dataType == 'RNAseq: Whole Blood' || input.dataType == 'RNAseq: PBMC'", #|| input.dataType == 'Cibersort: Whole Blood' ",
             h4('Plot By:'),
             radioButtons( 'newbornPlotBy', label = NULL,
                          #choices = c('Days of Life','Sex','Vaccination'),
                          choices = c('Age','Sex'), # we removed vaccination, since we are now only plotting 'delayed vaccine' group
                          selected = 'Age'
             ) 
        ),
        
        #Select age range of subjects
        conditionalPanel(condition = "input.dataType != 'RNAseq: T cells' && input.dataType != 'RNAseq: Whole Blood' && input.dataType != 'RNAseq: PBMC' && input.dataType != 'Mass Spectrometry: Newborns'", #&& input.dataType != 'Cibersort: Whole Blood'",
                         h4('Age Range:'),
                         sliderInput("ageRange",label = NULL,
                                     min = 0,
                                     max = 100,
                                     value = c(0,100),
                                     dragRange = TRUE,
                                     round = TRUE,
                                     step = 1),
                         
                                                
                         
                #Select the ethnicities of subjects
                 fluidRow(
                         column( 8, 
                             h4('Ethnicities:'),
                             checkboxGroupInput('race', label = NULL,
                                                choices = c('White', 'Black or African American','Asian','Other'),
                                                selected = c('White', 'Black or African American','Asian','Other')
                             )
                        ),
                         #How to color plot
                         column( 4 ,
                             h4('Plot By:'),
                             radioButtons('colorCode', label = NULL,
                                          choices = c('Age & Sex','Ethnicity','Study'),
                                          selected = 'Age & Sex'
                             )                  
                         )
                     
                     ),
                         
                 fluidRow(   
                                                  #Select male or female subjects
                         column( 12 ,
                             h4('Sex:'),
                             checkboxGroupInput("gender", label = NULL, choices = c('Female','Male'), selected = c('Female','Male'), inline=TRUE  )
                        )
                     )
                         

        )
        
      )
       

temp= quote({
    updateSelectizeInput(session, 'mass_spec_analyte',  choices= colnames(guinea_mass_spec)[-1:-8], server= TRUE)
    updateSelectizeInput(session, 'gene_pbmc_analyte',  choices= colnames(gene_pbmc)[-1:-8], server= TRUE)
    updateSelectizeInput(session, 'gene_blood_analyte', choices= colnames(gene_blood)[-1:-8], server= TRUE)
    updateSelectizeInput(session, 'T_cell_analyte',     choices= colnames(gene_Tcell_raw)[-1:-3], server= TRUE)
    updateSelectizeInput(session, 'RNAseq_newborns',    choices= whole_blood_counts$hgnc, server= TRUE)
    updateSelectizeInput(session, 'RNAseq_pbmc',        choices= pbmc_counts$hgnc, server= TRUE)
    # DEBUG
    #updateSelectizeInput(session, 'RNAseq_newborns',    choices= gambia_rna$hgnc, server= TRUE)
})
body(server) = callConcat( body(server), temp )


myDownloadButton <- function(outputId, label = "Download", myIcon="download", class = NULL, ... ){
     tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", class),
         href = "", target = "_blank", download = NA, 
         icon(myIcon) , label, ...)
}

main_panel = mainPanel(   #div( class="col-sm-8",
        fluidRow( div( class="col-md-offset-9  col-xs-offset-8",  #col-sm-offset-8 col-lg-offset-9 
                   tags$b( style="font-family: 'Times New Roman'; color: DarkSlateGrey; font-size: 115%", textOutput("num_subjects",inline = F) ) # 
                 ) ),
        plotlyOutput("dataPlot"),
        #tags$div( style = 'width:100; float:right; color:black',tags$b( textOutput("num_subjects",inline = F) ) ),
        #tags$div(class = 'container2', style = 'width:100; float:right; color:black',textOutput('nSubsText', inline = F)),
        #h6( id='numSubjects', class="pull-right", "Number of Subjects: " ),
        h2( class="col-xs-offset-2 col-md-offset-1", "Download"),
        div(
            myDownloadButton('downloadPlot', label='Image', myIcon="image", class="btn btn-success"),
            myDownloadButton('downloadPlotData', label='Plot Data', myIcon="file-download", class="btn btn-warning"),
            tags$button(  id="downloadAllData", class="btn", style="padding:0"#,  #  id='downloadAllData'
               #HTML("<a  href='https://storage.googleapis.com/bakar-data/10k/png_metabolomics.725331.csv' download ><button class='btn btn-danger'><i class='fa fa-cart-arrow-down'></i> All Data</button></a>")
            )
            #myDownloadButton('downloadAllData', label='All Data', myIcon="cart-arrow-down", class="btn btn-danger")
        ),
        p( style = 'color: DarkGrey; padding: 0px;', "* All Data is dataset's raw and formatted files")
        #h6( id='numSubjects', class="pull-right", "Number of Subjects: " ),
        #textOutput("num_subjects")
      )



tempHtml  = conditionalPanel( title="graphMain", condition = "input.page != 'home'",             
    sidebarLayout( sidebar, main_panel )             
)    
ui = tagAppendChild(ui, tempHtml )
#tempHtml

tempHtml = conditionalPanel( title="info_text", condition = "input.page != 'home'",   
    div( class="jumbotron", style="background-color: white; padding: .9em;",  # padding-left: .9em; padding-top: 0px;",
         p( id="graph_data_info" , '..........' )
   )                        
)
ui = tagAppendChild(ui, tempHtml )


temp = quote({
  observeEvent( input$dataType , {
      
      if( input$dataType == 'CyTOF: PBMC' ){
          html(id="graph_data_info", html= "<b>CYTOF Data: </b><small>CyTOF data of healthy human blood samples were downloaded from ImmPort web portal. Every .fcs file was pared down to 5000 events. These .fcs files constitute the “raw” CyTOF:PBMC data. All data were arcsinh transformed. For CyTOF data, the formula f(x) = arcsinh (x/8) was used. Transformation and compensation were done using the preprocessing.batch function in MetaCyto (1). The cell definitions from the Human ImmunoPhenotyping Consortium (2) were used to identify 24 types of immune cells using the searchClster.batch function in MetaCyto. Specifically, each marker in each cytometry panels was bisected into positive and negative regions. Cells fulfilling the cell definitions are identified. For example, the CD14+ CD33+ CD16- (CD16- monocytes) cell subset corresponds to the cells that fall into the CD14+ region, CD33+ region and CD16- region concurrently. The proportion of each cell subsets in the PBMC were then calculated by dividing the number of cells in a subset by the total number of cells in the blood. These steps together produce the “formatted” CyTOF: PBMC data. These data were then batch-corrected with an established empirical Bayes method (3), using study accession for batch and age, sex, and race as known covariates to produce the “formatted and normalized” CyTOF: PBMC data. </small>" ) 
      
      }else if( input$dataType == 'ELISA' ){
          html(id="graph_data_info", html= "<b>ELISA Data: </b><small>Parsed ELISA data were downloaded from ImmPort. Analyte names were standardized to HUGO gene names where appropriate, and measurements were standardized to a common unit of measurement (pg/mL). These steps produced the “formatted” ELISA data. Because ELISA data is low-throughput, and most subjects only have measurements for one analyte, batch correction was conducted with a simple linear model for each analyte, mean correcting by study accession while accounting for age, sex, and race. These steps produced the “formatted and normalized” ELISA data.</small>" ) 
      
      }else if( input$dataType %in% c('Flow Cytometry: Whole Blood','Flow Cytometry: PBMC')  ){
          html(id="graph_data_info", html= "<b>Cytometry Data: </b><small>Meta-analysis of Cytometry data is conducted using the MetaCyto package (1). Briefly, flow cytometry data were downloaded from ImmPort web portal. Every .fcs file was pared down to 5000 events. These .fcs files constitute the “raw” Flow Cytometry:PBMC data. Flow cytometry data from ImmPort were compensated for fluorescence spillovers using the compensation matrix supplied in each fcs file.  All data from ImmPort were arcsinh transformed. For flow cytometry data, the formula f(x) = arcsinh (x/150) was used. Transformation and compensation were done using the preprocessing.batch function in MetaCyto (1). The cell definitions from the Human ImmunoPhenotyping Consortium (2) were used to identify 24 types of immune cells using the searchClster.batch function in MetaCyto. Specifically, each marker in each cytometry panels was bisected into positive and negative regions. Cells fulfilling the cell definitions are identified. For example, the CD14+ CD33+ CD16- (CD16- monocytes) cell subset corresponds to the cells that fall into the CD14+ region, CD33+ region and CD16- region concurrently. The proportion of each cell subsets in the PBMC or whole blood were then calculated by dividing the number of cells in a subset by the total number of cells in the blood. These steps together produce the “formatted” Flow Cytometry: PBMC data. Because the Flow Cytometry data are sparse, batch correction was conducted with a simple linear model for each cell type, mean correcting by study accession while accounting for age, sex, and race. These steps produced the “formatted and normalized” Flow Cytometry data.</small>")
      
      }else if( input$dataType %in% c('Microarray: PBMC','Microarray: Whole Blood') ){
          html(id="graph_data_info", html= "<b>Microarray Data: </b><small>Gene expression array data were obtained in three formats. Data in their original formats (.CEL files, series matrix files, etc) constitute the “raw” gene expression data. For data collected on Affymetrix platforms, we utilized the ReadAffy utility in the affy Bioconductor package to read in raw .CEL files. The rma utility was used to conduct Robust Multichip Average (rma) background correction (as in (4)), quantile normalization, and log2 normalization of the data. For data collected on Illumina platforms and stored in the Gene Expression Omnibus (GEO) database, we utilized the getGEO utility in the GEOquery Bioconductor package to read the expression files and the preprocessCore package to conduction rma background correction, quantile normalization, and log2 normalization of the gene expression data. Finally, for data collected on Illumina platforms but not stored in GEO, we utilized the read.ilmn utility of the limma Bioconductor package to read in the data, and the neqc function to rma background correct, quantile normalize, and log2 normalize the gene expression data. In all instances, probe IDs were converted to Entrez Gene IDs. Where multiple probes mapped to the same Entrez Gene ID, the median value across probes was used to represent the expression value of the corresponding gene. The background-corrected and normalized datasets were combined based on common Entrez IDs, missing values were imputed with a k-nearest neighbors algorithm (R package: impute, function: impute.knn) using k = 10 and default values for rowmax, colmax, and maxp. Enter Gene IDs were then converted to HUGO gene names. These steps together produced the “formatted” gene expression files. To create the “formatted and normalized” datasets, we utilized established empirical Bayes algorithm for batch correction (2), compensating for possible batch effects while maintaining potential effects of age, race, and sex across datasets.</small>") 
      
      }else if( input$dataType == 'HAI Titer' ){
          html(id="graph_data_info", html= "<b>HAI Titer Data: </b><small>Parsed HAI data were downloaded from ImmPort. Names were standardized to WHO viral nomenclature where necessary. These steps produced the “formatted” HAI data. Because HAI data is low-throughput, and most subjects only have measurements for one-to-three of the viruses, batch correction was conducted with a simple linear model for each analyte, mean correcting by study accession while accounting for age, sex, and race. These steps produced the “formatted and normalized” HAI data.</small>")
      
      }else if( input$dataType %in%  c('Blood Count','Metabolic Panel','Lipid Profile') ){
          html(id="graph_data_info", html= "<b>Lab Test Data: </b><small>Parsed lab test data were downloaded from ImmPort and organized into three standard panels: Complete Blood Count (CBC), Fasting Lipid Profile (FLP), and Comprehensive Metabolic Panel (CMP). Because FLP and CMP data are derived from only one study, no further standardization was required. These parsed data constitute the 'formatted' lab test data for these two types, and no 'normalized' table is available. CBC data were derived from 12 different studies. As such, names of individual tests as well as units of measurement needed to be standardized for the data to be directly comparable. For example, cells reported as thousands of cells per microliter were variously described as 'K/mi', 'K/', “cells/mm3”, “thou/mcL”, ”per”, “1000/microliter”, “10^3/mm3”, “10^3”, “1e3/uL”, “10*3/ul”, “/uL”, or “10^3 cells/uL”, and the names of assays were comparably variable. These standardization steps produced the “formatted” Lab Test: Blood Count data. These data were then batch corrected with a simple linear model for each analyte, mean correcting by study accession while accounting for age, sex, and race to produce the “formatted and normalized” CBC data.</small>")
      
      }else if( input$dataType == 'Multiplex ELISA' ){
          html(id="graph_data_info", html= "<b>Multiplex ELISA Data: </b><small>Secreted protein data measured on the multiplex ELISA platform were collected from ImmPort studies SDY22, SDY23, SDY111, SDY113, SDY180, SDY202, SDY305, SDY311, SDY312, SDY315, SDY420, SDY472, SDY478, SDY514, SDY515, SDY519, and SDY720. Data were drawn from the ImmPort parsed data tables using RMySQL or loaded into R from user-submitted unparsed data tables. Across the studies that contribute data, there are disparities in terms of the dilution of samples and units of measure in which the data are reported. We corrected for differences in dilution factor and units of measure across experiments and standardized labels associated with each protein as HUGO gene symbols. This step represents the “formatted” Multiplex ELISA data table.  Compensation for batch effects was conducted using an established empirical Bayes algorithm (2), with study accession representing batch and a model matrix that included age, sex, and race of each subject. Data were log2 transformed before normalization to better fit the assumption that the data are normally distributed. The effectiveness of the log2 transform, as well as our batch correction efforts, are detailed in the manuscript associated with this resource (5). This batch-corrected data represents the “formatted and normalized” Multiplex ELISA data. </small>")
      
      }else if( input$dataType == 'Virus Neutralization Titer' ){
          html(id="graph_data_info", html= "<b>Virus Neutralization Data: </b><small>Parsed VNT data were downloaded from ImmPort. Names were standardized to WHO viral nomenclature where necessary. These steps produced the “formatted” VNT data. Because VNT data is low-throughput, and most subjects only have measurements for one-to-three of the viruses, batch correction was conducted with a simple linear model for each analyte, mean correcting by study accession while accounting for age, sex, and race. These steps produced the “formatted and normalized” VNT data.</small>")
      
      }else if( input$dataType %in% c( 'RNAseq: Whole Blood', 'RNAseq: PBMC') ){ #, 'Cibersort: Whole Blood') ){
          html(id="graph_data_info", html= "<b>RNA-Seq Whole Blood and PBMC Data: </b><small>The RNA-Seq whole blood data was downloaded from Immport  (Studies: SDY1092, SDY1172, SDY1381, and SDY1412). The RNA-Seq peripheral mononucler cells comes from Immport study SDY67. The files used from Immport were formatted and normalized by the EPIC Consortium. The whole blood datasets were converted into TPM files and harmonized together using the HarmonyRNA algorithm. More information on the study can be found in this <a href=‘https://www.nature.com/articles/s41467-019-08794-x’>Nature Article</a> published by Amy H. Lee, et al. Some minor additional formatting was done for presenting data on 10k Immunomes.</small>") 
            
      }else if( input$dataType %in% c('Mass Spectrometry: Newborns') ){ #, 'Cibersort: Whole Blood') ){
          html(id="graph_data_info", html= "<b>Newborn Mass Spectrometry Data: </b><small>The newborn RNA-Seq and Mass Spectrometry datasets were downloaded from Immport  (Studies: SDY1256 and SDY1412). The RNAseq data comes from peripheral blood and Mass-Spec data comes from blood plasma. Phenotyping data was obtaiend by from running Cibersort on RNAseq data. The files used from Immport were formatted and normalized by the EPIC Consortium. More information on the study can be found in this <a href=‘https://www.nature.com/articles/s41467-019-08794-x’>Nature Article</a> published by Amy H. Lee, et al. Some minor additional formatting was done for presenting data on 10k Immunomes.</small>") 
      
      }else if( input$dataType == 'RNAseq: T cells' ){
          html(id="graph_data_info", html= "<b>RNA-Seq T Cell Data: </b><small>The T cells' raw sequence read files were download from the Sequence Read Archive (SRA). The datasets relate to ImmPort studies SDY888 and SDY1324. The sequence reads are quantified at the gene level using Kallisto. The Transcripts per million (TPM) is used to quantify the transcription levels in T cells.</small>")

      }else{
          html(id="graph_data_info", html= "   ")
      }
  })
})
body(server) = callConcat( body(server), temp )

temp= quote({
#select the analyte for plotting based on user input
analyte <- reactive({ req(switch(input$dataType, #<-- req() makes it so that the switch stops, when user hasn't made selection yet (stops errors)
                              "CyTOF: PBMC" = input$cytof_pbmc_analyte,
                              "ELISA" = input$elisa_analyte,
                              "Flow Cytometry: PBMC" = input$flow_pbmc_analyte,
                              "Flow Cytometry: Whole Blood" = input$flow_blood_analyte,
                              "Microarray: Whole Blood" = input$gene_blood_analyte,
                              "Microarray: PBMC" = input$gene_pbmc_analyte,
                              "Gene Set Enrichment: Whole Blood" = input$gsea_analyte,
                              "HAI Titer" = input$hai_analyte,
                              "HLA Type" = input$hla_analyte,
                              "Multiplex ELISA" = input$mbaaAnalyte,
                              "Blood Count" = input$cbcAnalyte,
                              "Lipid Profile" = input$flpAnalyte,
                              "Metabolic Panel" = input$cmpAnalyte,
                              "Virus Neutralization Titer" = input$vntAnalyte,
                              "RNAseq: T cells" = input$T_cell_analyte,
                              "RNAseq: Whole Blood" = input$RNAseq_newborns,
                              "RNAseq: PBMC" = input$RNAseq_pbmc,
                              #"Cibersort: Whole Blood" = input$cibersort_analyte,
                              "Mass Spectrometry: Newborns" = input$mass_spec_analyte
  ))})
})
body(server) = callConcat( body(server), temp )


kelly_data= list('Multiplex ELISA RAW'=mbaa_raw, 'Flow Cytometry: PBMC RAW'=flow_pbmc_raw, 'CyTOF: PBMC RAW'=cytof_pbmc_raw, "ELISA RAW"=elisa_raw,
                 "Microarray: Whole Blood RAW"=gene_blood_raw, "Microarray: PBMC RAW"=gene_pbmc_raw, "HAI Titer"=hai_raw, "Blood Count RAW"=cbc_raw,
                 'Virus Neutralization Titer RAW'=vnt_raw, "CyTOF: PBMC"=cytof_pbmc, "ELISA"=elisa, "Flow Cytometry: PBMC"=flow_pbmc, "Blood Count"=cbc,
                 "Flow Cytometry: Whole Blood"=flow_blood_raw, "Microarray: Whole Blood"=gene_blood, 'Virus Neutralization Titer' = vnt, 
                 "Microarray: PBMC"=gene_pbmc, "HAI Titer"=hai, "HLA Type"=hla, "Multiplex ELISA"=mbaa, "Lipid Profile"=flp, "Metabolic Panel" = cmp #,
                )

temp= quote({
    # Generate a Title
    title <- reactive({paste(gsub('_',' ',analyte()), 'by Subject')})   
    
    # Generate Labels
    ylab <- reactive({ 
      switch( input$dataType ,
             "CyTOF: PBMC" = paste(gsub('_',' ',analyte()),'(percent)'),
             "ELISA"= paste(analyte(), '(Concentration in pg/mL)'),
             "Flow Cytometry: PBMC" = paste(gsub('_',' ',analyte()),'(percent)'),
             "Flow Cytometry: Whole Blood" = paste(gsub('_',' ',analyte()), '(percent)'),
             "Microarray: Whole Blood" = paste(analyte(), '(Expression)'),
             "Microarray: PBMC" = paste(analyte(), '(Expression)'),
             "Gene Set Enrichment: Whole Blood" = paste(analyte()),
             "HAI Titer" = paste(analyte(), '(HAI Titer)'),
             "Multiplex ELISA" = paste(analyte(), '(Concentration in pg/mL)'),
             "Blood Count" = analyte(),
             "Lipid Profile" = analyte(),
             "Metabolic Panel" = analyte(),
             "Virus Neutralization Titer" = paste(analyte(), '(Virus Neutralization Titer)'),
             "RNAseq: T cells" = paste(analyte(), '(Expression)'),
             "RNAseq: Newborns" = paste(analyte(), '(Expression)'),
             #"Cibersort: Whole Blood" = paste(analyte(), '(Expression)'),
             "Mass Spectrometry: Newborns" = paste(analyte(), '(Expression)')
      )
    })

})
body(server) = callConcat( body(server), temp )

cyto_labels= list('B_cells'="CD19+,CD3- or CD19+,CD20+", 'CD16_neg_monocytes'="CD14+,CD33+,CD16-", 'CD16_pos_monocytes'="CD14+,CD33+,CD16+",
                  'CD4_T_cells'="CD3+,CD4+", 'CD8_T_cells'="CD3+,CD8+", 'Central_Memory_CD4_T_cells'="CD3+,CD4+,CCR7+,CD45RA-",
                  'Central_Memory_CD8_T_cells'="CD3+,CD8+,CCR7+,CD45RA-", 'Effector_CD4_T_cells'="CD3+,CD4+,CCR7-,CD45RA+",
                  'Effector_CD8_T_cells'="CD3+,CD8+,CCR7-,CD45RA+", 'Effector_Memory_CD4_T_cells'="CD3+,CD4+,CCR7-,CD45RA-",
                  'Effector_Memory_CD8_T_cells'="CD3+,CD8+,CCR7-,CD45RA-", 'Gamma_Delta_T_cells'="TCRgd+,CD3+", 'Lymphocytes'="CD14-,CD33-",
                  'Memory_B_cells'="CD3-,CD19+,CD20+,CD24+,CD38-", 'Monocytes'="CD14+,CD33+", 'Naive_B_cells'="CD3-,CD19+,CD20+,CD24-,CD38+",
                  'Naive_CD4_T_cells'="CD3+,CD4+,CCR7+,CD45RA+", 'Naive_CD8_T_cells'="CD3+,CD8+,CCR7+,CD45RA+", 'NK_cells'="CD3-,CD16+,CD56+",
                  'NKT_cells'="CD3+,CD56+", 'Plasmablasts'="CD3-,CD20-,CD27+,CD38+", 'T_cells'="CD3+",
                  'Transitional_B_cells'="CD3-,CD19+,CD20+,CD24+,CD38+", 'Tregs'="CD3+,CD4+,CD25+,CD127- or CD3+,CD4+,CD25+,FOXP3+"
                )

temp= quote({
    kellyData <- reactive({
        
            # Get dataset from kelly_data dictionary
            data_name = input$dataType
            if( input$processing == F && input$dataType %in% c('CyTOF: PBMC','ELISA','Flow Cytometry: PBMC','Microarray: PBMC','Microarray: Whole Blood','HAI Titer','Multiplex ELISA','Blood Count','Virus Neutralization Titer') ){
                data_name = paste(data_name, "RAW" )
            }
            data = kelly_data[[data_name]]
            
            # Format data for graphing
            data = data[ data$age > min(input$ageRange) & data$age < max(input$ageRange) & data$gender %in% input$gender & data$race %in% input$race ,]
            data= data[ !is.na( data[,analyte()] ), ]
            
            # Remove outliers
            if( input$outliers == F ){
                vals= data[,analyte()]
                data= data[ abs(vals- mean(vals)) < 2*sd(vals) , ]
            }
        
            kelly_data = data.frame( Age=data[,'age'], Sex=as.factor(data[,'gender']), Value=round(data[,analyte()],2), Study=factor(data[,'study_accession']), Race=data[,'race']  )  
            kelly_data   
    })
})
body(server) = callConcat( body(server), temp )


temp= quote({
    kellyPlot <- reactive({
        
            to_graph = kellyData()
            
            # Decide if graph on log scale
            scaleChoice = 'identity'
            if( input$dataType %in% c('Microarray: Whole Blood','Microarray: PBMC','Multiplex ELISA','RNAseq: T cells') ){
                scaleChoice = 'log10'
            } 
        
            # If cytof dataset, display analyte label
            if( input$dataType == 'CyTOF: PBMC' ){
                output$cytof_label= renderText({ paste("* Defined by", cyto_labels[[analyte()]] ) })  #"*", analyte(),
            }
            
            # Plot Output depending on selected "plot by"
            if( input$colorCode == 'Age & Sex' ){
                p <- ggplot(data=to_graph, aes(x=Age, y=Value, color=Sex, race=Race, study=Study ) ) + geom_point(alpha=.6) + theme_gdocs() +  
                        labs(x = 'Subject Ages', y = ylab(), title = title() )  + scale_y_continuous(trans=scaleChoice) +
                        stat_smooth(  mapping=aes(x=Age, y=Value, color=Sex), inherit.aes=FALSE, se=FALSE )
                #p + stat_smooth( data=to_graph, mapping=aes(x=Age, y=Value, color=Sex), se=FALSE ) #stat_smooth(method=loess, data = to_graph[,c("Age","Value")] )  
            
            }else if( input$colorCode == 'Study' ){
                p <- ggplot(data= to_graph, aes(x=Age, y=Value, sex=Sex, race=Race, color=Study) ) + geom_point(alpha=.6) + theme_gdocs() +
                          labs(x = 'Subject Ages', y = ylab(), title = title() ) +  scale_y_continuous(trans=scaleChoice) +
                          stat_smooth(  mapping=aes(x=Age, y=Value, color=Study), inherit.aes=FALSE, se=FALSE )  
                #p +  scale_y_continuous(trans=scaleChoice) + stat_smooth(method=loess,data = dataBind(),aes(fill = factor(dataBind()[,'study_accession'])))+
            
            }else{ # Plot by Ethnicity
                par(las=1, bty='l', lwd=2, family  = 'sans',cex.axis=1.25, cex.lab=1.25, cex.main=1.75)
                p <- ggplot( data= to_graph, aes(x=Race, y= Value, fill=Race, sex=Sex, study=Study) ) +
                        geom_jitter( width=0.15, alpha=0.8, stroke=0, size=2 )  + #height = 0, opacity=.5
                        geom_violin( inherit.aes=FALSE, mapping=aes(x=Race, y= Value, fill=Race), trim= FALSE, alpha=0.5, show.legend=FALSE ) + #   inherit.aes=T, colour=NA,
                        #geom_jitter( data=to_graph, aes(x=Race, y= Value, fill=Race, sex=Sex, study=Study), width=0.15, alpha=0.8, stroke=0, size=2 )  + #height = 0, opacity=.5
                        labs(x = 'Race', y = ylab(), title = title() ) + 
                        theme_gdocs() 
                ggplotly(p, tooltip = c("Value", "density", "Sex", "Study") )
            }
       
    })
})
body(server) = callConcat( body(server), temp )




temp= quote({
    zichengData <- reactive({
        
        data.frame( Cell= gsub(" CD3.*","",gene_Tcell_raw[,'CellType']) , Value= gene_Tcell_raw[,analyte()] )
    })
})
body(server) = callConcat( body(server), temp )

temp= quote({
    zichengPlot <- reactive({
    
        # Create dataset
        to_graph= zichengData()

        # Create Plot
        par(las=1, bty='l', lwd=2, family  = 'sans', cex.axis=1.25, cex.lab=1.25, cex.main=1.75) # Some basic style formating
        p <- ggplot(data= to_graph, aes(x=Cell, y =Value, fill=Cell) )  + 
                geom_jitter(width = 0.15, alpha = 0.75 , stroke = .3, size=2 )  + #height = 0, opacity=.5
                geom_violin( trim= FALSE, alpha = 0.5, inherit.aes = T, show.legend=FALSE ) + # colour=NA,
                labs( y=paste(analyte(),' Expression'), title=paste( analyte()," by Cell") )+ #paste(analyte(), '(Expression)')
                theme_gdocs() 
       ggplotly(p, tooltip = c("Value", "density") ) # , "Sex", "Subject"
    })
})
body(server) = callConcat( body(server), temp )

#head(pbmc_subjects)

#pbmc_counts[1,1:10]

temp= quote({
    elliottData <- reactive({

        # Create Whole Blood RNA Data
        if( input$dataType == 'RNAseq: Whole Blood'  ){
            num = which(whole_blood_counts$hgnc==analyte() )  #  gambia_rna$hgnc
            gene_name = whole_blood_counts[num,2]
            to_graph = whole_blood_subjects
            a_row = whole_blood_counts[num,3:dim(whole_blood_counts)[2]]
            to_graph$Expression = as.numeric(a_row)
        }
        
        # Create PBMC RNA Data
        else if(  input$dataType == 'RNAseq: PBMC'  ){
            num = which(pbmc_counts$hgnc==analyte() )
            gene_name = pbmc_counts[num,1]
            to_graph = pbmc_subjects
            a_row = pbmc_counts[num,2:dim(pbmc_counts)[2]]
            to_graph$Expression = as.numeric(a_row)
        }
        
        # Create Mass Spec Data
        #else if( input$dataType == "Mass Spectrometry: Newborns" ){  
        else{
            col_num = which( colnames( guinea_mass_spec ) == analyte() )[1]  # <- get column number of analyte
            to_graph= data.frame( Expression=  round(guinea_mass_spec[,col_num],3) , Group= as.factor(  guinea_mass_spec$DAY ), Sex=guinea_mass_spec$SEX, Vaccination=guinea_mass_spec$TREATMENT  ) 
            to_graph$Day =  guinea_mass_spec$DAY 
            to_graph$DayPlot =  guinea_mass_spec$DAY + rnorm( dim(guinea_mass_spec)[1] , mean = 0, sd = .15 ) 
            to_graph$Subject =  guinea_mass_spec$SUBJECT
        }
        
        to_graph # <- return dataset
    })
})
body(server) = callConcat( body(server), temp )

temp= quote({
    elliottPlot <- reactive({
        
        to_graph = elliottData()  # <- Get dataset
        
    # For when the data is NOT RNAseq    
    if( input$dataType != 'RNAseq: Whole Blood' & input$dataType != 'RNAseq: PBMC' ){
        # Decide how to plot data based on "newbornPlotBy" input value
        main_plot = switch( input$newbornPlotBy ,
             'Age' = ggplot( to_graph, aes(x=DayPlot, day=Day, y=Expression, color=Group, sex=Sex, subject=Subject ) ), #status=Vaccination,
             'Sex'= ggplot( to_graph, aes(x=DayPlot, day=Day, y=Expression, color=Sex, status=Vaccination, subject=Subject ) ) #,
              #'Vaccination' = ggplot( to_graph, aes(x=DayPlot, day=Day, y=Expression, color=Vaccination, sex=Sex, subject=Subject ) ) 
             ) 
        
        my_ylab = switch( input$dataType ,
              'RNAseq: Newborns' = "Gene Expression (Transcripts per Million)",
              "Mass Spectrometry: Newborns" = "Molecule Expression",
              "Phenotype Expression"
              )
     
        p = main_plot + geom_point(alpha=.7) + theme_gdocs() + labs(x='Days Alive', y=my_ylab, title=paste( analyte()," Expression in Newborns") ) 
        ggplotly( p , tooltip = c("Day", "Expression", "Subject", "Sex" ) )  #, "Vaccination"
    
    }else{ # For when it is RNAseq Data
        
        # Change code so that whole blood study SDY1092 ages are displayed as NA
        to_graph$age = to_graph$Age
        if( input$dataType == 'RNAseq: Whole Blood' ){
            to_graph$Age = round(to_graph$Age, digits=1)
            to_graph$Expression = round(to_graph$Expression, digits=1)
            to_graph$Age[ to_graph$Study=="SDY1092" ] = NA
        }
        
        main_plot = switch( input$newbornPlotBy ,
             'Age' = ggplot( to_graph, aes(x=age, y=Expression, color=Study, sex=Sex, subject=Subject, Age=Age ) ), #status=Vaccination,
             'Sex' = ggplot( to_graph, aes(x=age, y=Expression, color=Sex, study=Study, subject=Subject, Age=Age ) ) #,
              #'Vaccination' = ggplot( to_graph, aes(x=DayPlot, day=Day, y=Expression, color=Vaccination, sex=Sex, subject=Subject ) ) 
             ) 
        
        my_ylab= "Transcripts per Million"
        
        p = main_plot + geom_point(alpha=.7) + theme_gdocs() + labs(x='Age', y=my_ylab, title=paste( analyte()," Expression") )
        ggplotly( p , tooltip = c("Age", "Expression","Study","Sex", "Subject"  ) )  #, "Vaccination"
    }
        
    })
})
body(server) = callConcat( body(server), temp )


temp= quote({
    
    plotInput <- reactive({
        if( input$dataType %in% names(kelly_data) ){ 
            p <- kellyPlot()
            my_data = kellyData()
        }else if( input$dataType == 'RNAseq: T cells'  ){
            p <- zichengPlot()
            my_data = zichengData()
        }else{ # these plots are: "RNAseq: Newborns" "Cibersort: Whole Blood" "Mass Spectrometry: Newborns" 
            p <- elliottPlot()
            my_data = elliottData()
        }        
        output$num_subjects = renderText({  paste( dim(my_data)[1]," Subjects" )  })
        p
    }) #<-- plotInput end    
    output$dataPlot <- renderPlotly({ plotInput() })
    
})  #<-- Quote end
body(server) = callConcat( body(server), temp )


temp= quote({
  #Handle 'Download Plot' button on Visualize page
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste('10KImmunomes', input$dataType, analyte(), '.pdf', sep='_')
      #paste('10KImmunomes', input$dataType, '.pdf', sep='_')  
    },
    content = function(file) {
      pdf(file, width = 8, height = 5)
      print(plotInput())
      dev.off()
    }
  )
})  #<-- Quote end
body(server) = callConcat( body(server), temp )

temp= quote({
  output$downloadPlotData <- downloadHandler(
    # Write File Name  
    filename = function() { paste('10KImmunomes', input$dataType, analyte(), '.csv', sep='_') },  # Sys.Date()  <-- can add if u like 
    
    # Create File
    content = function(file) {
        # Get Dataset for file
        if( input$dataType %in% names(kelly_data) ){ 
            dataPlot <- kellyData()
        }else if( input$dataType == 'RNAseq: T cells'  ){
            dataPlot <- zichengData()
        }else{ # these plots are: "RNAseq: Newborns" "Cibersort: Whole Blood" "Mass Spectrometry: Newborns" 
            dataPlot <- elliottData()
        }       
        write.csv( dataPlot, file, row.names = F) # Write File         
        
      # Get Dataset for file
      #dataPlot= switch( input$dataType,
      #                  'RNAseq: T cells'  =  cbind( data()[,2:3] , data()[, analyte()] ) ,
      #                  "RNAseq: Newborns"   =  data()[ which(data()$hgnc==analyte()) , ]   ,
      #                  "Mass Spectrometry: Newborns"=  cbind( data()[,4:8] , data()[, analyte()] ) ,
      #                  "Cibersort: Whole Blood"=  cbind( data()[,4:8] , data()[, analyte()] ) ,
      #                   cbind( data()[,2:5] , data()[, analyte()] ) # <-- All Other Data                       
      #                )        

    }
  )
})  #<-- Quote end
body(server) = callConcat( body(server), temp )

temp= quote({
    observeEvent( input$dataType , {
        # Get the path to where the data will be downloaded from
        file_path = switch(input$dataType,
              "CyTOF: PBMC" = "data_raw/CyTOF_PBMC.zip", 
              "ELISA" = "data_raw/ELISA.zip",
              "Flow Cytometry: PBMC" = "data_raw/Flow_Cytometry_PBMC.zip",
              "Flow Cytometry: Whole Blood" = "data_raw/Flow_Cytometry_Whole_Blood.zip",
              "Microarray: Whole Blood" = "data_raw/Gene_Expression_Whole_Blood_formatted.zip",
              "Microarray: PBMC" = "data_raw/Gene_Expression_PBMC.zip",  #"Gene Set Enrichment: Whole Blood" = input$gsea_analyte,
              "HAI Titer" = "data_raw/HAI_Titer.zip",
              "HLA Type" = "data_raw/hla.csv",
              "Multiplex ELISA" = "data_raw/Multiplex_ELISA.zip",
              "Blood Count" = "data_raw/Lab_Tests_Blood_Count.zip",
              "Lipid Profile" = "data_raw/lab_test_fasting_lipid_profile.csv",
              "Metabolic Panel" = "data_raw/lab_test_comprehensive_metabolic_panel.csv",
              "Virus Neutralization Titer" = "data_raw/Virus_Neutralization_Titer.zip",
              "RNAseq: T cells" = "data_raw/gene_Tcells_formatted.csv",
              "RNAseq: Whole Blood" = "data/whole_blood_rna_tpm.csv",
              "RNAseq: PBMC" = "data/pbmc_rna_counts.csv",         
              "Mass Spectrometry: Newborns" = "data/png_metabolomics.725331.csv",
        )
        # Change the download button's href to the download path
        html_pre = "<a  href='"
        html_post= "' download ><button class='btn btn-danger'><i class='fa fa-cart-arrow-down'></i> All Data</button></a>" 
        html( id='downloadAllData', html=paste( html_pre, file_path, html_post, sep="" )  )
        
    }) #<-- observeEvent end
})  #<-- Quote end
body(server) = callConcat( body(server), temp )


#options(shiny.port = 8888)
#options(shiny.host = "0.0.0.0")

shinyApp(ui = fluidPage( useShinyjs(), style='margin-left:5px; margin-right:5px', ui  ), server = server)


