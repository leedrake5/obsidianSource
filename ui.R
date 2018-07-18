library(DT)
library(dplyr)
library(shinythemes)
library(shiny)
library(rhandsontable)



shinyUI(navbarPage("Obsidian Source", id="nav", theme = shinytheme("simplex"),
tabPanel("Spectrum",
div(class="outer",
sidebarLayout(
sidebarPanel(



actionButton("actionprocess", label = "Process Data"),
actionButton("actionplot", label = "Plot Spectrum"),
downloadButton('downloadPlot', "Plot"),


tags$hr(),

uiOutput('filegrab'),


selectInput("filetype", label=NULL, c("PDZ", "CSV", "Net", "Spreadsheet"), selected="PDZ"),

tags$hr(),

textInput('projectname', label="Project Name", value="ObsidianSourcing"),

tags$hr(),


element <- selectInput(
"element", "Element:",
c("(Ne) Neon" = "Ne.table",
"(Na) Sodium" = "Na.table",
"(Mg) Magnesium" = "Mg.table",
"(Al) Aluminum" = "Al.table",
"(Si) Silicon" = "Si.table",
"(P)  Phosphorous" = "P.table",
"(S)  Sulfur" = "S.table",
"(Cl) Chlorine" = "Cl.table",
"(Ar) Argon" = "Ar.table",
"(K)  Potassium" = "K.table",
"(Ca) Calcium" = "Ca.table",
"(Sc) Scandium" = "Sc.table",
"(Ti) Titanium" = "Ti.table",
"(V)  Vanadium" = "V.table",
"(Cr) Chromium" = "Cr.table",
"(Mn) Manganese" = "Mn.table",
"(Fe) Iron" = "Fe.table",
"(Co) Cobalt" = "Co.table",
"(Ni) Nickel" = "Ni.table",
"(Cu) Copper" = "Cu.table",
"(Zn) Zinc"= "Zn.table",
"(Ga) Gallium" = "Ga.table",
"(Ge) Germanium" = "Ge.table",
"(As) Arsenic" = "As.table",
"(Se) Selenium" = "Se.table",
"(Br) Bromium" = "Br.table",
"(Kr) Krypton" = "Kr.table",
"(Rb) Rubidium" = "Rb.table",
"(Sr) Strontium" = "Sr.table",
"(Y)  Yttrium" = "Y.table",
"(Zr) Zirconium" = "Zr.table",
"(Nb) Niobium" = "Nb.table",
"(Mo) Molybdenum" = "Mo.table",
"(Tc) Technicium" = "Tc.table",
"(Ru) Ruthenium" = "Ru.table",
"(Rh) Rhodium" = "Rh.table",
"(Pd) Paladium" = "Pd.table",
"(Ag) Silver" = "Ag.table",
"(Cd) Cadmium" = "Cd.table",
"(In) Indium" = "In.table",
"(Sn) Tin" = "Sn.table",
"(Sb) Antimony" = "Sb.table",
"(Te) Tellerium" = "Te.table",
"(I) Iodine" = "I.table",
"(Xe) Xenon" = "Xe.table",
"(Cs) Cesium" = "Cs.table",
"(Bs) Barium" = "Ba.table",
"(Ce) Cerium" = "Ce.table",
"(Pr) Praeseodymeum" = "Pr.table",
"(Nd) Neodymeum" = "Nd.table",
"(Pr) Promethium" = "Pr.table",
"(Sm) Samarium" = "Sm.table",
"(Eu) Europium" = "Eu.table",
"(Gd) Gadolinium" = "Gd.table",
"(Tb) Terbium" = "Tb.table",
"(Dy) Dysprosium" = "Dy.table",
"(Ho) Holmium" = "Ho.table",
"(Er) Erbium" = "Er.table",
"(Tm) Thullium" = "Tm.table",
"(Yb) Ytterbium" = "Yb.table",
"(Lu) Lutetium" = "Lu.table",
"(Hf) Halfnium" = "Hf.table",
"(Ta) Tantalum" = "Ta.table",
"(W)  Tungsten" = "W.table",
"(Re) Rhenium" = "Re.table",
"(Os) Osmium" = "Os.table",
"(Ir) Irridium" = "Ir.table",
"(Pt) Platinum" = "Pt.table",
"(Au) Gold" = "Au.table",
"(Hg) Mercury" = "Hg.table",
"(Tl) Thallium" = "Tl.table",
"(Pb) Lead" = "Pb.table",
"(Bi) Bismuth" = "Bi.table",
"(Po) Polonium" = "Po.table",
"(At) Astatine" = "At.table",
"(Rn) Radon" = "Rn.table",
"(Fr) Francium" = "Fr.table",
"(Ra) Radium" = "Ra.table",
"(Ac) Actinum" = "Ac.table",
"(Th) Thorium" = "Th.table",
"(Pa) Proactinum" = "Pa.table",
"(U)  Uranium" = "U.table"),
selected="Fe.table"),


#checkboxInput('backgroundsubtract', "Background Subtract"),



tags$hr(),

fileInput('calfileinput1', 'Load Cal File', accept='.quant', multiple=FALSE),

checkboxInput('usecalfile', "Use Cal File"),




tags$hr(),

checkboxInput('otherdata', "Import Other Data", value=FALSE),

uiOutput('file2gen'),
uiOutput('calfile2gen'),
uiOutput('space23gen'),
uiOutput('file3gen'),
uiOutput('calfile3gen')



),




mainPanel(
fluidRow(
column(width = 11, class = "well",
plotOutput("distPlot", height = 650,
dblclick = "plot1_dblclick",
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
)))))
))
),

tabPanel("Counts",
div(class="outer",

fluidRow(
sidebarLayout(
sidebarPanel(



actionButton('hotableprocess', "Enter Values"),
downloadButton('downloadData', "Table"),

tags$hr(),

checkboxInput('asppm', "Convert to ppm", value=FALSE),

tags$hr(),

conditionalPanel(
condition='input.dataset === spectra.line.table',
uiOutput('defaultlines')

)),




mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('All Data', dataTableOutput('mytable1')),
tabPanel('Covariance', plotOutput('covarianceplot')),
tabPanel('Add Categories', rHandsontableOutput('hot'))

))
)

)



)),

tabPanel("Model Prep",
fluidRow(
sidebarLayout(

sidebarPanel(
downloadButton("downloadsubsetfinal", label = "Download"),
selectInput('obsidiandatabase', "Select Database", choices=c("All Subsources", "All Source Groups", "Qualified Subsources", "Qualified Source Groups"), selected="All Source Groups"),
selectInput('regionselect', "Define Region", choices=c("Lat/Long", "Continent", "Region", "Political"), selected="Political"),
uiOutput('minlat'),
uiOutput('maxlat'),
uiOutput('minlong'),
uiOutput('maxlong'),
uiOutput('choosecountry'),
uiOutput('choosecontinent'),
uiOutput('chooseregion'),
uiOutput('choosestate'),
uiOutput('source.number'),


tags$hr(),

numericInput('sensitivity', "Model Sensitivity", min=0.01, max=0.99, step=0.01, value=0.15),
checkboxInput('bayesian', "Use Lat/Long as Prior", value=FALSE),
checkboxInput('constraindata', "Limit to Complete Source Data", value=TRUE),

tags$hr(),



uiOutput('clipsubsetfinal')
),

mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('Covariance', plotOutput('covarianceplotsources'),
splitLayout(cellWidths=c("10%", "10%", "10%", "10%", "10%", "10%"),
uiOutput('qualSelect1a'),
uiOutput('qualSelect2a'),
uiOutput('qualSelect3a'),
uiOutput('qualSelect4a')
)
),
tabPanel('Data', dataTableOutput('mydatamerge2')),
tabPanel('Source Data', dataTableOutput('source_frame'))
)
)))),



tabPanel("Results",
fluidRow(
sidebarLayout(

sidebarPanel(
downloadButton('sourcelistresults', "Excel"),
downloadButton('sourceartifacts', "CSV"),
downloadButton('sourcelistresultsraw', "Raw"),
downloadButton('downloadsourcemap', "Map"),

tags$hr(),

checkboxInput('adjustcoordinates', "Adjust Map", value=FALSE),
uiOutput('minlatmap'),
uiOutput('maxlatmap'),
uiOutput('minlongmap'),
uiOutput('maxlongmap'),
tags$hr()


),

mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('Summary Table', dataTableOutput('summarytable')),
tabPanel('Sourced Artifacts', dataTableOutput('sourcedTable')),
tabPanel('Source Map', plotOutput('sourcedMap'))
))))),

tabPanel("PCA",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(
uiOutput('knumui'),

selectInput("pcacolour", "Colour", choices=c(
"Black"="Black",
"Source"="Source",
"Cluster"="Cluster",
"Focus" = "Focus",
"Qualitative1"="Qualitative1",
"Qualitative2"="Qualitative2",
"Qualitative3"="Qualitative3",
"Qualitative4"="Qualitative4",
"Quantitative"="Quantitative"),
selected="Source"),

uiOutput('pcaFocusVariable'),
uiOutput('pcaFocusUI'),
uiOutput('pcaFocusLabel'),


sliderInput("spotsize", label = "Point Size", value=2, min=2, max=15),

uiOutput('pcaellipsesources'),

checkboxInput('elipseplot1', "Elipse", value=TRUE),
checkboxInput('logtrans', "Log Transform"),


#uiOutput('inxlimrangepca'),
#uiOutput('inylimrangepca'),


tags$hr(),


downloadButton('downloadPlot2', "Plot"),
downloadButton('xrfpcatablefulldownload', "Results")

),



mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('PCA Plot',

# this is an extra div used ONLY to create positioned ancestor for tooltip
# we don't change its position
div(
style = "position:relative",
plotOutput("xrfpcaplot", height = 650,
hover = hoverOpts("plot_hoverpca", delay = 100, delayType = "debounce")),
uiOutput("hover_infopca")
)


),

tabPanel("Optimal Clusters",
div(
style = "position:relative",
plotOutput('optimalkplot',
hover = hoverOpts("plot_hoveroptimalk", delay = 100, delayType = "debounce")),
uiOutput("hover_infooptimalk"))
),

tabPanel("Table", DT::dataTableOutput('xrfpcatable'))

)))))),

tabPanel("Elemental Ratios",
div(class="outer",

fluidRow(
sidebarLayout(

sidebarPanel(

selectInput(
"ratiocolour", "Ratio Plot Type",
c(
"Black" = "Black",
"Source"="Source",
"Cluster" = "Cluster",
"Focus" = "Focus",
"Qualitative1"="Qualitative1",
"Qualitative2"="Qualitative2",
"Qualitative3"="Qualitative3",
"Qualitative4"="Qualitative4",
"Quantitative" = "Quantitative"
), selected="Source"),

uiOutput('ratioFocusVariable'),
uiOutput('ratioFocusUI'),
uiOutput('ratioFocusLabel'),

tags$hr(),

uiOutput('inelementratioa'),
uiOutput('inelementratiob'),

uiOutput('inelementratioc'),
uiOutput('inelementratiod'),

tags$hr(),

sliderInput("spotsize2", label = "Point Size", value=2, min=2, max=15),
uiOutput('ratioellipsesources'),



checkboxInput('elipseplot2', "Elipse", value=TRUE),

tags$hr(),

downloadButton('downloadPlot4', "Plot")),

mainPanel(
div(
style = "position:relative",
plotOutput("elementratiotimeseries", height = 650,
hover = hoverOpts("plot_hoverratio", delay = 100, delayType = "debounce")),
uiOutput("hover_inforatio")
)))

))),


tabPanel("Ternary Diagram",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(


selectInput("ternarycolour", "Colour", choices=c(
"Black"="black",
"Source"="Source",
"Cluster"="Cluster",
"Qualitative1"="Qualitative1",
"Qualitative2"="Qualitative2",
"Qualitative3"="Qualitative3",
"Qualitative4"="Qualitative4",
"Quantitative" = "Quantitative"),
selected="Source"),


tags$hr(),


uiOutput('inaxisa'),
uiOutput('inaxisb'),
uiOutput('inaxisc'),
checkboxInput('terndensityplot', "Density Contour"),
checkboxInput('ternnormplot', "Normalize"),


tags$hr(),

sliderInput("ternpointsize", label = "Point Size", value=2, min=2, max=15),


tags$hr(),

downloadButton('downloadPlot5', "Plot")

),

mainPanel(
tabPanel('Ternary Plot', plotOutput('ternaryplot',
dblclick = "plot1_dblclick", height = 800,
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
))))


))

))



)


)










