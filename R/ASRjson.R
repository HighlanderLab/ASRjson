#'@importFrom rjson fromJSON

#' @description
#' Automated script writing using a GUI. The GUI outputs
#' a json file that gets used to construct an AlphaSimR
#' simulation script.
#'
#' @keywords internal
"_PACKAGE"


#' @title Convert json AlphaSimR script
#'
#' @description Reads in a file in json format and uses
#' the information to create an AlphaSimR script.
#'
#' @param json path to input json file
#' @param output name of output file
#'
#' @examples
#' \dontrun{
#' json = system.file("extdata", "example.json",
#'                    package = "ASRjson")
#' json2script(json, "example.R")
#' }
#'
#'
#' @export
json2script = function(json, output){

  # Convert json to an R object
  rjson = fromJSON(file=json)

  # Hard coded values
  # These can be removed as hard coded values by adding input to the GUI or this function
  nRep = 10 # Number of replications
  nYears = 20 # Number of years for modeling breeding program

  # Copy data from edges and nodes for easier access
  Nodes = rjson$Nodes
  nodeType = sapply(Nodes, function(x) x$Type)
  nodeName = sapply(Nodes, function(x) x$id)
  Edges = rjson$Edges
  inNode = sapply(Edges, function(x) x$from)
  outNode = sapply(Edges, function(x) x$to)

  # Check for the presence of exactly one crossing block
  # The crossing block will be considered the start of the breeding program.
  # It will be initialized with the founder haplotypes, which will in
  # turn be used to fill in all other populations (Nodes) in the breeding program.
  isCB = nodeType=="Crossing block"
  if(sum(isCB)!=1){
    stop("Invalid simulation, there must be exactly one 'Crossing block'")
  }

  # Check that all nodes only have one entry point
  # This requirement serves a couple of purposes:
  # 1) It prevents loops in the graph that can't be resolved.
  # 2) It prevents ambiguity in the timing of the setPheno call.
  # The crossing block is exempt from this requirement, because
  # it will not use setPheno and it must be allowed to form a loop
  # to complete the breeding program.
  if(
    any(
      duplicated(
        inNode[ inNode!=nodeName[isCB] ]
        )
      )
    ){
    stop("Invalid simulation, only the 'Crossing block' can accept individuals from more than one population")
  }

  # Sort edges and nodes
  # Begin at the crossing block and work outwards

  ## Start writing AlphaSimR script
  script = c("library(AlphaSimR)",
             "")

  # Create objects for saving output

  #### TO DO ####

  # Add loop for replications
  script = c(script,
             "for(REP in 1:", nRep, "){","")

  # Determine total number of sites needed
  nSites = sapply(rjson$Traits, function(x) x$QTL)
  nSites = max(as.numeric(nSites))

  # Create founder haplotypes
  # Using quickHaplo for speed. This could be converted to runMacs2
  # to offer the ability to vary Ne in the parents.
  script = c(script,
             paste0("founderPop = quickHaplo(",
                    "nInd=",
                    rjson$Nodes[[which(isCB)]]["Number of Individuals"],
                    ", nChr=",
                    rjson$Genome$Chromosomes,
                    ", segSites=",
                    nSites,
                    ", genLen=",
                    rjson$Genome$`Genetic Length`,
                    ")"),
             "")

  # Set simulation parameters
  script = c(script,
             "SP = SimParam$new(founderPop)")

  # Create placeholder for varE that will grow when looping through traits
  varE = character()

  # Write script for adding traits by looping through traits one at a time
  # We are modeling traits as always having all types of effects
  # This incurs a performance penalty when the effects aren't need, but greatly
  # simplifies the code.
  for(jsonTrait in rjson$Traits){
    # Add addTrait code to script
    script = c(script,
               paste0("SP$addTraitADEG(",
                      jsonTrait$QTL, ", ",
                      "mean=", jsonTrait$Mean, ", ",
                      "var=", jsonTrait$`Genetic Variance`, ", ",
                      "varGxE=", jsonTrait$`GxE Variance`, ", ",
                      "meanDD=", jsonTrait$`Mean Degree`, ", ",
                      "varDD=", jsonTrait$`Variance Degree`, ", ",
                      "relAA=", jsonTrait$`Relative value`, ", ",
                      "useVarA=FALSE)")
    )

    # Track varE
    varE = c(varE, jsonTrait$`Error Variance`)
  }

  # Add varE to simulation parameters
  script = c(script,
             "SP$setVarE(varE=c(",
             paste(varE,collapse=","),
             "))")

  # Sort breeding stages

  #### TO DO ####

  # Initialize breeding program

  #### To DO ####

  # Cycle through years
  script = c(script,
             paste0("for(YEAR in 1:", nYears, "){"))

  #### To DO ####

  # End year loop
  script = c(script, "}")

  # End replication loop
  script = c(script, "}")

  # Write out script
  writeLines(script, output)
}

