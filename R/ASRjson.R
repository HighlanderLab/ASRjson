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
  # TO DO

  # Add loop for replications
  # Hard coded to 10 reps at the moment
  script = c(script,
             "for(REP in 1:10){","")

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

  # Initialize breeding program

  # Cycle through years

  # Still working
  #writeLines(script, output)
}

