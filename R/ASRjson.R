#' @importFrom AlphaSimR SimParam quickHaplo runMacs newPop
#' @importFrom AlphaSimR setPheno
#' @importFrom AlphaSimR selectInd selectWithinFam selectFam
#' @importFrom AlphaSimR self
#' @importFrom rjson fromJSON

#' @description
#' Functions for automating simulation runs designed in a GUI.
#' The GUI outputs a json file that the functions in this library
#' will use to run an AlphaSimR simulation.
#'
#' @keywords internal
"_PACKAGE"

# Check validity of simulation design
checkSim = function(rjson){
  # Check for loops
  # Only crossing block is allowed to have loops
}

# Build list of populations
buildPopList = function(rjson){
  # Length equal to nodes
}

# Build list of phenotyping functions
buildPhenoList = function(rjson){
  # Length equal to nodes
}

# Build list of selection functions
buildSelctionList = function(rjson){
  # Length equal to edges
}


# Build the list structure for the simulation
buildSkeleton = function(rjson){
  output = list()
  output$pops = buildPopList(rjson)
  output$pheno = buildPhenoList(rjson)
  output$selection = buildSelctionList(rjson)
  return(output)
}


#' @title Run AlphaSimR simulations from json
#'
#' @description Reads in a file in json format and uses
#' the information to run an AlphaSimR simulation. Output from
#' the simulation is saved as an R data file.
#'
#' @param json path to input json file
#' @param output name of output file
#'
#' @export
ASRjson = function(json, output){
  # Convert json to an R object
  rjson = fromJSON(json)

  # Check validity of simulation
  checkSim(rjson)

  # Build simulation skeleton
  sim = buildSkeleton(rjson)

  # Still working
}

#
# # Structure
# popList[["B"]] = setPheno(popList[["A"]], varE = VARE, reps = REPS)
#
#
# addItems = function(X, Y){
#   command = paste0("numList[['", Y,"']] = numList[['",X,"']] + 1")
#   return(str2expression(command))
# }
#
# numList = list(A=1, B=0)
# actionList = addItems("A", "B")
# numList
# eval(actionList)
# numList





