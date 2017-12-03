# ------------------------------------------------------------------------------
#
#    Author         J.M. Lietaer
#
#    Date
#       written     01-11-2017
#       modified    29-11-2017
#
#    Language       R
#
# ------------------------------------------------------------------------------
#
#    Package        jmlR.toolbox
#
# ------------------------------------------------------------------------------


#
#  START OF PACKAGE
#

#' Function write.xgml()
#'
#' This function produces a XGML file based on user input (file or data-variable).
#' @param orig Origin of the input, "F"=file, "D"=data. Default: <empty>
#' @param rel Type of relation. "N"=numeric, "C"=character. Default: "N"
#' @param sep Separator character. Default: ";"
#' @param directed TRUE or FALSE - Directed graph. Default: "TRUE"
#' @param data_in  <name_of_input_variable> | <fully_qualified_file_name_of_input_file>
#' @param data_out <fully_qualified_file_name_of_output_file>
#' @param applic <fully_qualified_file_name_of_application_to_open_data_out>
#' @param header TRUE or FALSE - Input had a header. Default: "TRUE"
#' @param fill_color TRUE or FALSE - Input contains fill color (format #FFFFFF). Default: "TRUE"
#' @return Returns a vector of character strings representing the result of the operation.
#' @author Jean Marc Lietaer, \email{jmlietaer.tech@@gmail.com}
# #' @seealso
#' @keywords utilities, graph, social network analysis
#' @examples
#' # rc <- jmlR.toolbox::write.xgml(orig = "F", rel = "C", sep = ";", directed=FALSE, data_in = "input.csv",
#' # header = TRUE, fill_color = F, data_out =  paste(getwd(), "/", "output.xgml", sep = ""),
#' # applic = "/home/jmlietaer/yEd/yEd")
#' @importFrom utils read.csv
#' @importFrom utils write.table
#' @importFrom stringr str_length
#' @importFrom stringr str_sub
#' @export
write.xgml <- function (orig, rel="N", directed=TRUE, sep=";", data_in, header=T, fill_color=T, data_out="diagram.xgml", applic="no"){
# is.letter <- function(x) grepl("[[:alpha:]]", x)
  is.number <- function(x) grepl("[[:digit:]]", x)
  if (orig == "F") {
    data_v <- read.csv(data_in, header=header, stringsAsFactors=F, skip=0, sep=sep)
  }
  else {
    if (orig == "D") {
      data_v <- data_in
    }
    else {
      return(-1)
    } # end-if
  } # end-if
  # ADD XGML HEADER
  xgml.header <- '<?xml version="1.0" encoding="ISO-8859-1"?>'
  xgml.header <- c(xgml.header, '<section name="xgml">')
  xgml.header <- c(xgml.header,'<attribute key="Creator" type="String">jmlRpol</attribute>', '<attribute key="Version" type="String">0.1</attribute>')
  xgml.header <- c(xgml.header, '<section name="graph">')
  xgml.header <- c(xgml.header, '<attribute key="hierarchic" type="int">1</attribute>')
  xgml.header <- c(xgml.header, '<attribute key="label" type="String"></attribute>')
  xgml.header <- c(xgml.header, '<attribute key="directed" type="int">1</attribute>')
  # INITIALIZE VARIABLES
  xgml.node <- ""
  xgml.edge <- ""
  key.base <- 1000000000
  # CREATE NODE LIST
  nodes <- c(unique(data_v[,1]), unique(data_v[,2]))
  nodes_save <- nodes
  nodes_temp <- unique(nodes_save)
  if (fill_color) {
    nodes <- cbind(nodes_temp, fill=0)
    l <- dim(nodes)
    for(i in 1:l[1]){
      nodes[i,2] <- stringr::str_sub(nodes[i,1], start=-7)
    }
  }
  # CREATE NODES
  for(i in 1:l[1]) {
    xgml.node <- c(xgml.node, '<section name="node">')
    xgml.node <- c(xgml.node, paste('<attribute key="id" type="int">', key.base+i,'</attribute>', sep=""))
    xgml.node <- c(xgml.node, '<attribute key="label" type="String"></attribute>')
    xgml.node <- c(xgml.node, '<section name="graphics">')
    xgml.node <- c(xgml.node, '<attribute key="x" type="double">100.0</attribute>')
    xgml.node <- c(xgml.node, '<attribute key="y" type="double">100.0</attribute>')
    xgml.node <- c(xgml.node, '<attribute key="w" type="double">200.0</attribute>')
    xgml.node <- c(xgml.node, '<attribute key="h" type="double">40.0</attribute>')
    xgml.node <- c(xgml.node, '<attribute key="type" type="String">rectangle</attribute>')
    # if ((as.numeric(data_v[i,3]) < 0)) {
    #   s <- key.from
    #   t <- key.to
    # }
    # else {
    #   t <- key.from
    #   s <- key.to
    # } # end-if
    if (fill_color) {
      xgml.node <- c(xgml.node, paste('<attribute key="fill" type="String">', nodes[i,2], '</attribute>', sep=""))
    }
    xgml.node <- c(xgml.node, '<attribute key="outline" type="String">#000000</attribute>')
    xgml.node <- c(xgml.node, '</section>')
    xgml.node <- c(xgml.node, '<section name="LabelGraphics">')
    if (fill_color) {
      entity_label <- stringr::str_sub(nodes[i,1], 1, stringr::str_length(nodes[i,1])-9)
      xgml.node <- c(xgml.node, '<attribute key="text" type="String">', entity_label, '</attribute>')
    }
    else {
      xgml.node <- c(xgml.node, '<attribute key="text" type="String">', nodes[i], '</attribute>')
    }
    xgml.node <- c(xgml.node, '<attribute key="fontSize" type="int">12</attribute>')
    xgml.node <- c(xgml.node, '<attribute key="fontName" type="String">Dialog</attribute>')
    xgml.node <- c(xgml.node, '<attribute key="anchor" type="String">c</attribute>')
    xgml.node <- c(xgml.node, '</section>')
    xgml.node <- c(xgml.node, '</section>')
  } # end-for
  # CREATE EDGES
  for(i in 1:length(data_v[,1])) {
    key.from <- key.base + pmatch(data_v[i,1], nodes)
    key.to <- key.base + pmatch(data_v[i,2], nodes)
    xgml.edge <- c(xgml.edge, '<section name="edge">')
    if (rel == "N") {
      if (is.number(data_v[i,3])) {
        if ((as.numeric(data_v[i,3]) < 0)) {
          s <- key.from
          t <- key.to
        }
        else {
          t <- key.from
          s <- key.to
        } # end-if
      }
      else {
        s <- key.from
        t <- key.to
      } # end-if
    } # end-if
    if (rel == "C") {
      s <- key.from
      t <- key.to
    }
    xgml.edge <- c(xgml.edge, paste('<attribute key="source" type="int">', s,'</attribute>', sep=""))
    xgml.edge <- c(xgml.edge, paste('<attribute key="target" type="int">', t,'</attribute>', sep=""))
    xgml.edge <- c(xgml.edge, '<attribute key="label" type="String"></attribute>')
    xgml.edge <- c(xgml.edge, '<section name="graphics">')
    xgml.edge <- c(xgml.edge, '<attribute key="fill" type="String">#000000</attribute>')
    if (directed) {
      xgml.edge <- c(xgml.edge, '<attribute key="targetArrow" type="String">standard</attribute>')
    }
    xgml.edge <- c(xgml.edge, '</section>')
    xgml.edge <- c(xgml.edge, '<section name="LabelGraphics">')
    if ((is.number(data_v[i,3])) & (rel == "N")) {
      data_v[i,3]<- abs(as.numeric(data_v[i,3]))
    }
    xgml.edge <- c(xgml.edge, '<attribute key="text" type="String">', data_v[i,3], '</attribute>')
    xgml.edge <- c(xgml.edge, '<attribute key="fontSize" type="int">12</attribute>')
    xgml.edge <- c(xgml.edge, '<attribute key="fontName" type="String">Dialog</attribute>')
    xgml.edge <- c(xgml.edge, '<attribute key="configuration" type="String">AutoFlippingLabel</attribute>')
    xgml.edge <- c(xgml.edge, '<attribute key="contentWidth" type="double">40.703125</attribute>')
    xgml.edge <- c(xgml.edge, '<attribute key="contentHeight" type="double">18.701171875</attribute>')
    xgml.edge <- c(xgml.edge, '<attribute key="model" type="String">six_pos</attribute>')
    xgml.edge <- c(xgml.edge, '<attribute key="position" type="String">head</attribute>')
    xgml.edge <- c(xgml.edge, '</section>')
    xgml.edge <- c(xgml.edge, '</section>')
  } # end-for
  # CLEAN NODES/EDGES LIST
  xgml.node=xgml.node[-1]
  xgml.edge=xgml.edge[-1]
  # ADD XGML FOOTER
  xgml.footer <- c('</section>', '</section>')
  # ASSEMBLE XGML COMPONENTS
  xgml <- c(xgml.header, xgml.node, xgml.edge, xgml.footer)
  # WRITE XGML FILE
  write.table(xgml, data_out, col.names=F, row.names=F, quote=F)
  # OPEN XGML FILE WITH VIEWER
  if (applic != "no") {
    applic <- paste(shQuote(applic), " ", shQuote(data_out))
    shell.cmd <- applic
    system(shell.cmd, wait = F)
  }
  return(0)
}


# --------------------------------------------------------------------------------------------

#' Function write.graphml()
#'
#' This function produces a GraphML file based on user input (file or data-variable).
#' @param orig Origin of the input, "F"=file, "D"=data. Default: <empty>
#' @param rel Type of relation. "N"=numeric, "C"=character. Default: "N"
#' @param directed TRUE or FALSE - Directed graph. Default: "TRUE"
#' @param sep Separator character. Default: ";"
#' @param data_in  <name_of_input_variable> | <fully_qualified_file_name_of_input_file>
#' @param header TRUE or FALSE - Input had a header. Default: "TRUE"
#' @param fill_color TRUE or FALSE - Input contains fill color (format #FFFFFF). Default: "TRUE"
#' @param data_out <fully_qualified_file_name_of_output_file>
#' @param applic <fully_qualified_file_name_of_application_to_open_data_out>
#' @return Returns a vector of character strings representing the result of the operation.
#' @author Jean Marc Lietaer, \email{jmlietaer.tech@@gmail.com}
# #' @seealso
#' @keywords utilities, graph, social network analysis
#' @examples
#' # rc <- jmlR.toolbox::write.graphml(orig = "F", rel = "C", sep = ";", directed=FALSE, data_in = "input.csv",
#' # header = TRUE, fill_color = F, data_out =  paste(getwd(), "/", "output.graphml", sep = ""),
#' # applic = "/home/jmlietaer/yEd/yEd")
#' @importFrom utils read.csv
#' @importFrom utils write.table
#' @importFrom stringr str_length
#' @importFrom stringr str_sub
#' @export
write.graphml <- function (orig, rel="N", directed=TRUE, sep=";", data_in, header=T, fill_color=T, data_out="diagram.graphml", applic="no"){
  # is.letter <- function(x) grepl("[[:alpha:]]", x)
  is.number <- function(x) grepl("[[:digit:]]", x)
  if (orig == "F") {
    data_v <- read.csv(data_in, header=header, stringsAsFactors=F, skip=0, sep=sep)
  }
  else {
    if (orig == "D") {
      data_v <- data_in
    }
    else {
      return(-1)
    } # end-if
  } # end-if
  # ADD GRAPHML HEADER
  graphml.header <- '<?xml version="1.0" encoding="ISO-8859-1" standalone="no"?>'
  graphml.header <- c(graphml.header, '<graphml xmlns="http://graphml.graphdrawing.org/xmlns" xmlns:java="http://www.yworks.com/xml/yfiles-common/1.0/java" xmlns:sys="http://www.yworks.com/xml/yfiles-common/markup/primitives/2.0" xmlns:x="http://www.yworks.com/xml/yfiles-common/markup/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:y="http://www.yworks.com/xml/graphml" xmlns:yed="http://www.yworks.com/xml/yed/3" xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd">')
  graphml.header <- c(graphml.header, '<!--Created by jmlR.toolbox 2017 -->')
  graphml.header <- c(graphml.header, '<key for="port" id="d0" yfiles.type="portgraphics"/>')
  graphml.header <- c(graphml.header, '<key for="port" id="d1" yfiles.type="portgeometry"/>')
  graphml.header <- c(graphml.header, '<key for="port" id="d2" yfiles.type="portuserdata"/>')
  graphml.header <- c(graphml.header, '<key attr.name="url" attr.type="string" for="node" id="d3"/>')
  graphml.header <- c(graphml.header, '<key attr.name="description" attr.type="string" for="node" id="d4"/>')
  graphml.header <- c(graphml.header, '<key for="node" id="d5" yfiles.type="nodegraphics"/>')
  graphml.header <- c(graphml.header, '<key for="graphml" id="d6" yfiles.type="resources"/>')
  graphml.header <- c(graphml.header, '<key attr.name="url" attr.type="string" for="edge" id="d7"/>')
  graphml.header <- c(graphml.header, '<key attr.name="description" attr.type="string" for="edge" id="d8"/>')
  graphml.header <- c(graphml.header, '<key for="edge" id="d9" yfiles.type="edgegraphics"/>')
  graphml.header <- c(graphml.header, '<graph edgedefault="directed" id="G">')
  # INITIALIZE VARIABLES
  graphml.node <- ""
  graphml.edge <- ""
  key.base <- 1000000000
  # CREATE NODE LIST
  nodes <- c(unique(data_v[,1]), unique(data_v[,2]))
  nodes_save <- nodes
  nodes_temp <- unique(nodes_save)
  if (fill_color) {
    nodes <- cbind(nodes_temp, fill=0)
    l <- dim(nodes)
    for(i in 1:l[1]){
      nodes[i,2] <- stringr::str_sub(nodes[i,1], start=-7)
    }
  }

  # CREATE NODES
  for(i in 1:l[1]) {
    graphml.node <- c(graphml.node, paste('<node id="n', key.base+i,'">', sep=""))
    graphml.node <- c(graphml.node, '<data key="d4"/>')
    graphml.node <- c(graphml.node, '<data key="d5">')
    graphml.node <- c(graphml.node, '<y:ShapeNode>')
    graphml.node <- c(graphml.node, '<y:Geometry height="40.0" width="200.0" x="100.0" y="100.0"/>')
    if (fill_color) {
      graphml.node <- c(graphml.node, paste0('<y:Fill color="', nodes[i,2], '" transparent="false"/>'))
    }
    else {
      graphml.node <- c(graphml.node, '<y:Fill color="#FFFFFF" transparent="false"/>')
    }
    graphml.node <- c(graphml.node, '<y:BorderStyle color="#000000" type="line" width="1.0"/>')
    graphml.node <- c(graphml.node, '<y:NodeLabel alignment="center" autoSizePolicy="content" fontFamily="Dialog" fontSize="12" fontStyle="plain" hasBackgroundColor="false" hasLineColor="false" height="50" horizontalTextPosition="center" iconTextGap="4" modelName="internal" modelPosition="c" textColor="#000000" verticalTextPosition="bottom" visible="true" width="200" x="0" y="0">')
    if (fill_color) {
      entity_label <- stringr::str_sub(nodes[i,1], 1, stringr::str_length(nodes[i,1])-9)
      graphml.node <- c(graphml.node, entity_label)
    }
    else {
      graphml.node <- c(graphml.node, nodes[i])
    }
    graphml.node <- c(graphml.node, '</y:NodeLabel>')
    graphml.node <- c(graphml.node, '<y:Shape type="rectangle"/>')
    graphml.node <- c(graphml.node, '</y:ShapeNode>')
    graphml.node <- c(graphml.node, '</data>')
    graphml.node <- c(graphml.node, '</node>')
  } # end-for
  # CREATE EDGES
  for(i in 1:length(data_v[,1])) {
    key.from <- key.base + pmatch(data_v[i,1], nodes)
    key.to <- key.base + pmatch(data_v[i,2], nodes)
    graphml.node <- c(graphml.node, paste('<edge id="e', key.base+i, '" source="', 'n', key.from, '" target="', 'n', key.to, '">', sep=""))
    graphml.node <- c(graphml.node, '<data key="d4"/>')
    graphml.node <- c(graphml.node, '<data key="d5">')
    graphml.node <- c(graphml.node, '<y:PolyLineEdge>')
    graphml.node <- c(graphml.node, '<y:Path sx="0.0" sy="0.0" tx="0.0" ty="0.0"/>')
    graphml.node <- c(graphml.node, '<y:LineStyle color="#000000" type="line" width="1.0"/>')
    # if (directed) {
    #   xgml.edge <- c(xgml.edge, '<attribute key="targetArrow" type="String">standard</attribute>')
    # }
    graphml.node <- c(graphml.node, '<y:Arrows source="none" target="none"/>')
    graphml.node <- c(graphml.node, '<y:EdgeLabel alignment="center" configuration="AutoFlippingLabel" distance="2.0" fontFamily="Dialog" fontSize="12" fontStyle="plain" hasBackgroundColor="false" hasLineColor="false" height="46" horizontalTextPosition="center" iconTextGap="4" modelName="six_pos" modelPosition="head" preferredPlacement="anywhere" ratio="0.5" textColor="#000000" verticalTextPosition="bottom" visible="true" width="8" x="0" y="0">')
    graphml.node <- c(graphml.node, '<y:PreferredPlacementDescriptor angle="0.0" angleOffsetOnRightSide="0" angleReference="absolute" angleRotationOnRightSide="co" distance="-1.0" frozen="true" placement="anywhere" side="anywhere" sideReference="relative_to_edge_flow"/>')
    graphml.node <- c(graphml.node, data_v[i,3])
    graphml.node <- c(graphml.node, '</y:EdgeLabel>')
    graphml.node <- c(graphml.node, '<y:BendStyle smoothed="false"/>')
    graphml.node <- c(graphml.node, '</y:PolyLineEdge>')
    graphml.node <- c(graphml.node, '</data>')
    graphml.node <- c(graphml.node, '</edge>')
  } # end-for
  # CLEAN NODES/EDGES LIST
  graphml.node=graphml.node[-1]
  graphml.edge=graphml.edge[-1]
  # ADD GRAPHML FOOTER
  graphml.footer <- c('</graph>', '<data key="d6">', '<y:Resources/>', '</data>', '</graphml>')
  # ASSEMBLE GRAPHML COMPONENTS
  graphml <- c(graphml.header, graphml.node, graphml.edge, graphml.footer)
  # WRITE GRAPHML FILE
  write.table(graphml, data_out, col.names=F, row.names=F, quote=F)
  # OPEN GRAPHML FILE WITH VIEWER
  if (applic != "no") {
    applic <- paste(shQuote(applic), " ", shQuote(data_out))
    shell.cmd <- applic
    system(shell.cmd, wait = F)
  }
  return(0)
}

# --------------------------------------------------------------------------------------------

#' Function packages.load()
#'
#' This function install/loads a list of packages
#' @param package_name_list List of package(s) to install and/or load. Default: <empty>
#' @param b_load TRUE or FALSE load package(s) after install (in necessary). Default: TRUE
#' @return Returns a vector of character strings representing the result of the operation.
#' @author Jean Marc Lietaer, \email{jmlietaer.tech@@gmail.com}
# #' @seealso
#' @keywords utilities
#' @examples
#' # pckgs=c("alfa", "beta", "gamma")
#' # packages.load(pckgs, b_load=F)
#' @export

packages.load <- function (package_name_list="", b_load=T) {
  if (!base::require(package_name_list)) {
    utils::install.packages(package_name_list)
    if (b_load) OK <- lapply(package_name_list, base::require, character.only = TRUE)
  }
  rc <- cbind(package_name_list, OK)
  return(rc)
}

# ------------------------------------------------------------------------------


#' Function packages.unload()
#'
#' This function unloads a list of packages
#' @param package_name_list List of package(s) to unload. Default: <empty>
#' @return Returns a vector of character strings representing the result of the operation.
#' @author Jean Marc Lietaer, \email{jmlietaer.tech@@gmail.com}
# #' @seealso
#' @keywords utilities
#' @examples
#' # pckgs=c("alfa", "beta", "gamma")
#' # packages.unload(pckgs)
#' @export

packages.unload <- function (package_name_list="") {
  rc <- 9
  rc <- lapply(package_name_list, base::unloadNamespace)
  # rc <- cbind(package_name_list, unloadOK)
  return(rc)
}

# --------------------------------------------------------------------------------------------


#' Function packages.remove()
#'
#' This function removes a list of packages
#' @param package_name_list List of package(s) to remove. Default: <empty>
#' @return A vector of character strings representing the result of the operation.
#' @author Jean Marc Lietaer, \email{jmlietaer.tech@@gmail.com}
# #' @seealso
#' @keywords utilities
#' @examples
#' # pckgs=c("alfa", "beta", "gamma")
#' # packages.remove(pckgs)
#' @export

packages.remove <- function (package_name_list="") {
  rc <- 9
  rc <- lapply(package_name_list, utils::remove.packages)
  return(rc)
}

# --------------------------------------------------------------------------------------------


#' Function system.info()
#'
#' This function returns the following information:
#' @return Returns: [R version] [R platform] [system date and time] [system timezone]
#' @author Jean Marc Lietaer, \email{jmlietaer.tech@@gmail.com}
#' @keywords utilities
#' @examples
#' system.info()
# [1] "[R version 3.4.2 (2017-09-28)] [x86_64-pc-linux-gnu] [2017-11-03 16:23:14] [Europe/Brussels]"
#' @export

system.info <- function() {
  sysinfo <- paste("[",
                   R.Version()$version.string, "] [",
                   R.Version()$platform, "] [",
                   Sys.time(), "] [",
                   Sys.timezone(), "]", sep="")
  # rc <- return(sysinfo)
  return(sysinfo)
}

# --------------------------------------------------------------------------------------------


#' Function clearConsole()
#'
#' This function clears the console.
#' @author Jean Marc Lietaer, \email{jmlietaer.tech@@gmail.com}
#' @keywords utilities
#' @examples
#' # clearConsole()
#' #   clears the console
#' @export

clearConsole <- function () {
  cat("\014")
  return(invisible(0))
}

# --------------------------------------------------------------------------------------------


#' Function pause()
#'
#' This function pauses for keyboard input
#' @param t Pause text. Default: "Press a key to continue..."
#' @param i TRUE or FALSE - Do not print your input. Default: TRUE
#' @return A character string representing the result of the input.
#' @author Jean Marc Lietaer, \email{jmlietaer.tech@@gmail.com}
#' @keywords utilities
#' @examples
#' pause()
#' #  returns Press a key to continue... and waits for input
#' pause("test")
#' #  returns test and waits for input
#' pause("")
#' #  returns an empty string and waits for input
#' pause(i=FALSE)
#' #  returns Press a key to continue... and prints your input
#' #  [1] <your input>
#' s <- pause("type here = ")
#' #  returns type here =  and puts your input in variable s
#' s
#' #  [1] "test"
#' #  Credits - Based on:
#' #    http://stackoverflow.com/revisions/18746519/3
#' @export

pause <- function(t="Press a key to continue...", i=T) {
  if (i) invisible(readline(prompt=t)) else readline(prompt=t)
}

# --------------------------------------------------------------------------------------------


#
#  END OF PACKAGE
#
