#' Data: DataForEver water quality data for northern boundary of Everglades National Park
#'
#'
#'
#' @format A dataframe of water quality data from DataForEver, downloaded on 16 October 2018.
#' \describe{
#' \item{stn}{station name}
#' \item{date}{date of observation}
#' \item{param}{parameter name}
#' \item{units}{measurement units}
#' \item{year}{year (extracted from date column)}
#' \item{value}{amount of parameter measured}
#' \item{mdl}{method detection limit, where available}
#' \item{src}{data source (DataForEver)}
#'}
#' @docType data
#' @keywords data, water quality
#' @name wqDat
#' @usage wqDat
#' @examples 
#' \dontrun{
#' ### build the object using dfe.wq():
#' targetStations <- c("COOPERTN", "FROGCITY", "G311", "G342A", "G342B", "G342C", 
#' "G342D", "G344A", "G344B", "G344C", "G344D", "G344E", "G344F", 
#' "G344G", "G344H", "G344I", "G344J", "G344K", "G352A", "G352B", 
#' "G352C", "G354A", "G354C", "G370", "G372", "G374A", "G374B", 
#' "G374C", "G374D", "G374E", "G374F", "G375B", "G375E", "G376ABC", 
#' "G376DEF", "G377A", "G377B", "G377C", "G377D", "G377E", "G378B", 
#' "G378D", "G378E", "G379ABC", "G379DE", "G380A", "G380B", "G380C", 
#' "G380D", "G380E", "G380F", "G381AB", "G381CDEF", "G384B", "G384E", 
#' "G388", "G389A", "G389B", "G390A", "G390B", "G393A", "G393B", 
#' "G393C", "G508", "G724A", "G724B", "G724C", "G724D", "G724E", 
#' "G724F", "G724G", "G724H", "G724I", "G724J", "GLADER", "L29C1", 
#' "L29C4", "L30M0", "L31NM0", "L31NM1", "L31NM2", "L31NM3", "L31NM4", 
#' "L31NM5", "S118", "S120", "S121", "S122", "S123", "S12A", "S12B", 
#' "S12C", "S12D", "S14", "S148", "S149", "S150", "S151", "S165", 
#' "S173", "S175", "S176", "S178", "S179", "S18C", "S195", "S197", 
#' "S20", "S21", "S319", "S332", "S333", "S334", "S335", "S335TW", 
#' "S336", "S339", "S340", "S343A", "S343B", "S344", "S346", "S355A", 
#' "S355ATW", "S355B", "S355BTW", "S356", "S356-334", "S356GW1", 
#' "S356GW2", "S356GW3", "S356GW4", "S361", "S362", "S7", "S700",
#' "S22", "S25", "S25A", "S25B", "S26", # central
#' "S27", "S28", "S29", # from SWIM plan - north biscayne bay
#' "G58", "S700", "G93", # new structures since SWIM plan?
#' "S123",  "S21A", "S21", "S20", "S20F", "S20G", "S197"))
#' 
#' testDat <- getWQ(stns = targetStations)
#' 
#' 
#' ### export to .csv: 
#' write.csv(testDat, file = file.path(tempdir(), "wqDat.csv"))
#' }
#' head(wqDat)
#' 
NULL
