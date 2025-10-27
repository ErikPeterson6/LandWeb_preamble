NW_AB <- function(ml, studyAreaName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  AB <- canProvs[canProvs$NAME_1 == "Alberta", ]

  targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                     "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  #nw_ab <- sf::st_read("inputs/NW_AB.shp")
  nw_ab <-prepInputs(
    url="https://drive.google.com/file/d/1kRb3fOSQEOTDZFhI_SEeF-Q7Mob0_cPy",
    targetFile = "NW_AB.shp",
    alsoExtract = "similar",
    projectTo=targetCRS
  )

  # make sure the reporting polygon has labels joinReportingPolygons expects
  if (!"Name" %in% names(nw_ab))   nw_ab$Name   <- "NW_AB"
  nw_ab$Name   <- as.character(nw_ab$Name)
  if (!"Name.1" %in% names(nw_ab)) nw_ab$Name.1 <- nw_ab$Name
  nw_ab$Name.1 <- as.character(nw_ab$Name.1)
  nw_ab$shinyLabel <- rep_len(nw_ab$Name, nrow(nw_ab))

  ## reportingPolygons
  tmp <- postProcess(ml[["National Ecozones"]],
                     studyArea = nw_ab, useSAcrs = TRUE,
                     filename2 = file.path(dataDir, "nw_ab_NATLEZ.shp"))
  tmp$Name.1 <- if ("Name" %in% names(tmp)) as.character(tmp$Name) else
    if ("ECOZONE" %in% names(tmp)) as.character(tmp$ECOZONE) else
      paste0("poly_", seq_len(nrow(tmp)))
  nw_ab.natlez <- joinReportingPolygons(tmp, nw_ab)
  if (!"Name" %in% names(nw_ab.natlez)) {
    nw_ab.natlez$Name <- if ("Name.1" %in% names(nw_ab.natlez)) as.character(nw_ab.natlez$Name.1) else
      paste0("poly_", seq_len(nrow(nw_ab.natlez)))
  }
  nw_ab.natlez$Name <- rep_len(as.character(nw_ab.natlez$Name), nrow(nw_ab.natlez))
  nw_ab.natlez$shinyLabel <- rep_len(nw_ab.natlez$Name, nrow(nw_ab.natlez))

  tmp <- postProcess(ml[["National Ecoregions"]],
                     studyArea = nw_ab, useSAcrs = TRUE,
                     filename2 = file.path(dataDir, "nw_ab_NATLER.shp"))
  tmp$Name.1 <- if ("Name" %in% names(tmp)) as.character(tmp$Name) else
    if ("ECOREGION" %in% names(tmp)) as.character(tmp$ECOREGION) else
      paste0("poly_", seq_len(nrow(tmp)))
  nw_ab.natler <- joinReportingPolygons(tmp, nw_ab)
  if (!"Name" %in% names(nw_ab.natler)) {
    nw_ab.natler$Name <- if ("Name.1" %in% names(nw_ab.natler)) as.character(nw_ab.natler$Name.1) else
      paste0("poly_", seq_len(nrow(nw_ab.natler)))
  }
  nw_ab.natler$Name <- rep_len(as.character(nw_ab.natler$Name), nrow(nw_ab.natler))
  nw_ab.natler$shinyLabel <- rep_len(nw_ab.natler$Name, nrow(nw_ab.natler))

  nw_ab.ansr <- ml[["Alberta Natural Subregions"]]
  if (!"Name" %in% names(nw_ab.ansr)) {
    # pick a reasonable existing attribute if present; else fallback
    cand <- intersect(c("NSN", "Name.1", "Name"), names(nw_ab.ansr))
    if (length(cand)) {
      nw_ab.ansr$Name <- as.character(nw_ab.ansr[[cand[1]]])
    } else {
      nw_ab.ansr$Name <- paste0("poly_", seq_len(nrow(nw_ab.ansr)))
    }
  }
  nw_ab.ansr$Name <- rep_len(as.character(nw_ab.ansr$Name), nrow(nw_ab.ansr))
  nw_ab.ansr$shinyLabel <- rep_len(nw_ab.ansr$Name, nrow(nw_ab.ansr))
  #nw_ab[["Name"]] <- nw_ab[["NAME_1"]]

  tmp <- postProcess(
    ml[["LandWeb Caribou Ranges"]],
    studyArea = nw_ab, useSAcrs = TRUE,
    filename2 = file.path(dataDir, "nw_ab_caribou.shp"), overwrite = TRUE
  )
  tmp$Name.1 <- if ("Name" %in% names(tmp) && length(tmp$Name) == nrow(tmp) && nrow(tmp) > 0)
    as.character(tmp$Name)
  else
    paste0("poly_", seq_len(nrow(tmp)))
  nw_ab.caribou <- joinReportingPolygons(tmp, nw_ab)
  if (!"Name" %in% names(nw_ab.caribou)) {
    nw_ab.caribou$Name <- if ("Name.1" %in% names(nw_ab.caribou)) as.character(nw_ab.caribou$Name.1) else
      paste0("poly_", seq_len(nrow(nw_ab.caribou)))
  }
  nw_ab.caribou$Name <- rep_len(as.character(nw_ab.caribou$Name), nrow(nw_ab.caribou))
  nw_ab.caribou$shinyLabel <- rep_len(nw_ab.caribou$Name, nrow(nw_ab.caribou))
  # Map adds
  ml <- mapAdd(nw_ab, ml, layerName = "nw_ab", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "nw_ab", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(nw_ab.natlez, ml, layerName = "nw_ab NATLEZ", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "nw_ab NATLEZ",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(nw_ab.natler, ml, layerName = "nw_ab NATLER", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "nw_ab NATLER",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(nw_ab.ansr, ml, layerName = "nw_ab ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "nw_ab ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(nw_ab.caribou, ml, layerName = "nw_ab Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "nw_ab Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## AB FMU boundaries (replaces previously added FMU map, for use as reporting polygon)
  ml <- mapAdd(map = ml, layerName = "AB FMU Boundaries",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1OH3b5pwjumm1ToytDBDI6jthVe2pp0tS", # 2025
               analysisGroupReportingPolygon = "AB FMU Boundaries", isStudyArea = FALSE,
               columnNameForLabels = "FMU_NAME", filename2 = NULL)
  ml[["AB FMU Boundaries"]][["Name"]] <- ml[["AB FMU Boundaries"]][["shinyLabel"]]

  ## AB Land Use Framework Planning Regions
  # TODO: clean up these polygons?
  #  #ml <- mapAdd(map = ml, layerName = "AB Land Use Framework Planning Regions",
  #               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
  #               url = "https://drive.google.com/file/d/1RnLGnuX0r9EGJ11YL2mov7n-Vgke0uTC",
  #               analysisGroupReportingPolygon = "AB Land Use Framework Planning Regions", isStudyArea = FALSE,
  #               columnNameForLabels = "LUF_NAME", filename2 = NULL)
  # # ml[["AB Land Use Framework Planning Regions"]][["Name"]] <- ml[["AB Land Use Framework Planning Regions"]][["shinyLabel"]]
  #
  #  ## AB regional planning units
  ml <- mapAdd(map = ml, layerName = "AB SUBR Bistcho Lake",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1taLQF-J69y7qweOsfDJ2f2wvTTvP_n6C",
               analysisGroupReportingPolygon = "AB SUBR Bistcho Lake", isStudyArea = FALSE,
               columnNameForLabels = "SUBR_NAME", filename2 = NULL)
  ml[["AB SUBR Bistcho Lake"]][["Name"]] <- ml[["AB SUBR Bistcho Lake"]][["shinyLabel"]]

  # ml <- mapAdd(map = ml, layerName = "AB SUBR Cold Lake",
  #              useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
  #              url = "https://drive.google.com/file/d/1jy4u2OyhnLjj1Wp_t27pPz_MsmMhPoSg",
  #              analysisGroupReportingPolygon = "AB SUBR Cold Lake", isStudyArea = FALSE,
  #              columnNameForLabels = "SUBR_NAME", filename2 = NULL)
  # ml[["AB SUBR Cold Lake"]][["Name"]] <- ml[["AB SUBR Cold Lake"]][["shinyLabel"]]
  #
  # ml <- mapAdd(map = ml, layerName = "AB SUBR Upper Smoky",
  #              useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
  #              url = "https://drive.google.com/file/d/1T2FgfHdHy41GsLnfSxWW0hpEqwo3f5VF",
  #              analysisGroupReportingPolygon = "AB SUBR Upper Smoky", isStudyArea = FALSE,
  #              columnNameForLabels = "SUBR_NAME", filename2 = NULL)
  # ml[["AB SUBR Upper Smoky"]][["Name"]] <- ml[["AB SUBR Upper Smoky"]][["shinyLabel"]]

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  ## CHANGED: use sf::st_buffer instead of amc::outerBuffer (sf vs sp)
  nw_ab_buf <- sf::st_buffer(sf::st_make_valid(sf::st_union(nw_ab)), dist = bufferDist)

  nw_ab_sr <- postProcess(ml[["LandWeb Study Area"]],
                          studyArea = nw_ab_buf,
                          useSAcrs = TRUE,
                          filename2 = file.path(dataDir, "nw_ab_SR.shp"),
                          overwrite = TRUE)

  # plotFMA(nw_ab, provs = AB, caribou = nw_ab.caribou, xsr = nw_ab_sr,
  #         title = "Alberta",
  #         png = file.path(dataDir, "nw_ab.png"))

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(nw_ab_sr, ml, isStudyArea = TRUE, layerName = "nw_ab SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
