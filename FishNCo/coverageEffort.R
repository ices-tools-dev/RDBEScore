library(RDBEScore)
library(dplyr)
library(lubridate)
library(sf)
library(ggplot2)
library(rgdal)
library(DT)
library(plotly)
library(biscale)
library(cowplot)
library(leaflet)
library(ggiraph)
library(viridis)
library(magrittr)
library(tidyverse)
library(reshape2)
library(readxl)
##############################
#' Provides graphical outputs to illustrate potential bias in commercial
#' sampling data.
#'
#' This function is to be used with Commercial Effort data in the ICES RDBES
#' data format.
#'

#'
#'
#' @param year Year to be assessed e.g 2021
#' @param quarter Quarter to be assessed - possible choices 1,2,3 or 4.
#' @param Vessel_flag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param var Variable of interest - "gear" or "Statrec"
#' @param CommercialVariable Effort variable to be assessed
#' @param SamplingVariable Sampling Variable to be assessed
#' @param CatchCat Sampling catch category - landings, catch or discards
#' @param SpatialPlot Type of Spatial plot to return - bivariate or points
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1:
#' biasEffort(
#'   dataToPlot = testData,
#'   year = 2020,
#'   Vessel_flag = "IE",
#'   var = "gear",
#'   CatchCat = "Lan"
#' )
#'
#' # Example 2:
#' biasEffort(
#'   dataToPlot = testData,
#'   year = 2020,
#'   Vessel_flag = "ES",
#'   CommercialVariable = "CEscikWDaySea",
#'   SamplingVariable = "SAsampWtMes",
#'   CatchCat = "Dis"
#' )
#'
#' # Example 3:
#' biasEffort(
#'   dataToPlot = testData,
#'   year = 2020,
#'   var = "Statrec",
#'   CommercialVariable = "CEsciNumHaulSet",
#'   SamplingVariable = "SAsampWtMes",
#'   CatchCat = "Lan",
#'   SpatialPlot = "Bivariate"
#' )
#' }
coverageEffort <- function(dataToPlot,
                           year = NA,
                           quarter = NA,
                           Vessel_flag = NA,
                           var = c("gear", "Statrec"),
                           CommercialVariable = c(
                             "CEnumFracTrips",
                             "CEnumDomTrip",
                             "CEoffDaySea",
                             "CESciDaySea",
                             "CEoffFishDay",
                             "CEsciFishDay",
                             "CEoffNumHaulSet",
                             "CEsciNumHaulSet",
                             "CEoffVesFishHour",
                             "CEsciVesFishHour",
                             "CEoffSoakMeterHour",
                             "CEsciSoakMeterHour",
                             "CEoffkWDaySea",
                             "CEscikWDaySea",
                             "CEoffkWFishDay",
                             "CEscikWFishDay",
                             "CEoffkWFishHour",
                             "CEscikWFishHour",
                             "CEgTDaySea",
                             "CEgTFishDay",
                             "CEgTFishHour"
                           ),
                           SamplingVariable = c(
                             "SAsampWtLive",
                             "SAnumSamp",
                             "SAsampWtMes"
                           ),
                           CatchCat = c(
                             "Lan",
                             "Dis",
                             "Catch"
                           ),
                           SpatialPlot = c(
                             "Bivariate",
                             "Points"
                           )) {
  if (length(CatchCat) == 3) {
    stop("You must provide a Catch Category")
  } else if (length(CatchCat) == 2) {
    stop("Only one Catch Category can be provided")
  } else {
    CatchCat
  }

  if (length(Vessel_flag) > 1) {
    stop("Only one vessel flag country can be provided")
  }

  if (var == "species") {
    stop("Species is not a variable for Effort")
  }

  if (var == "Statrec" || length(var) > 1) {
    if (length(CommercialVariable) > 1 ||
      length(SamplingVariable) > 1) {
      stop("You must provide  CommercialVariable and  SamplingVariable")
    }
  }

  if (var == "Statrec" && length(SpatialPlot) > 1) {
    stop("You must choose a Spatial Plot")
  }

  if (var == "Statrec" && is.na(quarter) == FALSE) {
    stop("Unused argument, no quarters available for  var Statrec")
  }

  if (var == "Statrec" && is.na(year) == TRUE) {
    stop("You must provide a year")
  }

  # We'll convert the CS data into a RDBESEstObject to
  # make it easier to handle here
  validateRDBESDataObject(dataToPlot)

  hierarchiesInData <- unique(dataToPlot[["DE"]]$DEhierarchy)
  if (length(hierarchiesInData)!=1) {
    stop("This function will only work if there is a single hierarchy in dataToPlot")
  }
  datatoPlot_EstOb <- createRDBESEstObject(dataToPlot, hierarchiesInData)

  # join to spatial data
  ices_rect <- readOGR(
    dsn = paste0(getwd(), "/Maps/shapefiles"),
    layer = "ICESrect",
    verbose = FALSE
  )

  # get country boundaries shapefile
  shoreline <- readOGR(
    dsn = paste0(
      getwd(),
      "/Maps/shapefiles/GSHHG/gshhg-shp-2.3.7/GSHHS_shp/l"
    ),
    layer = "GSHHS_l_L1",
    verbose = FALSE
  )

  # create dataframe
  ices_rect_df <- ices_rect@data

  myRDBESData <- dataToPlot

  # get effort
  EF <- myRDBESData[["CE"]] %>%
    select(
      CEvesFlagCou:CEMonth,
      CEstatRect,
      CEmetier6,
      CEvesLenCat,
      CEnumFracTrips:CEnumUniqVes
    )
  EF$CEGear <- substr(EF$CEmetier6, 0, 3)

  # get sampling
  SA <- datatoPlot_EstOb

  # Join to VD to get Vessel flag country
  SA <- left_join(SA, myRDBESData[["VD"]], by = "VDid")

  # Get the year and quarter of the sample from FO
  SA$SAyear <-
    as.integer(format(as.Date(SA$FOendDate, format = "%Y-%m-%d"), "%Y"))
  SA$SAquar <-
    as.integer(lubridate::quarter(as.Date(SA$FOendDate, format = "%Y-%m-%d")))
  SA$SAmonth <-
    as.integer(lubridate::month(as.Date(SA$FOendDate, format = "%Y-%m-%d")))

  # Get only necessary columns

  # Find the first SA columns - we are only dealing with the top level SA data
  colsToCheck <-
    names(datatoPlot_EstOb)[grep("^su.table$",names(datatoPlot_EstOb))]
  correctCol <- NA
  suNumber <- NA
  for (myCol in colsToCheck){
    myColValues <- unique(datatoPlot_EstOb[,..myCol])[[1]]
    myColValues <- myColValues[!is.na(myColValues)]
    if (myColValues == "SA"){
      correctCol <- myCol
      suNumber <- gsub("su","",correctCol)
      suNumber <- gsub("table","",suNumber)
      suNumber <- as.integer(suNumber)
      break
    }
  }
  if (is.na(correctCol)) {
    stop("Sample data could not be found - cannot continue")
  }

  # Rename the suXnumTotal and suXnumSamp columns to SAnumTotal and SAnumSamp
  SA <- SA %>% rename("SAnumTotal" = paste0("su",suNumber,"numTotal"))
  SA <- SA %>% rename("SAnumSamp" = paste0("su",suNumber,"numSamp"))

  SA <- SA %>%
    select(
      c("SAmetier5", "SAmetier6", "SAgear", "SAtotalWtLive",
        "SAsampWtLive",
        "SAnumTotal","SAnumSamp",
        "SAtotalWtMes", "SAsampWtMes", "SAyear", "SAquar", "SAmonth",
        "SAcatchCat", "SAspeCode", "SAspeCodeFAO", "SAstatRect",
        "VDflgCtry",
        "SAid")
    )%>%
    relocate(SAstatRect, SAyear, SAquar, SAmonth)

  if (length(which(duplicated(SA))) > 0) {
    SA <- SA[-which(duplicated(SA)), ]
  }

  # Remove any rows with SAid = NA, then get rid of the SAid column
  SA <- SA[!is.na(SA$SAid),]
  SA <- select(SA,-SAid)

  #########################

  if (is.na(year) == TRUE && is.na(quarter) == TRUE) {
    if (is.na(Vessel_flag) == TRUE) {
      EF1 <- EF
      SA1 <- SA %>% filter(SAcatchCat %in% CatchCat)
    } else {
      EF1 <- EF %>% filter(CEvesFlagCou %in% Vessel_flag)
      SA1 <- SA %>% filter(VDflgCtry %in% Vessel_flag)
      SA1 <- SA1 %>% filter(SAcatchCat %in% CatchCat)
    }
  } else if (is.na(year) == FALSE && is.na(quarter) == TRUE) {
    if (is.na(Vessel_flag) == TRUE) {
      EF1 <- EF %>% filter(CEyear %in% year)
      SA1 <- SA %>% filter(SAyear %in% year)
      SA1 <- SA1 %>% filter(SAcatchCat %in% CatchCat)
    } else {
      EF1 <- EF %>% filter(CEyear %in% year)
      SA1 <- SA %>% filter(SAyear %in% year)
      EF1 <- EF1 %>% filter(CEvesFlagCou %in% Vessel_flag)
      SA1 <- SA1 %>% filter(VDflgCtry %in% Vessel_flag)
      SA1 <- SA1 %>% filter(SAcatchCat %in% CatchCat)
    }
  } else if (is.na(year) == TRUE && is.na(quarter) == FALSE) {
    if (is.na(Vessel_flag) == TRUE) {
      EF1 <- EF %>% filter(CEquar %in% quarter)
      SA1 <- SA %>% filter(SAquar %in% quarter)
      SA1 <- SA1 %>% filter(SAcatchCat %in% CatchCat)
    } else {
      EF1 <- EF %>% filter(CEquar %in% quarter)
      SA1 <- SA %>% filter(SAquar %in% quarter)
      EF1 <- EF1 %>% filter(CEvesFlagCou %in% Vessel_flag)
      SA1 <- SA1 %>% filter(VDflgCtry %in% Vessel_flag)
      SA1 <- SA1 %>% filter(SAcatchCat %in% CatchCat)
    }
  } else if (is.na(year) == FALSE && is.na(quarter) == FALSE) {
    if (is.na(Vessel_flag) == TRUE) {
      EF1 <- EF %>% filter(CEyear %in% year)
      SA1 <- SA %>% filter(SAyear %in% year)
      EF1 <- EF1 %>% filter(CEquar %in% quarter)
      SA1 <- SA1 %>% filter(SAquar %in% quarter)
      SA1 <- SA1 %>% filter(SAcatchCat %in% CatchCat)
    } else {
      EF1 <- EF %>% filter(CEyear %in% year)
      SA1 <- SA %>% filter(SAyear %in% year)
      EF1 <- EF1 %>% filter(CEquar %in% quarter)
      SA1 <- SA1 %>% filter(SAquar %in% quarter)
      EF1 <- EF1 %>% filter(CEvesFlagCou %in% Vessel_flag)
      SA1 <- SA1 %>% filter(VDflgCtry %in% Vessel_flag)
      SA1 <- SA1 %>% filter(SAcatchCat %in% CatchCat)
    }
  }

  if (is.na(Vessel_flag)) {
    flag_label <- "All"
  } else {
    flag_label <- Vessel_flag
  }

  ##################################################################
  ########################## TEMPORAL #########################


  if (length(var) > 1) {
    d1 <- na.omit(EF1 %>% group_by(CEyear, CEquar) %>%
      summarize(CE = sum(!!sym(
        CommercialVariable
      )))) %>%
      mutate(relCE = CE / sum(CE))
    d2 <- na.omit(SA1 %>% group_by(SAyear, SAquar) %>%
      summarize(SA = sum(!!sym(
        SamplingVariable
      )))) %>%
      mutate(relSA = SA / sum(SA))

    df <-
      left_join(d1, d2, by = c("CEyear" = "SAyear", "CEquar" = "SAquar"))

    y <- unique(df$CEyear)


    all_plot <- htmltools::tagList()
    # all_plot<-list()
    for (i in 1:length(y)) {
      set <- df %>% filter(CEyear == y[i])
      show_legend <- if (i == 1) {
        TRUE
      } else {
        FALSE
      }
      all_plot[[i]] <- plot_ly(
        set,
        x = ~CEquar,
        y = ~relCE,
        type = "bar",
        alpha = 0.7,
        name = "Effort",
        hovertemplate = paste(
          "%{yaxis.title.text}:  %{y}<br>",
          "%{xaxis.title.text}: %{x}<br>"
        ),
        showlegend = show_legend,
        marker = list(
          color = "rgb(168, 74, 50)",
          line = list(color = "rgb(8,48,107)", width = 1.5)
        )
      ) %>%
        add_trace(
          y = ~relSA,
          name = "Sampling",
          alpha = 0.7,
          showlegend = show_legend,
          marker = list(
            color = "rgb(158,202,225)",
            line = list(color = "rgb(15,48,107)", width = 1.5)
          )
        ) %>%
        layout(
          title = paste0(
            "Vessel Flag ",
            flag_label,
            " | Effort: ",
            CommercialVariable,
            " vs Sampling: ",
            SamplingVariable,
            " - (",
            CatchCat,
            ") in ",
            y[i]
          ),
          xaxis = list(title = "Quarter"),
          yaxis = list(title = "Relative Values")
        )
    }
    all_plot
  }

  ######################## GEAR ####################

  else if (var == "gear") {
    if (is.na(quarter) == FALSE) {
      df1 <- na.omit(
        EF1 %>% group_by(CEyear, CEquar) %>%
          add_count(CEGear, name = "CEGearCount") %>%
          summarise(EffortGearCountQuar = sum(CEGearCount))
      )

      d1 <- na.omit(
        EF1 %>% group_by(CEyear, CEquar, CEGear) %>%
          add_count(CEGear, name = "CEGearCount") %>%
          summarise(EffortGearCount = sum(CEGearCount))
      )

      d1 <- left_join(d1, df1, by = "CEyear", "CEquar") %>%
        mutate(relativeValuesE = EffortGearCount / EffortGearCountQuar)

      df2 <- na.omit(
        SA1 %>% group_by(SAyear, SAquar) %>%
          add_count(SAgear, name = "SAGearCount") %>%
          summarise(SamplingGearCountQuar = sum(SAGearCount))
      )

      d2 <- na.omit(
        SA1 %>% group_by(SAyear, SAquar, SAgear) %>%
          add_count(SAgear, name = "SAGearCount") %>%
          summarise(SamplingGearCount = sum(SAGearCount))
      )

      d2 <- left_join(d2, df2, by = "SAyear", "SAquar") %>%
        mutate(relativeValuesS = SamplingGearCount / SamplingGearCountQuar)
    } else {
      df1 <- na.omit(
        EF1 %>% group_by(CEyear) %>%
          add_count(CEGear, name = "CEGearCount") %>%
          summarise(totalGearYear = sum(CEGearCount))
      )

      d1 <- na.omit(
        EF1 %>% group_by(CEyear, CEGear) %>%
          add_count(CEGear, name = "CEGearCount") %>%
          summarise(EffortGearCount = sum(CEGearCount))
      )

      d1 <- left_join(d1, df1, by = "CEyear") %>%
        mutate(relativeValuesE = EffortGearCount / totalGearYear)

      df2 <- na.omit(
        SA1 %>% group_by(SAyear) %>%
          add_count(SAgear, name = "SAGearCount") %>%
          summarise(SamplingGearCountYear = sum(SAGearCount))
      )

      d2 <- na.omit(
        SA1 %>% group_by(SAyear, SAgear) %>%
          add_count(SAgear, name = "SAGearCount") %>%
          summarise(SamplingGearCount = sum(SAGearCount))
      )

      d2 <- left_join(d2, df2, by = "SAyear") %>%
        mutate(relativeValuesS = SamplingGearCount / SamplingGearCountYear)
    }


    df <- left_join(d1, d2, by = c("CEyear" = "SAyear", "CEGear" = "SAgear"))
    # df = df %>% select(-c(relativeValuesE, totalSamplingYear))
    y <- unique(df$CEyear)

    all_plot_gear <- htmltools::tagList()

    for (i in 1:length(y)) {
      dd <- d1 %>% filter(CEyear == y[i])
      dd <- dd[-1]
      ds <- d2 %>% filter(SAyear == y[i])
      ds <- ds[-1]
      p1 <- plot_ly(
        dd,
        x = ~ as.character(CEGear),
        y = ~relativeValuesE,
        color = ~ as.character(CEGear),
        type = "bar",
        showlegend = F
      ) %>%
        layout(
          title = paste0("Vessel Flag ",
                         flag_label,
                         " : Top Effort \nGear - Relative Values per Plot in",
                         y[i]),
          yaxis = list(title = "Effort"),
          xaxis = list(categoryorder = "total descending"),
          barmode = "stack"
        )
      p2 <- plot_ly(
        ds,
        x = ~ as.character(SAgear),
        y = ~relativeValuesS,
        color = ~ as.character(SAgear),
        type = "bar",
        showlegend = F
      ) %>%
        layout(
          title = paste0(
                      "Vessel Flag ",
                      flag_label,
                      " : Top Effort and Sampling Gear (",
                      CatchCat,
                      ")\n Relative Values per Plot in ",
                      y[i]
          ),
          yaxis = list(title = "Sampling"),
          xaxis = list(categoryorder = "total descending"),
          barmode = "stack"
        )

      all_plot_gear[[i]] <- subplot(p1, p2, titleY = TRUE, nrows = 2)
    }
    all_plot_gear
  }

  ################# SPATIAL #############

  else if (var == "Statrec") {
    d1 <- na.omit(EF1 %>% group_by(CEyear, CEstatRect) %>%
      summarize(CE = sum(!!sym(
        CommercialVariable
      ))))
    d2 <- na.omit(SA1 %>% group_by(SAyear, SAstatRect) %>%
      summarize(SA = sum(!!sym(
        SamplingVariable
      ))))

    df <-
      left_join(d1, d2, by = c("CEyear" = "SAyear",
                               "CEstatRect" = "SAstatRect"))

    y <- unique(df$CEyear)
    all_plot_sp <- htmltools::tagList()
    for (i in 1:length(y)) {
      dd <- df %>% filter(CEyear == y[i])

      ##########################################################################
      ############## bivariate plot#################

      if (SpatialPlot == "Bivariate") {
        dd <- dd %>%
          mutate_if(is.numeric, ~ replace(., is.na(.), 1))

        # create Classes

        biToPlot <-
          bi_class(
            dd,
            x = SA,
            y = CE,
            style = "jenks",
            dim = 3
          )
        # join to our data
        bi_ices <-
          left_join(ices_rect_df, biToPlot, by = c("ICESNAME" = "CEstatRect"))

        # assign back to ices rectangles
        ices_rect@data <- bi_ices

        # get ICES rectangles feature collection
        bi_fc <- st_as_sf(ices_rect)

        # ICES rectangles feature collection without NAs for bounding box
        bi_fc_No_NA <- na.omit(bi_fc)


        # create map
        map <- ggplot() +
          geom_polygon(
            data = shoreline,
            aes(x = long, y = lat, group = group),
            color = "white",
            fill = "azure4",
            show.legend = FALSE
          ) +
          geom_sf(
            data = bi_fc,
            mapping = aes(fill = bi_class),
            color = "transparent",
            size = 0.1,
            show.legend = FALSE
          ) +
          bi_scale_fill(
            pal = "GrPink",
            dim = 3,
            na.value = "transparent"
          ) +
          labs(
            title = paste0("Vessel Flag: ", flag_label),
            subtitle = paste0(
              "Sampling - ",
              CatchCat, " (",
              SamplingVariable,
              ")  vs Landings (",
              CommercialVariable,
              ") in ",
              y[i]
            )
          ) +
          coord_sf(
            xlim = c(st_bbox(bi_fc_No_NA)[1], st_bbox(bi_fc_No_NA)[3]),
            ylim = c(st_bbox(bi_fc_No_NA)[2], st_bbox(bi_fc_No_NA)[4]),
            expand = FALSE
          ) +
          theme(
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none",
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()
          )


        # create legend

        legend <- bi_legend(
          pal = "GrPink",
          dim = 3,
          xlab = "Higher Sampling",
          ylab = "Higher Effort",
          size = 9
        )

        # combine map with legend

        finalPlot <- ggdraw() +
          draw_plot(
            map,
            x = 0,
            y = 0,
            width = 1,
            height = 1
          ) +
          draw_plot(legend, -0.05, .25, 0.4, 0.4)
        print(finalPlot)
      } else {

        ########################################################################
        ########################################################################
        ############# points plot###############

        # get ices shp
        ices_rects <- read_sf("Maps/shapefiles/ICESrect.shp")

        ices_rects %<>%
          left_join(dd, by = c("ICESNAME" = "CEstatRect"))

        # get extent of plot

        No_NA <- ices_rects[ices_rects$CE != "NA", ]
        xlim1 <- st_bbox(No_NA)[1]
        ylim2 <- st_bbox(No_NA)[2]
        xlim3 <- st_bbox(No_NA)[3]
        ylim4 <- st_bbox(No_NA)[4]

        # define number of Classes
        no_classes <- 6

        # extract quantiles
        quantiles <- ices_rects %>%
          pull(CE) %>%
          quantile(
            probs = seq(0, 1, length.out = no_classes + 1),
            na.rm = TRUE
          ) %>%
          as.vector() # to remove names of quantiles, so idx below is numeric
        quantiles <- round(quantiles, 0)

        # create custom labels
        labels <- imap_chr(quantiles, function(., idx) {
          return(paste0(
            round(quantiles[idx], 10),
            " - ",
            round(quantiles[idx + 1], 10)
          ))
        })

        # remove last lable that includes NA
        labels <- labels[1:length(labels) - 1]

        # create new variable with quantiles - Effort
        ices_rects %<>%
          mutate(mean_quantiles_land = cut(
            CE,
            breaks = quantiles,
            labels = labels,
            include.lowest = T
          ))


        # create point on surface
        points <- st_coordinates(st_point_on_surface(ices_rects))
        points <- as.data.frame(points)
        points$SA <- ices_rects$SA

        # plot univariate map with points
        gg <- ggplot(data = ices_rects) +
          geom_polygon(
            data = shoreline,
            aes(x = long, y = lat, group = group),
            color = "white",
            fill = "gray",
            show.legend = FALSE
          ) +
          geom_sf_interactive(
            aes(
              fill = mean_quantiles_land,
              tooltip = paste("Effort:", CE)
            ),
            color = "white",
            size = 0.1
          ) +
          scale_fill_brewer_interactive(
            type = "seq",
            palette = "RdYlBu",
            name = "Effort Variable",
            direction = -1,
            guide = guide_legend(
              keyheight = unit(5, units = "mm"),
              title.position = "top",
              reverse = T
            )
          ) +
          geom_point_interactive(
            data = points,
            aes(
              x = X,
              y = Y,
              size = SA,
              tooltip = paste("Sampling: ", SA)
            ),
            # color = "white",
            shape = 1,
            color = "black",
            alpha = 0.5
          ) +
          coord_sf(
            xlim = c(xlim1, xlim3),
            ylim = c(ylim2, ylim4)
          ) +
          # add titles
          labs(
            x = NULL,
            y = NULL,
            title = paste0("Vessels Flag:  ", flag_label),
            subtitle = paste0(
              "Sampling - ",
              CatchCat, " (",
              SamplingVariable,
              ")  vs Landings (",
              CommercialVariable,
              ") in ",
              y[i]
            ),
            size = "Sampling Variable"
          ) +
          theme(
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.background = element_rect(color = "gray"),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()
          )



        x <- girafe(ggobj = gg)
        x <- girafe_options(
          x,
          opts_zoom(min = .4, max = 4)
        )

        all_plot_sp[[i]] <- x
      }
    }
    all_plot_sp
  }
}
