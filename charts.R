# This code was partially adapted from an original
# written by Timo Grossenbacher
# (timogrossenbacher.ch) and can be be found here:
# https://github.com/grssnbchr/thematic-maps-ggplot2
#
# The original code is licensed under CC-BY-SA:
# https://creativecommons.org/licenses/by-sa/3.0/
#
# Any code found in this reposity is also liscensed under
# CC-BY-SA. Enjoy!

detachAllPackages <- function() {
  # This is a script to detach packges and require
  # (and install if not installed) those needed for
  # other scripts. It also adds a map theme.
  basic.packages.blank <-  c("stats",
                             "graphics",
                             "grDevices",
                             "utils",
                             "datasets",
                             "methods",
                             "base")
  basic.packages <- paste("package:", basic.packages.blank, sep = "")

  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1,
                                  TRUE,
                                  FALSE)]

  package.list <- setdiff(package.list, basic.packages)

  if (length(package.list) > 0)  for (package in package.list) {
    detach(package, character.only = TRUE)
    print(paste("package ", package, " detached", sep = ""))
  }
}
loadPackages <- function() {
  if(!require(scales)) {
    install.packages("scales", repos="http://cloud.r-project.org")
    require(scales)
  }
  if(!require(ggplot2)) {
    install.packages("ggplot2", repos="http://cloud.r-project.org")
    require(ggplot2)
  }
  if(!require(plyr)) {
    install.packages("plyr", repos = "https://cloud.r-project.org/")
    require(plyr)
  }
  if(!require(dplyr)) {
    install.packages("dplyr", repos = "https://cloud.r-project.org/")
    require(dplyr)
  }
  if(!require(RSocrata)) {
    install.packages("RSocrata", repos = "https://cloud.r-project.org/")
    require(RSocrata)
  }
  if(!require(RColorBrewer)) {
    install.packages("RcolorBrewer", repos = "https://cloud.r-project.org/")
    require(RColorBrewer)
  }
}

detachAllPackages()
loadPackages()

theme_map <- function(...) {
  theme_minimal() +
  theme(
    text=element_text(family="Helvetica", color="#22211d"),
    plot.title=element_text(face="bold"),
    axis.line=element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    panel.grid.major=element_line(color="#ebebe5", size=0.8),
    panel.grid.minor=element_line(color="#ebebe5", size=0.2),,
    plot.background=element_rect(fill="#f5f5f2", color=NA),
    panel.background=element_rect(fill="#f5f5f2", color=NA),
    legend.background=element_rect(fill="#f5f5f2", color=NA),
    panel.border=element_blank(),
  )
}

g <- ggplot() +
    scale_y_continuous(labels=comma) +
    scale_fill_manual(NULL, values=brewer.pal(8, "Set2")) +
    theme_map() +
    labs(
      y="Votes",
      caption="A maximum of eight candidates are shown.\n\nScott Leitch, @leitchsd, 2017.")

chartResults <- function(chart) {
  # 2013 Edmonton Election results API: https://data.edmonton.ca/resource/ee98-x4ib
  # This will need to be changed for LIVE RESULTS TKTK
  response <- read.socrata("https://data.edmonton.ca/resource/ee98-x4ib.json")
  # reported_at_datetime <- as.POSIXct(response$reported_at, tz="America/Edmonton", format="%A, %B %d, %Y %I:%M %p")

  plots <- tbl_df(response) %>%
           mutate(
            out_of=as.integer(out_of),
            percentage=as.numeric(percentage),
            race=as.integer(race),
            race_id=as.integer(race_id),
            reported_at=as.POSIXct(reported_at, tz="America/Edmonton", format="%A, %B %d, %Y %I:%M %p"),
            reporting=as.integer(reporting),
            votes_cast=as.integer(votes_cast),
            votes_received=as.integer(votes_received),
            candidate_name=as.factor(candidate_name)
          ) %>%
          group_by(race_id) %>%
          dlply(.(race_id), function(x) {
            to_chart <- head(x, 8)
            sorted <- to_chart[order(to_chart$votes_received, decreasing=T),]
            subtitle <- paste(" With", x$reporting, "of", x$out_of, "polls reporting",
                              sorted[1, 2], "leads", sorted[2, 2], "by",
                              format(sorted[1,11] - sorted[2,11], big.mark=","),
                              "votes.\n", as.character(x$reported_at, "%I:%M %p"))
            title <- paste(x$contest, "-", x$ward_name)

            g <- chart +
            geom_bar(
              data=to_chart,
              mapping=aes(
                x=candidate_name,
                y=votes_received,
                fill=candidate_name
              ),
              stat="identity",
            ) +
            labs(
              title=title,
              subtitle=subtitle
            )
            g %+% to_chart
            filename <- paste(gsub(" ", "", x$ward_name), ".png", sep="")
            ggsave(filename, plot=g, device="png", path="charts/")
          })

  return(plots)
}

main <- function() {
  while(TRUE) {
    t1 <- Sys.time()
    plots <- chartResults(g)
    t2 <- difftime(Sys.time(), t1)
    if (t2 < 120) cat(paste("Sleeping for", round(120 - t2), "seconds\n")); Sys.sleep(120 - t2)
  }
}

main()
