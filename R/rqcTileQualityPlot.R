rqcTileQualityPlot <- function(rqcResultSet)
{
    df <- perTileQuality(rqcResultSet)
    df$tile <- factor(df$tile)
    df$lane <- factor(df$lane)
    ggplot(df, aes_string(x="lane", y="tile", fill="average")) + 
        geom_tile() +
        facet_wrap(facets=~filename, ncol=2) +
        scale_fill_gradient(low = "#EF8A62", high = "#67A9CF", limits=c(0,41)) +
        scale_y_discrete(breaks=c(1, 60, 120)) +
        labs(x="Lane", y="Tile", fill="Mean")
}