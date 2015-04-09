rqcCycleAverageQualityPcaPlot <- function(rqcResultSet)
{
    fit <- rqcCycleAverageQualityPcaCalc(rqcResultSet)
    df <- data.frame(cycle=factor(1:nrow(fit$x)), fit$x)
    pc <- data.frame(filename=rownames(fit$rotation), fit$rotation)
    mult <- min(
        (max(df$PC2) - min(df$PC2)/(max(pc$PC2)-min(pc$PC2))),
        (max(df$PC1) - min(df$PC1)/(max(pc$PC1)-min(pc$PC1)))
    )
    pc <- transform(pc,
        v1 = .7 * mult * (get("PC1")),
        v2 = .7 * mult * (get("PC2"))
    )
    ggplot(df, aes_string(x="PC1", y="PC2")) + 
        geom_text(aes_string(label="cycle")) +
        geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2) + 
        geom_text(aes_string(x="v1", y="v2", label="filename"), vjust=1, color="red", pc) +
        geom_segment(aes_string(x=0, y=0, xend="v1", yend="v2"), 
            arrow=arrow(length=unit(0.2,"cm")), color="red", pc) +
        theme_bw()
}
