suppressPackageStartupMessages(library(ropls))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(htmlwidgets))
suppressPackageStartupMessages(library(magrittr))

model_score_scatter <- function(model, ci = 0.95, df_group) {
    require(ropls)
    require(ggplot2)
    require(magrittr)
    
    tmp_palette <- c(
        "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#ffff99", 
        "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6"
    )
    if (model@typeC == "PCA" | model@typeC == "PLS-DA") {
        df_p <- model@scoreMN[, seq(2)]
    } else if (model@typeC == "OPLS-DA") {
        df_p <- cbind(
            model@scoreMN[, 1] %>% as.data.frame(), 
            model@orthoScoreMN[,1] %>% as.data.frame()
        )
    }
    df_p %<>% as.data.frame() %>% set_colnames(c("x", "y"))
    df_p$group <- 
        df_group[rownames(df_p), "subg"] %>% 
        factor(levels = unique(.))
    n <- nrow(df_p)
    hfn <- 2*(n-1)*(n^2-1)/(n^2*(n-2))*qf(ci, 2, (n-2))
    rv <- seq(0, 2*pi, length.out = 360)
    df_ell <- data.frame(  # ci
        x = sqrt(var(df_p$x)*hfn)*cos(rv), 
        y = sqrt(var(df_p$y)*hfn)*sin(rv)
    )
    if (model@typeC == "PCA" | model@typeC == "PLS-DA") {
        r2x <- model@modelDF[c("p1", "p2"), "R2X"]
    } else if (model@typeC == "OPLS-DA") {
        # TODO
    }
    r2x %<>% multiply_by(100) %>% sprintf("%2.02f%%", .)
    p <- 
        ggplot() + 
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) + 
        geom_polygon(data = df_ell, aes(x, y), color = 'black', fill = NA) + 
        # stat_ellipse(
        #     data = df_p, geom = 'polygon', level = ci, 
        #     aes(x, y, fill = group, color = group), alpha = I(0.1)
        # ) +
        # stat_ellipse(
        #     data = df_p, geom = 'blank', level = ci, 
        #     aes(-x, -y, fill = group)
        # ) + 
        geom_point(
            data = df_p, size = 3, aes(x, y, shape = group, color = group)
        ) +
        geom_blank(
            data = df_p, aes(-x, -y, shape = group, color = group)
        ) +
        scale_shape_manual(
            values = rep_len(
                c(16, 15, 17, 18), length(levels(df_p$group))
            )[sort(unique(as.numeric(df_p$group)))]
        ) +
        scale_color_manual(
            values = rep_len(
                tmp_palette, length(levels(df_p$group))
            )[sort(unique(as.numeric(df_p$group)))]
        ) +
        scale_fill_manual(
            values = rep_len(
                tmp_palette, length(levels(df_p$group))
            )[sort(unique(as.numeric(df_p$group)))]
        ) +
        theme_bw() + 
        theme(
            legend.title = element_blank(), 
            legend.key = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()
        ) + 
        labs(
            x = paste0("PC[1](", r2x[1], ")"), 
            y = paste0("PC[2](", r2x[2], ")")
        )
}
model_score_scatter_3d <- function(model, df_group) {
    require(plotly)
    require(htmlwidgets)
    require(magrittr)
    
    tmp_palette <- c(
        "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#ffff99", 
        "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6"
    )
    if (model@typeC == "PCA" | model@typeC == "PLS-DA") {
        df_data <- model@scoreMN[, seq(3)] %>% as.data.frame()
    } else if (model@typeC == "OPLS-DA") {
        # TODO
    }
    class_id <- 
        df_group[rownames(df_data), "subg"] %>% 
        factor(levels = unique(.))
    df_data %<>% 
        cbind(class_id, .) %>% 
        set_colnames(c("class_id", "PC1", "PC2", "PC3"))
    axis.x <- list(
        range = c(- max(abs(df_data$PC1)), max(abs(df_data$PC1)))
    )
    axis.y <- list(
        range = c(- max(abs(df_data$PC2)), max(abs(df_data$PC2)))
    )
    axis.z <- list(
        range = c(- max(abs(df_data$PC3)), max(abs(df_data$PC3)))
    )
    p <- 
        plot_ly(
            df_data, x = ~PC1, y = ~PC2, z = ~PC3, 
            type = "scatter3d", mode = "markers", 
            color = ~class_id, 
            colors = rep_len(
                tmp_palette, length.out = length(levels(class_id))
            ), 
            symbol = ~class_id, 
            symbols = rep_len(
                c(16, 15, 17, 18), length.out = length(levels(class_id))
            ), 
            text = ~rownames(df_data), 
            marker = list(size = 7, opacity = 0.8)
        ) %>% 
        layout(
            legend = list(bgcolor = "#E2E2E2", bordercolor = "#FFFFFF"), 
            scene = list(xaxis = axis.x, yaxis = axis.y, zaxis = axis.z)
        )
}
