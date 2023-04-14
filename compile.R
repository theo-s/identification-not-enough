library(ggplot2)

all_files <- list.files(path = "./res", full.names = TRUE)

get_res <- function(file) {
  temp_res <- readRDS(file)
  cbind(
    data.frame(n = temp_res$n, DGP = temp_res$DGP, nrounds = temp_res$nrounds),
    as.data.frame(t(temp_res$rmse))
  )
}

res <- do.call(rbind, lapply(all_files, get_res))
basevars <- c("n", "DGP", "nrounds")
vars <- setdiff(names(res), basevars)
res <- do.call(rbind, lapply(vars, \(var) { cbind(res[basevars], est = var, rmse = res[[var]]) }))

res$rmse[res$DGP == 2L & res$est == "glm"] <- res$rmse[res$DGP == 2L & res$est == "glm"] + 0.005
res$rmse[res$DGP == 2L & res$est == "dr_glm"] <- res$rmse[res$DGP == 2L & res$est == "dr_glm"] - 0.005

res$rmse[res$DGP == 3L & res$est == "glm"] <- res$rmse[res$DGP == 3L & res$est == "glm"] + 0.06
res$rmse[res$DGP == 3L & res$est == "loess"] <- res$rmse[res$DGP == 3L & res$est == "loess"] + 0.02
res$rmse[res$DGP == 3L & res$est == "dr_glm"] <- res$rmse[res$DGP == 3L & res$est == "dr_glm"] - 0.015
res$rmse[res$DGP == 3L & res$est == "dr_loess"] <- res$rmse[res$DGP == 3L & res$est == "dr_loess"] - 0.05

res$DGP <- paste("Data generating process", res$DGP)

res <- subset(res, est != "aht_glm")

palette <- c(glm = "#377eb8",
             loess = "#e41a1c",
             dr_glm = "#377eb8",
             dr_loess = "#e41a1c",
             ht = "#4daf4a",
             aht_glm = "#4daf4a",
             aht_loess = "#e41a1c")

shapes <- c(glm = 21,
            loess = 24,
            dr_glm = 21,
            dr_loess = 24,
            ht = 22,
            aht_glm = 22,
            aht_loess = 24)

linetype <- c(glm = "dotted",
              loess = "dotted",
              dr_glm = "dashed",
              dr_loess = "dashed",
              ht = "solid",
              aht_glm = "solid",
              aht_loess = "solid")

legend_title <- "Estimators"

legend_order <- c("glm",
                  "loess",
                  "dr_glm",
                  "dr_loess",
                  "ht",
                  "aht_glm",
                  "aht_loess")

legend_labels <- c(glm = "LOG",
                   loess = "LOESS",
                   dr_glm = "DRLOG",
                   dr_loess = "DRLOESS",
                   ht = "HT",
                   aht_glm = "HLOG",
                   aht_loess = "HTLOESS")


ggplot(res, aes(n, rmse, color = est, shape = est, linetype = est)) +
  geom_point() + geom_line() + scale_y_log10() +
  scale_x_log10(breaks = c(1e3, 1e4, 1e5), labels = c(expression(10^3),expression(10^4),expression(10^5))) +
  facet_wrap(~ DGP) +
  theme_bw() + theme(legend.position = "bottom", legend.key.width = unit(2.5, "line")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(y = "Root mean square error (log)", x = "Sample size (log)") +
  scale_color_manual(values = palette, breaks = legend_order, labels = legend_labels, name = "Estimators") +
  scale_shape_manual(values = shapes, breaks = legend_order, labels = legend_labels, name = "Estimators") +
  scale_linetype_manual(values = linetype, breaks = legend_order, labels = legend_labels, name = "Estimators")

ggsave("figures/ine-rmse.pdf", width = 10, height = 4)
