require(tidyverse)
require(cowplot)
require(latex2exp)
require(glmnet)
require(see)
require(knitr)
require(kableExtra)
require(parallel)

prepare_data <- function (.data, columns) {
  dat_count <- .data %>%
    filter(Logged == 1) %>%
    group_by(CaseID) %>%
    count(name = 'NumLogged') %>%
    ungroup()

  dat_weight <- .data %>%
    filter(Logged == 1) %>%
    group_by(CaseID) %>%
    summarize(Pre_Weight = mean(Pre_Weight),
              Post_Weight = mean(Post_Weight),
              Fu_Weight = mean(Fu_Weight),
              NumLogged = n()) %>%
    ungroup() %>%
    mutate(WeightChange1 = -(Post_Weight - Pre_Weight) / Pre_Weight,
           WeightChange2 = -(Fu_Weight - Post_Weight) / Post_Weight,
           WeightChange3 = -(Fu_Weight - Pre_Weight) / Pre_Weight) %>%
    select(CaseID, WeightChange1, WeightChange2, WeightChange3)

  dat_cont <- .data %>%
    filter(Logged == 1) %>%
    select(CaseID, columns, -Logged)  # Kcal

  dat_mean <- NULL

  for (caseid in unique(dat_cont$CaseID)) {
    tmp_case <- dat_cont %>% filter(CaseID == caseid)
    tmp_mean <- colMeans(tmp_case, na.rm =TRUE)
    dat_mean <- rbind(dat_mean, tmp_mean)
  }

  dat_mean <- as_tibble(as.data.frame(dat_mean))

  dat_glm <-
    dat_count %>%
    left_join(dat_weight, by = 'CaseID') %>%
    left_join(dat_mean, by = 'CaseID') %>%
    filter(NumLogged >= 15) %>%
    select(-CaseID)

  return(as_tibble(as.data.frame(dat_glm)))
}

scale_df <- function (.data) {
  ret <- scale(.data)
  attr(ret, "scaled:center") <- NULL
  attr(ret, "scaled:scale") <- NULL
  return(as_tibble(as.data.frame(ret)))
}

run_fit <- function (dat_train, dat_test, columns, y_label,
                     alpha = 0.1, lambda_type = '1se') {

  f_glm <- as.formula(paste0(y_label, ' ~ ', str_c(columns, collapse = ' + ')))
  x_train <- as.matrix(dat_train[columns])
  y_train <- as.vector(as.matrix(dat_train[y_label]))
  x_test <- as.matrix(dat_test[columns])
  y_test <- as.vector(as.matrix(dat_test[y_label]))
  x_total <- rbind(x_train, x_test)
  y_total <- c(y_train, y_test)

  print(x_train)
  print(y_train)

  cvfit <- cv.glmnet(x = x_train, y = y_train, standardize = FALSE,
                     alpha = alpha, type.measure = 'mse', nfolds = 5)

  if (lambda_type == '1se') {
    lambda <- cvfit$lambda.1se
  } else {
    lambda <- cvfit$lambda.min
  }

  fit <- glmnet(x_train, y_train, alpha = alpha, lambda = lambda,
                standardize = FALSE)
  yhat_total <- predict(fit, s = lambda, newx = x_total)
  yhat_train <- predict(fit, s = lambda, newx = x_train)
  yhat_test <- predict(fit, s = lambda, newx = x_test)

  ret <-
    fit$beta %>%
    as.matrix() %>%
    t() %>%
    as.data.frame() %>%
    remove_rownames()

  calculate_r2 <- function(data, fitted) {
    1 - sum((data - fitted) ** 2) / sum((data - mean(data)) ** 2)
  }

  ret['alpha'] <- alpha
  ret['r2_total'] <- calculate_r2(y_total, yhat_total)
  ret['r2_train'] <- calculate_r2(y_train, yhat_train)
  ret['r2_test'] <- calculate_r2(y_test, yhat_test)
  ret['rmse_total'] <- sqrt(mean((y_total - yhat_total) ** 2))
  ret['rmse_train'] <- sqrt(mean((y_train - yhat_train) ** 2))
  ret['rmse_test'] <- sqrt(mean((y_test - yhat_test) ** 2))

  ret <- ret %>% select(alpha, rmse_train, rmse_test, everything())
  return(ret)
}

find_best_fit <- function(n_iter, n_core) {
  clust <- makeCluster(n_core)
  clusterExport(clust, "dat_glm")
  clusterExport(clust, "predictors")
  clusterExport(clust, "y_label")
  clusterExport(clust, "prepare_data")
  clusterExport(clust, "run_fit")

  fits <- parLapply(clust, rep(ALPHAS, n_iter), function(alpha) {
  # fits <- lapply(rep(ALPHAS, n_iter), function(alpha) {
    require(tidyverse)
    require(glmnet)

    dat_all <- dat_glm %>%
      filter(!is.na(!!as.name(y_label))) %>%
      as.data.frame() %>%
      as_tibble()
    dat_ret <- NULL

    n_data <- nrow(dat_all)

    for (i in 1:n_data) {
      # Leave-one-out Cross-validation (LOOCV)
      data_train <- dat_all[-i,]
      data_test <- dat_all[i,]

      dat_ret <- rbind(dat_ret,
        run_fit(data_train, data_test, predictors, y_label,
                alpha = alpha, lambda_type = 'min'))
    }

    dat_ret %>%
      as_tibble() %>%
      summarize_all(mean)
  }) %>% do.call(what = rbind) %>% as_tibble()

  stopCluster(clust)

  return(fits)
}

get_alpha_min_rmse <- function(res) {
  df_avg <- res %>% select(alpha, rmse_total, rmse_train, rmse_test) %>%
    group_by(alpha) %>%
    summarize(avg_total = mean(rmse_total),
              avg_train = mean(rmse_train),
              avg_test = mean(rmse_test))

  df_avg[which.min(df_avg$avg_total),]$alpha
}

plot_rmse_alpha_train <- function(res, title) {
  df_com <- res %>%
    select(alpha, rmse_train) %>%
    group_by(alpha) %>%
    summarize(avg = mean(rmse_train),
              sd = sd(rmse_train)) %>%
    ungroup() %>%
    mutate(ymax = avg + sd, ymin = avg - sd)

  x_min <- df_com[which.min(df_com$avg),]$alpha
  y_min <- df_com[which.min(df_com$avg),]$avg

  df_com %>%
    ggplot(aes(x = alpha, y = avg)) +
    geom_line() +
    geom_vline(xintercept = x_min, color = "red") +
    geom_errorbar(aes(ymax = avg + sd, ymin = avg - sd), width = 0, alpha = 0.5) +
    geom_point() +
    geom_point(aes(x = x_min, y = y_min), color = "red", size = 4, shape = 1) +
    labs(x = TeX('$\\alpha$'),
         y = 'Root Mean Squared Error',
         title = title,
         subtitle = TeX(str_glue("Training set ($\\alpha = {x_min}$)"))) +
    theme_bw() +
    theme(legend.position = 'none')
}

plot_rmse_alpha_test <- function(res, title) {
  df_com <- res %>%
    select(alpha, rmse_test) %>%
    group_by(alpha) %>%
    summarize(avg = mean(rmse_test),
              sd = sd(rmse_test)) %>%
    ungroup() %>%
    mutate(ymax = avg + sd, ymin = avg - sd)

  x_min <- df_com[which.min(df_com$avg),]$alpha
  y_min <- df_com[which.min(df_com$avg),]$avg

  df_com %>%
    ggplot(aes(x = alpha, y = avg)) +
    geom_line() +
    geom_vline(xintercept = x_min, color = "red") +
    geom_errorbar(aes(ymax = ymax, ymin = ymin), width = 0, alpha = 0.5) +
    geom_point() +
    geom_point(aes(x = x_min, y = y_min), color = "red", size = 4, shape = 1) +
    labs(x = TeX('$\\alpha$'),
         y = 'Root Mean Squared Error',
         title = title,
         subtitle = TeX(str_glue("Test set ($\\alpha = {x_min}$)"))) +
    theme_bw() +
    theme(legend.position = 'none')
}

plot_rmse_alpha <- function(res, title) {
  plot_grid(
    plot_rmse_alpha_train(res, title),
    plot_rmse_alpha_test(res, title),
    ncol = 2, align = "hv"
  )
}

plot_coef <- function(res, title, alpha) {
  df_alpha <- res %>%
    filter(alpha == alpha) %>%
    select(-alpha, -r2_train, -r2_test, -rmse_train, -rmse_test)

  df_avg <- df_alpha %>%
    summarize_all(list("_avg" = mean,
                       "_ymax" = function(col) { quantile(col, .025) },
                       "_ymin" = function(col) { quantile(col, .975) })) %>%
    gather(key, value, everything()) %>%
    separate(key, c("predictor", "stat"), "__") %>%
    spread(stat, value)

  df_std <- df_alpha %>%
    summarize_all(sd) %>%
    gather(predictor, sd, everything())

  df_com <- full_join(df_avg, df_std, by = 'predictor') %>%
    mutate(category = map(predictor, ~ map_col_to_cat[[.x]]) %>% flatten_chr(),
           predictor = factor(predictor, levels = rev(predictors),
                              labels = rev(str_replace(predictors, "^Pre_", ""))),
           category = factor(category, levels = categories),
           ymax = avg + 1.96 * sd,
           ymin = avg - 1.96 * sd,
           color = ifelse(ymax < 0, "neg", ifelse(ymin > 0, "pos", "notsig")),
           color = as.factor(color)) %>%
    arrange(category, predictor)

  p <- df_com %>%
    ggplot(aes(x = predictor, y = avg)) +
    geom_hline(yintercept = 0, alpha = 0.8, color = "#999999") +
    geom_errorbar(aes(ymax = ymax, ymin = ymin, color = color), width = 0) +
    geom_point(aes(color = color), stat = 'identity', size = 2) +
    labs(x = 'Predictors',
         y = 'Regression coefficient',
         title = title,
         caption = TeX(str_glue("* $\\alpha = {alpha}$"))) +
    scale_color_manual(values = c(neg = "red", notsig = "#666666", pos = "blue")) +
    facet_grid(category ~ ., switch = "y", scales = "free_y", space = "free_y") +
    coord_flip() +
    # theme_bw() +
    theme(legend.position = 'none',
          panel.spacing = unit(3, "pt"),
          strip.placement = "outside",
          strip.text.y = element_text(angle = 180, vjust = 0.5, hjust = 0.5, face = "bold"),
          strip.background = element_rect(color = NA),
          axis.title.y = element_blank())

  p
}

get_sig_predictors <- function(res, alpha) {
  df_alpha <- res %>%
    filter(alpha == alpha) %>%
    select(-alpha, -rmse_train, -rmse_test)

  df_avg <- df_alpha %>%
    summarize_all(mean) %>%
    gather(predictor, avg, everything())

  df_std <- df_alpha %>%
    summarize_all(sd) %>%
    gather(predictor, sd, everything())

  df_com <- full_join(df_avg, df_std, by = 'predictor') %>%
    mutate(predictor = factor(predictor, levels = predictor),
           ymax = avg + 1.96 * sd,
           ymin = avg - 1.96 * sd,
           color = ifelse(ymax < 0, "neg", ifelse(ymin > 0, "pos", "notsig")),
           color = as.factor(color))

  list(pos = df_com$predictor[df_com$color == "pos"],
       neg = df_com$predictor[df_com$color == "neg"])
}

plot_r2_train <- function(res, title, alpha) {
  df_com <- res %>%
    select(alpha, r2_train) %>%
    filter(alpha == alpha)

  avg = mean(df_com$r2_train)
  y025 = quantile(df_com$r2_train, .025)
  y975 = quantile(df_com$r2_train, .975)

  df_com %>%
    ggplot(aes(x = r2_train)) +
    geom_vline(xintercept = avg, color = "red", linetype = 'dashed') +
    geom_vline(xintercept = y025, color = "red", linetype = 'dotted') +
    geom_vline(xintercept = y975, color = "red", linetype = 'dotted') +
    geom_density() +
    labs(x = 'R squared',
         y = 'Density',
         title = title,
         subtitle = TeX(str_glue("Train set ($\\alpha = {alpha}$)"))) +
    theme_bw() +
    theme(legend.position = 'none')
}

plot_r2_test <- function(res, title, alpha) {
  df_com <- res %>%
    select(alpha, r2_test) %>%
    filter(alpha == alpha)

  avg = mean(df_com$r2_test)
  y025 = quantile(df_com$r2_test, .025)
  y975 = quantile(df_com$r2_test, .975)

  df_com %>%
    ggplot(aes(x = r2_test)) +
    geom_vline(xintercept = avg, color = "red", linetype = 'dashed') +
    geom_vline(xintercept = y025, color = "red", linetype = 'dotted') +
    geom_vline(xintercept = y975, color = "red", linetype = 'dotted') +
    geom_density() +
    labs(x = 'R squared',
         y = 'Density',
         title = title,
         subtitle = TeX(str_glue("Test set ($\\alpha = {alpha}$)"))) +
    theme_bw() +
    theme(legend.position = 'none')
}

plot_r2 <- function(res, title) {
  plot_grid(
    plot_r2_train(res, title),
    plot_r2_test(res, title),
    ncol = 2, align = "hv"
  )
}
