library(ggplot2)
library(ivd)
library(coda)
library(ggdist)
library(ggridges)

obj <- readRDS("../../../melms_educ/WORK/-ANALYSIS/MODELS/saeb/out/outm1.rds")


.extract_to_mcmc <- function(obj) {
  e_to_mcmc <- lapply(obj$samples, FUN = function(x) mcmc(x$samples))
  return(e_to_mcmc)
}

.summary_table <- function(stats, Kr ) {
  ## Convert row names into a format that makes it easy to identify the rows to exclude
  rows_to_exclude <- c()
  for (row_name in rownames(stats)) {
    if (grepl("^R\\[", row_name)) {
      ## Extract the numeric indices from the row name
      elements <- as.numeric(unlist(regmatches(row_name, gregexpr("[0-9]+", row_name))))
      if (length(elements) == 2) {
        ## Exclude if row index is less than or equal to column index
        ## This removes both the diagonal and the upper triangular part
        if (elements[1] <= elements[2]) {
          rows_to_exclude <- c(rows_to_exclude, row_name)
        }
      }
    } else if (grepl("^ss\\[", row_name)) {
      ## Exclude the rows that pertain to location as SS is always 1
      row_number <- as.numeric(unlist(regmatches(row_name, gregexpr("[0-9]+", row_name)))[1])
      if (row_number <= Kr) {
        rows_to_exclude <- c(rows_to_exclude, row_name)
      }
    } else if (grepl("^sigma\\_rand\\[", row_name)) {
      ## Extract the numeric indices from the row name
      elements <- as.numeric(unlist(regmatches(row_name, gregexpr("[0-9]+", row_name))))
      if (length(elements) == 2) {
        ## Keep only diagonal elements by excluding if row index is not equal to column index
        if (elements[1] != elements[2]) {
          rows_to_exclude <- c(rows_to_exclude, row_name)
        }
      }
    } else if (grepl("^u\\[", row_name)) {
      rows_to_exclude <- c(rows_to_exclude,  row_name )
    }
  }
  ## Exclude rows
  stats_filtered <- stats[!rownames(stats) %in% rows_to_exclude, ]
  return(stats_filtered )
}

type = "pip"; pip_level = .75; variable = NULL; col_id = TRUE; legend = TRUE


# 1. Data prep ------------------------------------------------------------

ranef_scale_names <- colnames(obj$Z_scale)
fixef_scale_names <- colnames(obj$X_scale)

col_names <- dimnames(.summary_table(obj$samples[[1]]$samples ))[[2]]
Kr <- obj$nimble_constants$Kr

cols_to_keep <- c( )
## Find spike and slab variables
col_ss <- col_names[ grepl( "^ss\\[", col_names ) ]
for(col_name in col_ss) {
  col_number <- as.numeric(unlist(regmatches(col_name, gregexpr("[0-9]+", col_name)))[1])
  if(col_number >  Kr ) {
    cols_to_keep <- c(cols_to_keep,  col_name )
  }
}

## Subset each MCMC matrix to keep only the relevant columns
subsamples <- lapply(.extract_to_mcmc(obj), function(x) x[, cols_to_keep])

## Calculate column means for each subsetted MCMC matrix
means_list <- lapply(subsamples, colMeans)

## Average across the lists and chains
final_means <- Reduce("+", means_list) / length(means_list)

## assign the means to the specific random effects
ss_means <- list()
## Select the ss effect(s)
Sr <- obj$nimble_constants$Sr
for(i in 1:Sr ) {
  index <- paste0("\\[",  i+Kr)
  position_ss_value <- grepl(index, names(means_list[[1]]) )
  ss_means[[i]] <- final_means[position_ss_value]
}

## Get number of random scale effects:
no_ranef_s <- obj$nimble_constants$Sr

## Create a color vector for clusters with pip >= pip_level
set.seed(167779)
cluster_colors <- randomcoloR::distinctColorPalette(
  k = sum(ss_means[[1]] >= pip_level)
)


## With multiple random effects, ask user which one to be plotted:
if(no_ranef_s == 1) {  
  ## Define ordered dataset
  df_pip <- data.frame(id = seq_len(length(ss_means[[1]])),
                       pip = ss_means[[1]])
  df_pip <- df_pip[order(df_pip$pip), ]
  df_pip$ordered <- 1:nrow(df_pip)
} else if (no_ranef_s > 1 ) {
  if(is.null(variable)) {
    ## Prompt user for action when there are multiple random effects
    variable <- readline(prompt="There are multiple random effects. Please provide the variable name to be plotted or type 'list' \n(or specify as plot(fitted, type = 'funnel', variable = 'variable_name'): ")
    if (tolower(variable) == "list") {
      variable <- readline(prompt = cat(ranef_scale_names, ": "))
    }
  }
  
  ## Find position of user requested random effect
  scale_ranef_position_user <- which(ranef_scale_names == variable)
  
  ## Define ordered dataset
  df_pip <- data.frame(id = seq_len(length(ss_means[[scale_ranef_position_user]])),
                       pip = ss_means[[scale_ranef_position_user]])
  df_pip <- df_pip[order(df_pip$pip), ]
  df_pip$ordered <- 1:nrow(df_pip)
}


## find scale random effects
## Extract numbers and find locations
column_indices <- sapply(col_names, function(x) {
  if (grepl("^u\\[", x)) {  # Check if the name starts with 'u['
    ## Extracting numbers
    nums <- as.numeric(unlist(strsplit(gsub("[^0-9,]", "", x), ",")))
    ## Check if second number (column index) is greater than Kr
    return(nums[2] > Kr)
  } else {
    return(FALSE )
  }
})

## Indices of columns where column index is greater than Kr
scale_ranef_pos <- which(column_indices)

## Create tau locally
if(no_ranef_s == 1) {
  # Extract the posterior mean and 95% credible interval for each random effect:
  posterior_samples <- do.call(rbind, lapply(.extract_to_mcmc(obj), FUN = function(x) x[, scale_ranef_pos]))
  ## Extract the posterior mean of the fixed effect:
  zeta <- mean( unlist( lapply(.extract_to_mcmc( obj ), FUN = function(x) mean(x[, "zeta[1]"])) ) )
  ## Extract the posterior mean of each random effect:
  u <- colMeans(posterior_samples)
  # 95% credible intervals:
  u_median <- apply(posterior_samples, 2, function(x) quantile(x, probs = 0.5))
  u_lower <- apply(posterior_samples, 2, function(x) quantile(x, probs = 0.025))
  u_upper <- apply(posterior_samples, 2, function(x) quantile(x, probs = 0.975))
  
  
  
  # tau <- exp(zeta + u )
  # tau_lower <- exp(zeta + u_lower)
  # tau_upper <- exp(zeta + u_upper)
  
} else if (no_ranef_s > 1 ) {
  ## if(is.null(variable)) {
  ##   ## Prompt user for action when there are multiple random effects
  ##   variable <- readline(prompt="There are multiple random effects. Please provide the variable name to be plotted or type 'list' \n(or specify as plot(fitted, type = 'funnel', variable = 'variable_name'): ")
  ##   if (tolower(variable) == "list") {
  ##     variable <- readline(prompt = cat(ranef_scale_names, ": "))
  ##   }
  ## }
  
  ## Find position of user requested random effect
  scale_ranef_position_user <-
    which(ranef_scale_names == variable)
  
  ## Find position of user requested fixed effect
  ## TODO: When interactions are present plot will change according to moderator...
  ## Currently only main effect is selected
  scale_fixef_position_user <-
    which(fixef_scale_names == variable)
  
  ## Use ranef_position_user to select corresponding fixed effect
  zeta <- mean( unlist( lapply(.extract_to_mcmc(obj), FUN = function(x) mean(x[, paste0("zeta[", scale_fixef_position_user, "]")])) ) )
  
  ## Extract the posterior mean of each random effect:        
  pos <- scale_ranef_pos[ grepl( paste0(Kr + scale_ranef_position_user, "\\]"),  names(scale_ranef_pos ) ) ]
  
  u <-
    colMeans(do.call(rbind, lapply(.extract_to_mcmc(obj ), FUN = function(x) colMeans(x[, pos]))))
  # tau <- exp(zeta + u )
  
} else {
  print("Invalid action specified. Exiting.")
}

## Get mu's across chains
mu_combined <- lapply(obj$samples, function(chain) {
  mu_indices <- grep("mu", colnames(chain$samples))
  mu_samples <- chain$samples[, mu_indices, drop = FALSE]
  return(mu_samples)
})

## Get tau's across chains
tau_combined <- lapply(obj$samples, function(chain) {
  tau_indices <- grep("tau", colnames(chain$samples))
  tau_samples <- chain$samples[, tau_indices, drop = FALSE]
  return(tau_samples)
})

# Combine chains into one large matrix
# tau_matrix <- colMeans(do.call(rbind, tau_combined))

# Compute the posterior means
posterior_tau_means <- colMeans(do.call(rbind, tau_combined))
posterior_mu_means <- colMeans(do.call(rbind, mu_combined))

tau <- tapply(posterior_tau_means, obj$Y$group_id, mean)
mu <- tapply(posterior_mu_means, obj$Y$group_id, mean)

## Add tau to data frame -- ensure correct order
df_pip <-
  cbind(df_pip[order(df_pip$id), ], tau )

df_pip <-
  cbind(df_pip[order(df_pip$id), ], mu )
## Make nudge scale dependent:
## (not used)
ord <- (max(df_pip$ordered ) - min(df_pip$ordered ))/50
nx <- (max(df_pip$tau ) - min(df_pip$tau ))/50


# 2. PIP ------------------------------------------------------------------

plt_pip <- ggplot(df_pip, aes(x = ordered, y = pip)) +
  geom_point(data = subset(df_pip, pip < pip_level), 
             alpha = .6, 
             size = 4, 
             stroke = 1, 
             shape = 21, 
             fill = "grey40", 
             color = "black") +
  geom_jitter(data = subset(df_pip, pip >= pip_level),
              aes(fill = factor(id)), 
              size = 4, 
              stroke = 1, 
              shape = 21, 
              color = "black") +
  scale_fill_manual("ID", values = cluster_colors) +
  geom_abline(intercept = pip_level, slope = 0, lty =  3)+
  ylim(c(0, 1 ) ) + 
  labs(x = "Ordered index",
       y = "Posterior Inclusion Probability",
       title = "Scale Random Intercept") +
  theme_ridges() +
  theme(
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  )
print(plt_pip )

saveRDS(plt_pip, "final_project/plots/pip.rds")

# 3. Funnel ---------------------------------------------------------------

# ## Add tau to data frame -- ensure correct order
# df_funnel <-
#   cbind(df_pip[order(df_pip$id), ], tau )



plt_funnel <- ggplot(df_pip, aes(x = tau, y = pip)) +
  geom_point(data = subset(df_pip, pip < pip_level), 
             alpha = .3, size = 4, stroke = 1, shape = 21, fill = "grey40", color = "black") +
  geom_jitter(data = subset(df_pip, pip >= pip_level),
             aes(fill = factor(id)), size = 4, shape = 21,  position = "jitter") + 
  labs(x = "Within-Cluster SD") +
  geom_text(data = subset(df_pip, pip >= pip_level),
            aes(label = id),
            nudge_x = -nx,
            vjust = -0.5,
            #nudge_x = -.005,
            size = 4)+
  geom_abline(intercept = pip_level, slope = 0, lty =  3) +
  ylim(c(0, 1 ) ) +
  scale_fill_manual("ID", values = cluster_colors) +
  labs(x = "Within-cluster SD",
       y = "Posterior Inclusion Probability",
       title = "Intercept") +
  guides(fill = "none") +
  annotate(
    "rect", xmin = quantile(df_pip$tau, .025), 
    xmax = quantile(df_pip$tau, .975), ymin = 0
    , ymax = 1, fill = "#9ECAE1", alpha = .3
  )+
  annotate("label", label = "95% CrI", 
           color = "white", 
           fill = "#9ECAF0", 
           x = quantile(df_pip$tau, .975) -sd(df_pip$tau),
           size = 6, fontface = "bold",
           y =0.1, hjust = 0) + 
  theme_ridges() +
  theme(
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  )
print( plt_funnel )

saveRDS(plt_funnel, "final_project/plots/funnel.rds")

# 4. Outcome --------------------------------------------------------------

plt_y <- ggplot(df_pip, aes(x = mu, y = pip)) +
  geom_point(data = subset(df_pip, pip < pip_level), 
             alpha = .3, stroke = 1, aes(size=tau),
             shape = 21, fill = "grey40", color = "black") +
  geom_point(data = subset(df_pip, pip >= pip_level),
             aes(fill = factor(id), size = tau),  shape = 21, color = "black") +
  geom_text(data = subset(df_pip, pip >= pip_level),
            aes(label = id),
            nudge_x = -.05,
            vjust = -0.5,
            size = 4)+
  geom_abline(intercept = pip_level, slope = 0, lty =  3)+
  ylim(c(0, 1 ) ) + 
  scale_fill_manual("ID", values = cluster_colors) +
  labs(x = "Cluster mean",
       y = "Posterior Inclusion Probability",
       title = variable) +
  guides(fill = "none", 
         size = "none") +
  theme_ridges() +
  theme(
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  )
print(plt_y )
saveRDS(plt_y, "final_project/plots/outcome.rds")

# 5. Halfeye plot ----------------------------------------------------

posterior_long <- stack(as.data.frame(posterior_samples))
posterior_long$ind <- as.numeric(sub(".*\\[(\\d+),.*", "\\1", posterior_long$ind))
subdata <- subset(posterior_long, ind %in% which(df_pip$pip >= pip_level))

pip_order <- df_pip[df_pip$pip >= pip_level, c("id", "pip")]

ggplot(subdata) +
  aes( y = reorder(factor(ind), values, median),
       x = values,
       fill = factor(ind),
       color = factor(ind)
       )+
  scale_fill_manual(values = cluster_colors) +
  stat_slab(
    height = 5,
    alpha = .7,
    expand = FALSE, trim = FALSE, density = "unbounded",
    ill_type = "gradient",
    show.legend = FALSE
  ) +
  stat_pointinterval()+
  stat_pointinterval(
    geom = "label",
    aes(label = factor(ind)),
    .width = c(0.66, 0.95),
    #position = position_dodge(width = 1),
    size = 3,
    color = "black",
    #position = position_dodge(width = .4, preserve = "single")
  )+
  scale_color_manual(values =cluster_colors) +
  scale_y_discrete(breaks = NULL) +
  guides(fill = "none",
         color = "none")+
  theme_light()

## ggridges

plt_u <- ggplot(subdata) +
  aes( y = reorder(factor(ind), values, median),
       x = values,
       fill = factor(ind),
       color = factor(ind)
  )+
  scale_fill_manual(values =cluster_colors, guide = "none") +
  scale_color_manual(values =cluster_colors, guide = "none") +
  geom_density_ridges(alpha = .7, scale =3,rel_min_height = 0.0005,
                      linewidth = 1
                      ) +
  stat_pointinterval()+
  stat_pointinterval(
    geom = "label",
    aes(label = factor(ind)),
    .width = c(0.66, 0.95),
    size = 3,
    color = "black",
  )+
  scale_y_discrete(expand = c(0, 0), name= NULL) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0), name= "Random effect estimate") +   # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges() +
  theme(
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5),
    axis.text.y = element_blank()
  )
plt_u

saveRDS(plt_u, "final_project/plots/ranef.rds")

# 6. Within SD ------------------------------------------------------------

group_id <- obj$Y$group_id

# Function to compute mean within school clusters for one matrix
aggregate_by_school <- function(mat, group_id) {
  t(sapply(split(seq_along(group_id), group_id), function(cols) rowMeans(mat[, cols, drop = FALSE])))
}

# Apply this function to each chain separately
tau_reduced <- lapply(tau_combined, aggregate_by_school, group_id = group_id)

# Convert each element in the list to a matrix (for consistency)
tau_reduced <- lapply(tau_reduced, as.matrix)

# Check the structure of the reduced data
str(tau_reduced)

# Transpose each matrix (to make schools the columns)
tau_transposed <- lapply(tau_reduced, t)

# Stack chains row-wise
tau_matrix <- do.call(rbind, tau_transposed)

# Check structure
str(tau_matrix)


tau_long <- stack(as.data.frame(tau_matrix))

sub_tau <- subset(tau_long, ind %in% which(df_pip$pip >= pip_level))

## Plot

ggplot(sub_tau) +
  aes( y = reorder(factor(ind), values, median),
       x = values,
       fill = factor(ind),
       color = factor(ind)
       )+
  scale_fill_manual(values = cluster_colors) +
  stat_slab(
    height = 3,
    alpha = .7,
    expand = FALSE, trim = FALSE, density = "unbounded",
    show.legend = FALSE
  ) +
  stat_pointinterval()+
  stat_pointinterval(
    geom = "label",
    aes(label = factor(ind)),
    .width = c(0.66, 0.95),
    #position = position_dodge(width = 1),
    size = 3,
    color = "black",
    #position = position_dodge(width = .4, preserve = "single")
  )+
  scale_color_manual(values =cluster_colors) +
  scale_y_discrete(breaks = NULL) +
  guides(fill = "none",
         color = "none") +
  labs(x = "Within-cluster SD posterior distribution",
       y = NULL,
       title = variable) +
  theme_light()

## ggridges

plt_sigma <- ggplot(sub_tau) +
  aes( y = reorder(factor(ind), values, median),
       x = values,
       fill = factor(ind),
       color = factor(ind)
  )+
  #stat_pointinterval()+
 
  scale_fill_manual(values =cluster_colors, guide = "none") +
  scale_color_manual(values =cluster_colors, guide = "none") +
  geom_density_ridges(alpha = .7, scale =2.5,rel_min_height = 0.005,
                      linewidth = 1 ) +
  stat_pointinterval()+
  stat_pointinterval(
    geom = "label",
    aes(label = factor(ind)),
    .width = c(0.66, 0.95),
    size = 3,
    color = "black",
  )+
  scale_y_discrete(expand = c(0, 0), name= NULL) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0), name= "Within-cluster SD") +   # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges() +
  theme(
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5),
    axis.text.y = element_blank()
  )
plt_sigma
saveRDS(plt_sigma, "final_project/plots/sigma.rds")
