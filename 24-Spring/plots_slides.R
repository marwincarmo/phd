
# Run model ---------------------------------------------------------------

library(ivd)

school_dat <- mlmRev::Hsb82

## Ensure that school id is a continuous vector
school_dat$schoolid <- NA
k <- 0
for( i in unique(school_dat$school) ) {
  k <- k+1
  school_dat[school_dat$school == i, "schoolid"] <- k
}

dat <- ivd:::prepare_data_for_nimble(data = school_dat,
                                     location_formula = mAch ~ ses * sector +(ses | schoolid),
                                     scale_formula =  ~ ses + (1 | schoolid) )

out <- ivd(location_formula = mAch ~ ses + sector + (ses | schoolid),
           scale_formula =  ~ ses + (1 | schoolid),
           data = school_dat,
           niter = 1000, nburnin = 1000)

# Prepare data to plot ----------------------------------------------------

col_names <- colnames(out$samples[[1]])
Kr <- out$nimble_constants$Kr

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
subsamples <- lapply(out$samples, function(x) x[, cols_to_keep])

## Calculate column means for each subsetted MCMC matrix
means_list <- lapply(subsamples, colMeans)

##  Aggregate these means across all chains
# This computes the mean of means for each column across all chains
final_means <- Reduce("+", means_list) / length(means_list)

df_pip <- data.frame(id = seq_len(length(final_means)),
                     pip = final_means)

df_pip <- df_pip[order(df_pip$pip), ]
df_pip$ordered <- 1:nrow(df_pip)

## Get number of random scale effects:
no_ranef_s <- out$nimble_constants$Sr

## find scale random effects
## Extract numbers and find locations
column_indices <- sapply(col_names, function(x) {
  if (grepl("^ss\\[", x)) {  # Check if the name starts with 'ss['
    ## Extracting numbers
    nums <- as.numeric(unlist(strsplit(gsub("[^0-9,]", "", x), ",")))
    ## Check if second number (column index) is greater than Kr
    return(nums[2] > Kr)
  } else {
    return(FALSE )
  }
})


# PIP ---------------------------------------------------------------------

plt <- ggplot(df_pip, aes(x = ordered, y = pip)) +
  geom_point( aes(color = as.factor(id)), size = 3) +
  geom_text(data = subset(df_pip, pip >= 0.75),
            aes(label = id),
            nudge_x = -10,
            size = 3) +
  geom_abline(intercept = 0.75, slope = 0, lty =  3)+
  geom_abline(intercept = 0.25, slope = 0, lty =  3)+
  ylim(c(0, 1 ) ) + 
  guides(color ="none") +
  theme_minimal() +
  xlab("Sorted Index") +
  ylab("Posterior Inclusion Probability")
plt
ggsave(filename= "pip.png", path = "24-Spring/img", width = 8, height = 6, dpi=300)

# V-plot ------------------------------------------------------------------

schools <- school_dat |> 
  dplyr::with_groups(schoolid, dplyr::summarise,
                     mean = mean(mAch, na.rm=TRUE),
                     sd = sd(mAch, na.rm=TRUE))

df_merged <- df_pip |> 
  tibble::rownames_to_column("ranef") |> 
  dplyr::left_join(out_summary, by = "ranef")

df_merged |> 
  ggplot(aes(x = SD, y = pip), color = as.factor(id )) + 
  geom_point( ) +  
  guides(color = FALSE) + 
  labs(x = "Within-School Variance")+
  geom_text(data = subset(df_pip, pip >= 0.75),
            aes(label = id),
            nudge_x = -0.015,
            size = 3)


# Means vs pip ------------------------------------------------------------


average_dataset <- df_pip |> 
  dplyr::left_join(schools, by = c("id" = "schoolid")) 

average_dataset |> 
  ggplot(aes(x = mean, y = pip)) +
  geom_point( aes(color = as.factor(id)), size = 3) +
  geom_text(data = subset(average_dataset, pip >= 0.75),
            aes(label = id),
            nudge_x = -.3,
            size = 3) +
  geom_abline(intercept = 0.75, slope = 0, lty =  3)+
  geom_abline(intercept = 0.25, slope = 0, lty =  3)+
  ylim(c(0, 1 ) ) + 
  guides(color ="none") +
  labs(x = "Math achievement",
       y = "Posterior Inclusion Probability") +
  theme_minimal()

ggsave(filename= "avg.png", path = "24-Spring/img", width = 8, height = 6, dpi=300)
