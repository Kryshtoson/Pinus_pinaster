library(psych)
library(tidyverse)
library(vegan)
library(twinspan)
data(varespec)
`%!in%` <- Negate(`%in%`)

# vegan data
varspe <- spe
cl <- head$twin2

# todo phi coefficient

##### Simple non-formated synoptic table

syn_table <- function(varspe, cl) {
  for (i in sort(unique(cl))) { # cluster-wise iterations
    A <- colSums((varspe == 0) * (cl != i)) # ABSENT OUT
    B <- colSums((varspe == 0) * (cl == i)) # ABSENT IN
    C <- colSums((varspe != 0) * (cl != i)) # PRESENT OUT
    D <- colSums((varspe != 0) * (cl == i)) # PRESENT IN
    phi_iter <- (((A * D) - (B * C)) / sqrt((A + B) * (C + D) * (A + C) * (B + D)))

    if (i == sort(unique(cl))[1]) {
      phi_fin <- phi_iter
    }
    else {
      phi_fin <- cbind(phi_fin, phi_iter)
    }
  }

  colnames(phi_fin) <- sort(unique(cl))
  twintable <- mutate_if(as_tibble(rownames_to_column(data.frame(phi_fin), 'species')),
                         is.numeric,
                         function(x) {
                           x = replace_na(x, 0)
                           x[x < 0] <- 0
                           x
                         }
  )
}

#### Sorting the synoptic table and adding freq info

th <- .2
dx <- syn_table(varspe, cl)

sort_table <- function(dx, varspe, cl, th = .2) {
  first_part <- pivot_longer(dx, -1) %>%
    split(.$name) %>%
    map(~.x %>%
      arrange(-value) %>%
      filter(value > th)) %>%
    bind_rows() %>%
    select(species) %>%
    pull()
  first_part <- first_part[!duplicated(first_part)]
  order <- c(first_part, dx$species[dx$species %!in% first_part])

  z <- split(data.frame(varspe != 0), cl)

  freq <- bind_cols(
    species = names(varspe),
    bind_cols(split(data.frame(varspe != 0), cl) %>%
                map(function(x) { round((colSums(x) / nrow(x)) * 100) }))) %>%
    mutate(species = factor(species, levels = order)) %>%
    arrange(species)

  synt <- dx %>%
    mutate(species = factor(species, levels = order)) %>%
    arrange(species) %>%
    `colnames<-`(names(freq))

  for (i in names(freq)[-1]) {
    synt[[i]] <- paste0(freq[[i]], '% (', round(synt[[i]] * 100, 0), ')')
  }
  synt
}

syn_sortable <- function(varspe, cl, th = .2) {
  sort_table(syn_table(varspe, cl), varspe, cl, th)
}


syn_sortable(varspe, cl, th = .5)