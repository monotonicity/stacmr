
make_adj_matrix <- function(data, data_list, 
                            col_within, col_between,
                            stats_df, partial) {
  dimname <- paste(make.names(stats_df$within), make.names(stats_df$between), 
             sep = ":")
  adj_mat <- matrix(0, nrow = length(dimname), ncol = length(dimname), 
                      dimnames = list(dimname, dimname))
  
  if (missing(partial)) {
    return(adj_mat)
  } 
  
  if (missing(col_within)) {
    col_within <- NULL
  }
  if (missing(col_between)) {
    col_between <- NULL
  }
  all_cols <- c(col_within, col_between)
  rel_data <- data[all_cols]
  n_cols <- vapply(droplevels(rel_data), 
               function(x) length(unique(x)), 0)
  levels_cols <- lapply(
    droplevels(rel_data), 
    function(x) if (inherits(x, "factor")) levels(x) else unique(x)
  )
  dims_adj <- expand.grid(levels_cols)
  
  if (!is.list(partial) && (partial[1] == "auto")) {
    partial <- as.list(rep("auto", length(all_cols)))
    names(partial) <- all_cols
  }
     
  if (is.list(partial)) {
    if (is.null(names(partial))) 
      stop("List of partial orders must be named!", call. = FALSE)
    for (i in seq_along(partial)) {
      if (!(names(partial)[i] %in% all_cols)) {
        warning("No column '", names(partial)[i], 
                "' in model and no corresponding partial order applied.", 
                call. = FALSE)
        next
      }
      if (length(partial[[i]]) > 1) {
        warning("Partial order for '", names(partial)[i], 
                "': Only first element of list used")
        partial[[i]] <- partial[[i]][1]
      }
      if (partial[[i]] == "auto") {
        if (!inherits(rel_data[[names(partial)[i]]], "factor")) {
          warning("None factor condition '",  names(partial)[i], 
                  "' uses alphabetic order for automatic partial order.", 
              call. = FALSE)
          rel_data[[i]] <- factor(rel_data[[names(partial)[i]]])
        }
        partial[[i]] <- paste(paste0("`", 
                                     levels(rel_data[[names(partial)[i]]]), 
                                     "`"), 
                              collapse = " < ")
      }
      parsed_order <- parse_partial_order(partial[[i]])
      for (j in seq_len(nrow(parsed_order))) {
        i1 <- which(dims_adj[[names(partial)[i]]] == parsed_order[j,1])
        i2 <- which(dims_adj[[names(partial)[i]]] == parsed_order[j,2])
        if (parsed_order[j,"operator"] == "<") {
          adj_mat[cbind(i1, i2)] <- 1
        }
        if (parsed_order[j,"operator"] == ">") {
          adj_mat[cbind(i2, i1)] <- 1
        }
        if (parsed_order[j,"operator"] == "=") {
          adj_mat[cbind(i1, i2)] <- 1
          adj_mat[cbind(i2, i1)] <- 1
        }
      }
    }
    return(adj_mat)
  } else {
    stop("partial argument not recognized.", call. = FALSE)
  }
}

parse_partial_order <- function(symbolic_order, 
                                operators = c("=", "<", ">")) {
  vars <- strsplit(symbolic_order, paste(operators, collapse = "|"))[[1]]
  vars <- parse(text = vars)
  
  m <- gregexpr(paste(operators, collapse = "|"), symbolic_order)
  ops <- regmatches(symbolic_order, m)[[1]]
  
  out <- matrix(NA_character_, 
                nrow = length(ops), 
                ncol = 3)
  colnames(out) <- c("var1", "var2", "operator")
  
  for (i in seq_len(nrow(out))) {
    out[i,1] <- as.character(vars[[i]])
    out[i,2] <- as.character(vars[[i+1]])
    out[i,3] <- ops[i]
  }
  return(out)
}

list2adj <- function(nodes, E=list()) {
  ## Converts a partial order model in list form to an adjacency matrix suitable for monotonic regression
  ##
  ## Args:
  ##   nodes: set of elements (usually 1:n)
  ##   E: List containing the partial order model
  ##
  ## Returns:
  ##   Adjacency matrix for monotonic regression
  # ********************************************************
  # Written 12 September 2016
  # based on matlab code cell2adj.m written by Luke Finlay
  # and original R code written by Wai Keen Vong
  # *********************************************************
  
  if (!is.list(E)) {E <- list(E)}
  
  n <- length(nodes)
  
  ## Initialize adjacency matrix with zeros
  adj <- matrix(0, nrow = n, ncol = n)
  
  ## Fill in adjacency matrix
  
  if (n > 1) {
    if (length(E) > 0) {
      for (i in 1:length(E)) {
        if (length(E[[i]]) >= 2) {
          u = E[[i]]
          for (j in 1:(length(u)-1)) {
            adj[u[j],u[j+1]]=1
          }
        }
      }
    }
  }
  return(adj)
}


adj2list <- function(adj) {
  ## Converts a partial order model from adjacency matrix to list form 
  ##
  ## Args:
  ##   adj: Adjacency matrix for monotonic regression
  ##
  ## Returns:
  ##   List containing the partial order model
  # ********************************************************
  # Written 12 September 2016
  # *********************************************************

  u = which(adj==1,arr.ind=T); n = nrow(u)
  E = NULL
  if (n > 0) {
    E = vector("list", n)
    for (i in 1:n) {
      E[[i]] = c(u[i,1],u[i,2])
    } 
  }
  return (E)
}

