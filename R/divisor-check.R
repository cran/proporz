# If last party with seat and first without a seat have the same matrix_quotient
# from highest_averages_method the result is undefined as both parties could get
# the last seat
check_edge_quotient = function(mtrx_quotient, n_seats, return_indices = FALSE) {
    ordered_quotients = order(mtrx_quotient, decreasing = TRUE)
    quotient_last_with = mtrx_quotient[ordered_quotients[n_seats]]
    quotient_first_without = mtrx_quotient[ordered_quotients[n_seats+1]]

    if(is.nan(quotient_first_without)) quotient_first_without <- 0

    if(quotient_last_with == quotient_first_without) {
        indices = unique(which(mtrx_quotient == quotient_last_with, arr.ind = TRUE)[,"col"])
        parties = collapse_names(indices)
        if(!is.null(colnames(mtrx_quotient))) {
            parties <- collapse_names(colnames(mtrx_quotient)[indices])
        }
        stop("Result is undefined, equal quotient for parties: ", parties, call. = FALSE)
    }
}

# methods that give every party at least one seat should also get one
check_enough_seats = function(votes, n_seats, method) {
    if(n_seats >= length(votes[votes > 0])) {
        return()
    }
    if(n_seats == 0) {
        # n_seats=0 is explicitly allowed to make modelling scenarios easier
        return()
    }
    stop("With ", method," rounding there must be at least as many seats as ",
         "there are parties with non-zero votes.", call. = FALSE)
}

check_seats_number = function(n_seats, n_seats.name) {
    if(length(n_seats) == 1 && !is.null(n_seats) && !is.na(n_seats) &&
       (n_seats %% 1 == 0) &&
       n_seats >= 0) {
        return()
    }
    stop("`", n_seats.name, "` must be an integer >= 0", call. = FALSE)
}

check_votes_vector = function(votes, votes.name) {
    if(is.numeric(votes) && all(!is.na(votes)) &&
       all(votes >= 0) && is.vector(votes)) {
        return()
    }
    stop("`", votes.name, "` must be a numeric vector >= 0", call. = FALSE)
}
