## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(proporz)

# Define a custom dataset for this vignette
votes_matrix = matrix(
    c( 800, 2802, 4095,  0, 150,
      3900,  814, 3990, 20,  60,
      1400, 1302, 4305, 10,  80,
         0,    0,    0, 50,   0,
       610,  500, 1001, 40, 120),
    ncol = 5, byrow = TRUE,
    dimnames = list(
        party = c("A", "B", "C", "D", "E"),
        district = c("City 1", "City 2", "City 3", "Region 4", "Region 5")
    ))

district_seats = setNames(c(5, 5, 14, 1, 1), colnames(votes_matrix))

## ----weight_votes-------------------------------------------------------------
votes_matrix

(voters = weight_list_votes(votes_matrix, district_seats))

## ----standard-----------------------------------------------------------------
seats_biproporz_standard = biproporz(votes_matrix, district_seats)

# Number of seats per party
rowSums(seats_biproporz_standard)

## ----summary------------------------------------------------------------------
summary(seats_biproporz_standard)

# You can transpose the matrix
# summary(t(seats_biproporz_standard))

## ----standard_quorum----------------------------------------------------------
biproporz(votes_matrix, district_seats, quorum_any(total = 0.05))

## ----alternative_methods------------------------------------------------------
biproporz(votes_matrix, district_seats, method = list("adams", "round"))

## ----custom_rounding----------------------------------------------------------
custom_rounding_func = function(x) {
    stopifnot(all(x >= 0))
    lt0.7 = x < 0.7
    x[lt0.7] <- 0
    x[!lt0.7] <- ceil_at(x[!lt0.7], 0.5)
    x
}

# The function must work with a matrix
custom_rounding_func(matrix(c(0.5, 0.6, 1.5, 2.5), 2))

# Apply custom rounding function in lower apportionment
biproporz(votes_matrix, district_seats, 
          method = list("adams", custom_rounding_func))

## ----wto----------------------------------------------------------------------
try(biproporz(votes_matrix, district_seats, method = "wto"))

## ----wto_quorum---------------------------------------------------------------
biproporz(votes_matrix, district_seats, method = "wto",
          quorum = quorum_any(total = 0.01))

## ----wto_other_method---------------------------------------------------------
biproporz(votes_matrix, district_seats, method = list("adams", "wto"))

## ----wto_ties-----------------------------------------------------------------
(tied_votes = matrix(
    c(1000, 500, 150, 150), 2, 
    dimnames = list(party = c("X", "Y"), district = 1:2)))
tied_votes_seats = setNames(c(2,1), colnames(tied_votes))

try(biproporz(tied_votes, tied_votes_seats, method = "wto"))

## ----wto_tiebreak-------------------------------------------------------------
tied_districts = district_winner_matrix(tied_votes, tied_votes_seats)
set.seed(4)
for(d in seq_len(ncol(tied_votes))) {
    if(anyNA(tied_districts[,d])) {
        tied_parties = which(is.na(tied_districts[,d]))
        
        # break tie randomly
        tiebreak_winner = sample(tied_parties, 1)
        cat("party", names(tiebreak_winner), "wins district", d)
        
        # assuming the impact of a small vote difference on 
        # the overall result is negligible
        tied_votes[tiebreak_winner,d] <- tied_votes[tiebreak_winner,d]+1e-9
    }
}

biproporz(tied_votes, tied_votes_seats, method = "wto")

## ----absolute_wto_function----------------------------------------------------
biproporz_absolute_wto = function(votes_matrix, district_seats,
                                  quorum = NULL, use_list_votes = TRUE) {
    # 1) Identify unambiguous district winners
    # Note: This step could also happen after the quorum has been applied
    # (depending on the desired method implementation)
    district_winners = district_winner_matrix(votes_matrix, district_seats)
    district_winners[is.na(district_winners)] <- FALSE # Ignore ties

    # 2) Apply quorum if specified
    if(!is.null(quorum)) {
        votes_matrix <- apply_quorum(votes_matrix, quorum)
    }
    
    # 3) Assign party seats in upper apportionment
    ua = upper_apportionment(votes_matrix, district_seats, 
                             use_list_votes, method = "round")

    # 4.1) Assign seats to district winners without 
    # enough upper apportionment seats
    seats_without_ua = district_winners * 1
    seats_without_ua[rowSums(district_winners) <= ua$party, ] <- 0
    
    # 4.2) Biproportional apportionment for remaining seats
    # Build votes matrix, set votes for district winners 
    # without enough upper apportionment seats to zero
    biprop_votes_matrix = votes_matrix
    biprop_votes_matrix[seats_without_ua > 0] <- 0
    
    # Reduce the number of seats for districts that 
    # already had a "insufficient district winner" seat assigned
    non_biprop_distr = colSums(seats_without_ua) > 0
    biprop_district_seats = district_seats
    biprop_district_seats[non_biprop_distr] <- 
        biprop_district_seats[non_biprop_distr] - 1
    
    # Run biproporz
    seats_biproporz = biproporz(biprop_votes_matrix, biprop_district_seats, 
                                method = "wto")
    
    # Remove divisor attributes, as they're no longer 
    # meaningful for the combined distribution
    seats_biproporz <- as.matrix(seats_biproporz)
    
    # 5) Return final seat distribution,
    #    combining the two apportionments 
    return(seats_biproporz + seats_without_ua)
}

## ----absolute_wto-------------------------------------------------------------
seats_biproporz_absolute_wto = biproporz_absolute_wto(votes_matrix, district_seats)

# Show the difference to the standard apportionment
seats_biproporz_absolute_wto - seats_biproporz_standard

