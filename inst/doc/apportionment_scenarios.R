## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----library------------------------------------------------------------------
library(proporz)
str(finland2019)

## ----prepare_votes_matrix-----------------------------------------------------
votes_matrix = pivot_to_matrix(finland2019$votes_df)
dim(votes_matrix)

# Let's look at all parties with at least 10k votes
knitr::kable(votes_matrix[rowSums(votes_matrix) > 10000,])

## ----prepare_district_seats---------------------------------------------------
district_seats = finland2019$district_seats_df$seats
names(district_seats) <- finland2019$district_seats_df$district_name
district_seats

## ----apply_proporz------------------------------------------------------------
apply_proporz = function(votes_matrix, district_seats, method, quorum = 0) {
    seats_matrix = votes_matrix
    seats_matrix[] <- NA
    
    # calculate proportional apportionment for each district (matrix column)
    for(district in names(district_seats)) {
        seats_matrix[,district] <- proporz(votes_matrix[,district],
                                           district_seats[district],
                                           quorum = quorum,
                                           method = method)
    }
    return(seats_matrix)
}

## -----------------------------------------------------------------------------
bydistrict_v0 = apply_proporz(votes_matrix, district_seats, "d'hondt")

bydistrict_v0[rowSums(bydistrict_v0) > 0,]

## -----------------------------------------------------------------------------
bydistrict_v1 = apply_proporz(votes_matrix, district_seats,
                                      method = "sainte-lague")

bydistrict_v2 = apply_proporz(votes_matrix, district_seats,
                                      method = "huntington-hill", 
                                      quorum = 0.03)

## ----compare------------------------------------------------------------------
df_bydistrict = data.frame(
    D.Hondt = rowSums(bydistrict_v0),
    Sainte.Lague = rowSums(bydistrict_v1),
    Huntington.Hill = rowSums(bydistrict_v2)
)

# sort table by D'Hondt seats
df_bydistrict <- df_bydistrict[order(df_bydistrict[[1]], decreasing = TRUE),] 

# print parties with at least one seat
knitr::kable(df_bydistrict[rowSums(df_bydistrict) > 0,])

## ----seat_vote_share----------------------------------------------------------
vote_shares = rowSums(votes_matrix)/sum(votes_matrix)

shares = data.frame(
    seats = rowSums(bydistrict_v0)/sum(district_seats),
    votes = vote_shares 
)
shares$difference <- shares$seats-shares$votes
shares <- round(shares, 4)

# Only look at parties with at least 0.5 % of votes
shares <- shares[shares$votes > 0.005,]
shares <- shares[order(shares$difference),]

shares

## ----biprop, results="hide"---------------------------------------------------
seats_biproportional = biproporz(votes_matrix, 
                                 district_seats, 
                                 use_list_votes = FALSE)

# show only parties with seats
seats_biproportional[rowSums(seats_biproportional) > 0,]

## ----biprop.knit, echo = F----------------------------------------------------
knitr::kable(seats_biproportional[rowSums(seats_biproportional) > 0,])

## ----compare_vote_seat_shares-------------------------------------------------
vote_shares = rowSums(votes_matrix)/sum(votes_matrix)
seat_shares = rowSums(seats_biproportional)/sum(seats_biproportional)

range(vote_shares - seat_shares)

## ----compare_matrices---------------------------------------------------------
seat_changes = seats_biproportional-bydistrict_v0

knitr::kable(seat_changes[rowSums(abs(seat_changes)) > 0,colSums(abs(seat_changes))>0])

## ----biprop_seats-------------------------------------------------------------
full_biproportional = biproporz(votes_matrix, 
                                district_seats = sum(district_seats),
                                use_list_votes = FALSE)

# party seat distribution has not changed
rowSums(full_biproportional) - rowSums(seats_biproportional)

# district seat distribution is different
colSums(full_biproportional) - colSums(seats_biproportional)

## ----uri----------------------------------------------------------------------
seats_old_system = apply_proporz(uri2020$votes_matrix, uri2020$seats_vector, "hagenbach-bischoff")

seats_new_system = biproporz(uri2020$votes_matrix, uri2020$seats_vector)

seats_new_system-seats_old_system

