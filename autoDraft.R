
# Author: Robert Botto
# Date: 2019-02-08

# functions
choosePlayers <- function(p, nt, pos, ex = c()) {
    return(sample(x = p[p$Position %in% pos & !(p$Id %in% ex) , 'Id'], 
                  size = nt, 
                  replace = FALSE,
                  prob = p[p$Position %in% pos & !(p$Id %in% ex) , 'Owned']))
}

printPlayers <- function(p, ids) {
    return(p[p$Id %in% ids, ])
}

formatPositionColumn <- function(c, p) {
    return(gsub(' \\()', '', 
           paste0(p[p$Id %in% c, 'Name'], ' (', 
                  p[p$Id %in% c, 'AAF.Team'], ')')))
}

formatPositionColumnD <- function(c, p) {
    return(p[p$Id %in% c, 'Name'])
}

formatPositionColumnB <- function(c, p) {
    return(paste0(p[p$Id %in% c, 'Name'], ', ',
                  p[p$Id %in% c, 'Position'],' (', 
                  p[p$Id %in% c, 'AAF.Team'], ')'))
}

# read in all players
players <- read.csv("players.csv")

# add id field
players$Id <- 1:nrow(players)

# remove players without teams(?)
players <- players[players$AAF.Team != '' | players$Position == 'DEFENSE', ]

# number of teams
number.teams <- 8

# labels from data set
qb.label <- 'QUARTERBACK'
rb.label <- 'RUNNING_BACK'
wr.label <- 'WIDE_RECEIVER'
te.label <- 'TIGHT_END'
k.label  <- 'KICKER'
d.label  <- 'DEFENSE'

# choose roster
set.seed(1549660786)
qb1 <- choosePlayers(players, number.teams, c(qb.label))
rb1 <- choosePlayers(players, number.teams, c(rb.label))
rb2 <- choosePlayers(players, number.teams, c(rb.label), rb1)
wr1 <- choosePlayers(players, number.teams, c(wr.label))
wr2 <- choosePlayers(players, number.teams, c(wr.label), wr1)
te1 <- choosePlayers(players, number.teams, c(te.label))
flex1 <- choosePlayers(players, number.teams, 
                       c(rb.label, wr.label, te.label), 
                       c(rb1, rb2, wr1, wr2, te1))
d1 <- choosePlayers(players, number.teams, c(d.label))
b1 <- choosePlayers(players, number.teams, 
                    c(qb.label, rb.label, wr.label, te.label, d.label), 
                    c(qb1, rb1, rb2, wr1, wr2, te1, flex1, d1))
b2 <- choosePlayers(players, number.teams, 
                    c(qb.label, rb.label, wr.label, te.label, d.label), 
                    c(qb1, rb1, rb2, wr1, wr2, te1, flex1, d1, b1))
b3 <- choosePlayers(players, number.teams, 
                    c(qb.label, rb.label, wr.label, te.label, d.label), 
                    c(qb1, rb1, rb2, wr1, wr2, te1, flex1, d1, b1, b2))
b4 <- choosePlayers(players, number.teams, 
                    c(qb.label, rb.label, wr.label, te.label, d.label), 
                    c(qb1, rb1, rb2, wr1, wr2, te1, flex1, d1, b1, b2, b3))

# assign teams
team <- c('ro@reloadbags.com',
          'gerik.reload.1@gmail.com',
          'loudstevesq@gmail.com',
          'Altay.akgun@gmail.com',
          'Christos.d.pappas@gmail.com',
          'fpgaffney@gmail.com',
          'annettemonnier@gmail.com',
          'robert.botto@gmail.com')

# randomly assemble benches from samples
draft <- data.frame('QB' = sample(qb1),
                    'RB1' = sample(rb1),
                    'RB2' = sample(rb2),
                    'WR1' = sample(wr1),
                    'WR2' = sample(wr2),
                    'TE' = sample(te1),
                    'D' = sample(d1),
                    'B1' = sample(b1),
                    'B2' = sample(b2),
                    'B3' = sample(b3),
                    'B4' = sample(b4))

# test if I fucked up and drafted anyone twice
any(duplicated(c(unlist(draft))))

# format and print it out
draft.print <- cbind('Team' = sample(team),
                     apply(draft[, 1:7], 2, formatPositionColumn, players),
                     apply(draft[, 8:11], 2, formatPositionColumnB, players)
)
write.csv(draft.print, "draft.csv", row.names = FALSE)

