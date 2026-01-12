################################################################################
#                                                                              #
#                         CONWAY'S GAME OF LIFE - DATA                         #
#                                PATTERN LIBRARY                               #
#                                                                              #
################################################################################

### 1. Patterns ----
# Still lifes ----
# Block
block <- matrix(
  c(1,1,
    1,1),
  nrow = 2,
  byrow = TRUE
)

# Bee-hive
beehive <- matrix(
  c(0,1,1,0,
    1,0,0,1,
    0,1,1,0),
  nrow = 3,
  byrow = TRUE
)

# Loaf
loaf <- matrix(
  c(0,1,1,0,
    1,0,0,1,
    0,1,0,1,
    0,0,1,0),
  nrow = 4,
  byrow = TRUE
)

# Boat
boat <- matrix(
  c(1,1,0,
    1,0,1,
    0,1,0),
  nrow = 3,
  byrow = TRUE
)

# Tub
tub <- matrix(
  c(0,1,0,
    1,0,1,
    0,1,0),
  nrow = 3,
  byrow = TRUE
)

# Oscillators ----
# Blinker
blinker <- matrix(
  c(1,1,1),
  nrow = 1,
  byrow = TRUE
)

# Toad
toad <- matrix(
  c(0,1,1,1,
    1,1,1,0),
  nrow = 2,
  byrow = TRUE
)

# Beacon
beacon <- matrix(
  c(1,1,0,0,
    1,1,0,0,
    0,0,1,1,
    0,0,1,1),
  nrow = 4,
  byrow = TRUE
)

# Pulsar
pulsar <- matrix(
  c(0,0,1,1,1,0,0,0,1,1,1,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,0,0,0,0,1,0,1,0,0,0,0,1,
    1,0,0,0,0,1,0,1,0,0,0,0,1,
    1,0,0,0,0,1,0,1,0,0,0,0,1,
    0,0,1,1,1,0,0,0,1,1,1,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,1,1,1,0,0,0,1,1,1,0,0,
    1,0,0,0,0,1,0,1,0,0,0,0,1,
    1,0,0,0,0,1,0,1,0,0,0,0,1,
    1,0,0,0,0,1,0,1,0,0,0,0,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,1,1,1,0,0,0,1,1,1,0,0),
  nrow = 13,
  byrow = TRUE
)

# Penta-decathlon
pentadecathlon <- matrix(
  c(0,0,1,0,0,0,0,1,0,0,
    1,1,0,1,1,1,1,0,1,1,
    0,0,1,0,0,0,0,1,0,0),
  nrow = 3,
  byrow = TRUE
)

# Spaceships ----
# Glider
glider <- matrix(
  c(0,1,0,
    0,0,1,
    1,1,1),
  nrow = 3,
  byrow = TRUE
)

# Light-weight spaceship
LWSS <- matrix(
  c(0,1,0,0,1,
    1,0,0,0,0,
    1,0,0,0,1,
    1,1,1,1,0),
  nrow = 4,
  byrow = TRUE
)

# Middle-weight spaceship
MWSS <- matrix(
  c(0,0,0,1,0,0,
    0,1,0,0,0,1,
    1,0,0,0,0,0,
    1,0,0,0,0,1,
    1,1,1,1,1,0),
  nrow = 5,
  byrow = TRUE
)

# Heavy-weight spaceship
HWSS <- matrix(
  c(0,0,0,1,1,0,0,
    0,1,0,0,0,0,1,
    1,0,0,0,0,0,0,
    1,0,0,0,0,0,1,
    1,1,1,1,1,1,0),
  nrow = 5,
  byrow = TRUE
)

# Guns ----
# Gosper glider gun
gosper_gun <- matrix(
  c(
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
    0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
    1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  ),
  nrow = 9,
  byrow = TRUE
)

patterns <- list(
  random = function(nrow, ncol, p) {
    matrix(rbinom(nrow*ncol, 1, p), nrow, ncol)
  },
  block = block,
  beehive = beehive,
  loaf = loaf,
  boat = boat,
  tub = tub,
  blinker = blinker,
  toad = toad,
  beacon = beacon,
  pulsar = pulsar,
  pentadecathlon = pentadecathlon,
  glider = glider,
  LWSS = LWSS,
  MWSS = MWSS,
  HWSS = HWSS,
  gosper_gun = gosper_gun
)

usethis::use_data(patterns, internal = TRUE, overwrite = TRUE)
