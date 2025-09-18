#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = current_LCU

current_LCU_2_current_xCU <- function(gdp, iso3c_y, source) {
  gdp %>%
    current_LCU_2_current_USMER(source) %>e%
    current_USMER_2_current_xCU(iso3c_y, source)
}

# Convert from current LCU to constant Int$PPP base y
current_LCU_2_constant_IntPPP_base_y <- function(gdp, base_y, source) {
  gdp %>%
    current_LCU_2_constant_LCU_base_y(base_y, source) %>e%
    constant_LCU_2_constant_IntPPP(base_y, source)
}

# Convert from current LCU to constant xCU base y
current_LCU_2_constant_xCU_base_y <- function(gdp, iso3c_y, base_y, source) {
  gdp %>%
    current_LCU_2_constant_LCU_base_y(base_y, source) %>e%
    constant_LCU_2_constant_USMER(base = base_y, source) %>e%
    constant_USMER_2_constant_xCU(iso3c_unit = iso3c_y, base = base_y, source)
}

#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = current_IntPPP

# Convert from current Int$PPP to current US$MER
current_IntPPP_2_current_USMER <- function(gdp, source) {
  gdp %>%
    current_IntPPP_2_current_LCU(source) %>e%
    current_LCU_2_current_USMER(source)
}

current_IntPPP_2_current_xCU <- function(gdp, iso3c_y, source) {
  gdp %>%
    current_IntPPP_2_current_LCU(source) %>e%
    current_LCU_2_current_USMER(source) %>e%
    current_USMER_2_current_xCU(iso3c_y, source)
}

# Convert from current Int$PPP to constant LCU base y
current_IntPPP_2_constant_LCU_base_y <- function(gdp, base_y, source) {
  gdp %>%
    current_IntPPP_2_current_LCU(source) %>e%
    current_LCU_2_constant_LCU_base_y(base_y, source)
}

# Convert from current Int$PPP to constant Int$PPP base y
current_IntPPP_2_constant_IntPPP_base_y <- function(gdp, base_y, source) {
  gdp %>%
    current_IntPPP_2_constant_LCU_base_y(base_y, source) %>e%
    constant_LCU_2_constant_IntPPP(base = base_y, source)
}

# Convert from current Int$PPP to constant xCU base y
current_IntPPP_2_constant_xCU_base_y <- function(gdp, iso3c_y, base_y, source) {
  gdp %>%
    current_IntPPP_2_constant_LCU_base_y(base_y, source) %>e%
    constant_LCU_2_constant_USMER(base = base_y, source) %>e%
    constant_USMER_2_constant_xCU(iso3c_unit = iso3c_y, base = base_y, source)
}

#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = current_xCU

current_xCU_2_current_LCU <- function(gdp, iso3c_x, source) {
  gdp %>%
    current_xCU_2_current_USMER(iso3c_x, source) %>e%
    current_USMER_2_current_LCU(source)
}

current_xCU_2_current_xCU <- function(gdp, iso3c_x, iso3c_y, source) {
  gdp %>%
    current_xCU_2_current_USMER(iso3c_x, source) %>e%
    current_USMER_2_current_xCU(iso3c_y, source)
}

current_xCU_2_current_IntPPP <- function(gdp, iso3c_x, source) {
  gdp %>%
    current_xCU_2_current_USMER(iso3c_x, source) %>e%
    current_USMER_2_current_LCU(source) %>e%
    current_LCU_2_current_IntPPP(source)
}

current_xCU_2_constant_LCU_base_y <- function(gdp, iso3c_x, base_y, source) {
  gdp %>%
    current_xCU_2_current_USMER(iso3c_x, source) %>e%
    current_USMER_2_current_LCU(source) %>e%
    current_LCU_2_constant_LCU_base_y(base_y, source)
}

current_xCU_2_constant_IntPPP_base_y <- function(gdp, iso3c_x, base_y, source) {
  gdp %>%
    current_xCU_2_current_USMER(iso3c_x, source) %>e%
    current_USMER_2_current_LCU(source) %>e%
    current_LCU_2_constant_LCU_base_y(base_y, source) %>e%
    constant_LCU_2_constant_IntPPP(base = base_y, source)
}

# Convert from current xCU to constant xCU base y
current_xCU_2_constant_xCU_base_y <- function(gdp, iso3c_x, iso3c_y, base_y, source) {
  gdp %>%
    current_xCU_2_current_USMER(iso3c_x, source) %>e%
    current_USMER_2_current_LCU(source) %>e%
    current_LCU_2_constant_LCU_base_y(base_y, source) %>e%
    constant_LCU_2_constant_USMER(base = base_y, source) %>e%
    constant_USMER_2_constant_xCU(iso3c_unit = iso3c_y, base = base_y, source)
}

#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = constant_LCU

# Convert from constant LCU base x to current Int$PPP
constant_LCU_base_x_2_current_IntPPP <- function(gdp, base_x, source) {
  gdp %>%
    constant_LCU_base_x_2_current_LCU(base_x, source) %>e%
    current_LCU_2_current_IntPPP(source)
}

# Convert from constant LCU base x to current Int$PPP
constant_LCU_base_x_2_current_USMER <- function(gdp, base_x, source) {
  gdp %>%
    constant_LCU_base_x_2_current_LCU(base_x, source) %>e%
    current_LCU_2_current_USMER(source)
}

constant_LCU_base_x_2_current_xCU <- function(gdp, base_x, iso3c_y, source) {
  gdp %>%
    constant_LCU_base_x_2_current_LCU(base_x, source) %>e%
    current_LCU_2_current_USMER(source) %>e%
    current_USMER_2_current_xCU(iso3c_y, source)
}

# Convert from constant LCU in one base year to constant Int$PPP of another
constant_LCU_base_x_2_constant_IntPPP_base_y <- function(gdp, base_x, base_y, source) {
  gdp %>%
    constant_LCU_base_x_2_constant_LCU_base_y(base_x, base_y, source) %>e%
    constant_LCU_2_constant_IntPPP(base = base_y, source)
}

# Convert from constant LCU in one base year to constant xCU of another
constant_LCU_base_x_2_constant_xCU_base_y <- function(gdp, base_x, iso3c_y, base_y, source) {
  gdp %>%
    constant_LCU_base_x_2_constant_LCU_base_y(base_x, base_y, source) %>e%
    constant_LCU_2_constant_USMER(base = base_y, source) %>e%
    constant_USMER_2_constant_xCU(iso3c_unit = iso3c_y, base = base_y, source)
}

#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = constant_IntPPP

# Convert from constant Int$PPP base year x to current LCU
constant_IntPPP_base_x_2_current_LCU <- function(gdp, base_x, source) {
  gdp %>%
    constant_IntPPP_2_constant_LCU(base = base_x, source) %>e%
    constant_LCU_base_x_2_current_LCU(base_x, source)
}

# Convert from constant Int$PPP base year x to current xCU
constant_IntPPP_base_x_2_current_xCU <- function(gdp, base_x, iso3c_y, source) {
  gdp %>%
    constant_IntPPP_base_x_2_current_LCU(base_x, source) %>e%
    current_LCU_2_current_USMER(source) %>e%
    current_USMER_2_current_xCU(iso3c_y, source)
}

# Convert from constant Int$PPP base year x to current Int$PPP
constant_IntPPP_base_x_2_current_IntPPP <- function(gdp, base_x, source) {
  gdp %>%
    constant_IntPPP_base_x_2_current_LCU(base_x, source) %>e%
    current_LCU_2_current_IntPPP(source)
}

# Convert from constant Int$PPP in one base year to constant LCU of another
constant_IntPPP_base_x_2_constant_LCU_base_y <- function(gdp, base_x, base_y, source) {
  gdp %>%
    constant_IntPPP_2_constant_LCU(base = base_x, source) %>e%
    constant_LCU_base_x_2_constant_LCU_base_y(base_x, base_y, source)
}

# Convert constant Int$PPP series from one base year to another
constant_IntPPP_base_x_2_constant_IntPPP_base_y <- function(gdp, base_x, base_y, source) {
  gdp %>%
    constant_IntPPP_base_x_2_constant_LCU_base_y(base_x, base_y, source) %>e%
    constant_LCU_2_constant_IntPPP(base = base_y, source)
}

# Convert from constant Int$PPP in one base year to constant xCU of another
constant_IntPPP_base_x_2_constant_xCU_base_y <- function(gdp, base_x, iso3c_y, base_y, source) {
  gdp %>%
    constant_IntPPP_base_x_2_constant_LCU_base_y(base_x, base_y, source) %>e%
    constant_LCU_2_constant_USMER(base = base_y, source) %>e%
    constant_USMER_2_constant_xCU(iso3c_unit = iso3c_y, base = base_y, source)
}


#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = constant xCU
## Helpers, Unit_in = constant US$MER

# Convert from constant US$MER base year x to current LCU
constant_USMER_base_x_2_current_LCU <- function(gdp, base_x, source) {
  gdp %>%
    constant_USMER_2_constant_LCU(base = base_x, source) %>e%
    constant_LCU_base_x_2_current_LCU(base_x, source)
}

# Convert from constant US$MER in one base year to constant LCU of another
constant_USMER_base_x_2_constant_LCU_base_y <- function(gdp, base_x, base_y, source) {
  gdp %>%
    constant_USMER_2_constant_LCU(base = base_x, source) %>e%
    constant_LCU_base_x_2_constant_LCU_base_y(base_x, base_y, source)
}

# Convert constant US$MER series from one base year to another
constant_USMER_base_x_2_constant_USMER_base_y <- function(gdp, base_x, base_y, source) {
  if (base_x == base_y) {
    return(gdp)
  }
  gdp %>%
    constant_USMER_base_x_2_constant_LCU_base_y(base_x, base_y, source) %>e%
    constant_LCU_2_constant_USMER(base = base_y, source)
}

# Convert from constant xCU base year x to current LCU
constant_xCU_base_x_2_current_LCU <- function(gdp, iso3c_x, base_x, source) {
  gdp %>%
    constant_xCU_2_constant_USMER(iso3c_unit = iso3c_x, base = base_x, source) %>e%
    constant_USMER_base_x_2_current_LCU(base_x, source)
}

# Convert from constant xCU base year x to current Int$PPP
constant_xCU_base_x_2_current_IntPPP <- function(gdp, iso3c_x, base_x, source) {
  gdp %>%
    constant_xCU_2_constant_USMER(iso3c_unit = iso3c_x, base = base_x, source) %>e%
    constant_USMER_base_x_2_current_LCU(base_x, source) %>e%
    current_LCU_2_current_IntPPP(source)
}

# Convert from constant xCU base year x to current US$MER
constant_xCU_base_x_2_current_USMER <- function(gdp, iso3c_x, base_x, source) {
  gdp %>%
    constant_xCU_2_constant_USMER(iso3c_unit = iso3c_x, base = base_x, source) %>e%
    constant_USMER_base_x_2_current_LCU(base_x, source) %>e%
    current_LCU_2_current_USMER(source)
}

# Convert from constant xCU base year x to current US$MER
constant_xCU_base_x_2_current_xCU <- function(gdp, iso3c_x, base_x, iso3c_y, source) {
  gdp %>%
    constant_xCU_2_constant_USMER(iso3c_unit = iso3c_x, base = base_x, source) %>e%
    constant_USMER_base_x_2_current_LCU(base_x, source) %>e%
    current_LCU_2_current_USMER(source) %>e%
    current_USMER_2_current_xCU(iso3c_y, source)
}

# Convert from constant xCU in one base year to constant LCU of another
constant_xCU_base_x_2_constant_LCU_base_y <- function(gdp, iso3c_x, base_x, base_y, source) {
  gdp %>%
    constant_xCU_2_constant_USMER(iso3c_unit = iso3c_x, base = base_x, source) %>e%
    constant_USMER_base_x_2_constant_LCU_base_y(base_x, base_y, source)
}

# Convert from constant xCU in one base year to constant Int$PPP of another
constant_xCU_base_x_2_constant_IntPPP_base_y <- function(gdp, iso3c_x, base_x, base_y, source) {
  gdp %>%
    constant_xCU_2_constant_USMER(iso3c_unit = iso3c_x, base = base_x, source) %>e%
    constant_USMER_base_x_2_constant_LCU_base_y(base_x, base_y, source) %>e%
    constant_LCU_2_constant_IntPPP(base = base_y, source)
}

# Convert from constant xCU in one base year to constant xCU of another
constant_xCU_base_x_2_constant_xCU_base_y <- function(gdp, iso3c_x, base_x, iso3c_y, base_y, source) {
  gdp %>%
    constant_xCU_2_constant_USMER(iso3c_unit = iso3c_x, base = base_x, source) %>e%
    constant_USMER_base_x_2_constant_USMER_base_y(base_x, base_y, source) %>e%
    constant_USMER_2_constant_xCU(iso3c_unit = iso3c_y, base = base_y, source)
}
