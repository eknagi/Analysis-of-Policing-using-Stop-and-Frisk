
############################
#Read in csvs from 2008-2016
############################
sqf.data.full <- foreach(year=2008:2016, .combine='rbind.fill', .packages="plyr") %dopar% {
  filename <- paste0( year, '.csv')
  this.data <- read_csv(filename, na = c("", "NA", "*", "**"),
                        col_types = cols(perobs = col_double(), 
                                         sumoffen = col_character(),
                                         inout = col_character(), 
                                         datestop = col_integer(), 
                                         timestop = col_integer(),
                                         offverb = col_character(),
                                         offshld = col_character(),
                                         arstoffn = col_character()))
    this.data <- this.data %>%
      `names<-`(tolower(names(.)))
    this.data
}

# duplicate data for cleaning
sqf.data <- sqf.data.full

############################
#     CLEANING
############################
#0)
sqf.data <- sqf.data %>% select(-recstat, -officrid, -sumoffen, -compyear, -comppct)

#1) timestamp
sqf.data <- sqf.data %>% mutate(datestop =  sprintf("%08d", as.integer(datestop)),
                                timestop = sprintf("%04d", as.integer(timestop)),
                                timestamp = mdy_hm(paste(datestop, timestop))) %>% select(-datestop, -timestop)

# 2) location information and serial number
sqf.data <- sqf.data %>% mutate(precinct = as.factor(pct), xcoord = as.integer(xcoord),
                                ycoord = as.integer(ycoord), serial = ser_num) %>% 
  select(-pct, -ser_num)

#3) FUNCTIONS
#-----------------------------------------------------------------------------
# functions to recode factor levels
recode.factor <- function(f, old.levels, new.levels) {
  f.new <- as.factor(f)
  level.map <- new.levels
  names(level.map) <- old.levels
  levels(f.new) <- level.map[levels(f.new)]
  f.new <- factor(f.new, levels = unique(new.levels))
  f.new
}

recode.yn <- function(f) {
  f.new <- factor(f, levels=c('N','Y'))
  f.new <- as.logical(as.integer(f.new) - 1)
  f.new
}

recode.yesno <- function(f) {
  f.new <- factor(f, levels=c('NO','YES'))
  f.new <- as.logical(as.integer(f.new) - 1)
  f.new
}

# this function is solely for the stop being inside or outside
recode.io <- function(f) {
  f.new <- factor(f, levels=c('O','I'))
  f.new <- as.logical(as.integer(f.new) - 1)
  f.new
}

# this function is solely for whether the officer provided a verbal statement when not in uniform
recode.0V <- function(f) {
  f.new <- factor(f, levels=c('0','V'))
  f.new <- as.logical(as.integer(f.new) - 1)
  f.new
}

# this function is solely for whether the officer provided a shield when not in uniform
recode.0S <- function(f) {
  f.new <- factor(f, levels=c('0','S'))
  f.new <- as.logical(as.integer(f.new) - 1)
  f.new
}

# convert offense codes to human-readable names
offense.codes <- read.delim('offense-codes.tsv', header=FALSE, col.names=c('code','offense'))
offense.codes$offense <- tolower(offense.codes$offense)
convert.offense.code <- function(codes) {
  offenses <- offense.codes$offense[as.integer(codes)]
  offenses <- factor(offenses, levels=offense.codes$offense)
  offenses
}

#combine and standardize top 100 reasons for arrest.
arrest.offenses <- read.delim('arrest.offenses.tsv', header=FALSE, col.names=c('real.offense','nominal.offense'))
arrest.offenses$real.offense <- trim(arrest.offenses$real.offense)
arrest.offenses$nominal.offense <- trim(arrest.offenses$nominal.offense)
convert.arrest.reasons <- function(rawlist) {
  ndx <- match(rawlist, arrest.offenses$nominal.offense)
  responses <- arrest.offenses$real.offense[ndx]
  ndx <- is.na(responses) & (rawlist!="")
  responses[ndx] <- "not.top.100"
  responses <- factor(responses)
  responses
}

nyc.proj = "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"

#-----------------------------------------------------------------------------------
#3) recode y/n variables

sqf.data <- sqf.data %>% mutate(frisked = recode.yn(frisked), 
                                searched = recode.yn(searched), 
                                extra.reports = recode.yn(adtlrept),
                                reason.explained = recode.yn(explnstp), 
                                others.stopped = recode.yn(othpers),
                                arrested = recode.yn(arstmade),
                                summons.issued = recode.yn(sumissue),
                                radio.run = recode.yn(radio)
) 

# 4) recode other binary variables and drop irrelevant variables
sqf.data <- sqf.data %>% mutate(inside = recode.io(inout), observation.period = perobs,
                                suspected.crime = convert.offense.code(detailcm),
                                officer.verbal = recode.0V(offverb),
                                officer.shield = recode.0S(offshld),
                                arrested.reason = convert.arrest.reasons(arstoffn)) %>% 
  select(-adtlrept, -explnstp, -othpers, -arstmade, -sumissue, -radio, -inout, 
         -perobs, -detailcm, -offverb, -offshld, -arstoffn, -forceuse)

#5) location: recode 'P' (for Pedestrian, which occurs mainly after 2008) and blank as 'neither'.
sqf.data <- sqf.data %>% mutate(location.housing = 
                                  recode.factor(sqf.data.full$trhsloc, c('P', 'H', 'T'), 
                                                c('neither', 'housing', 'transit')))
sqf.data <- sqf.data %>% 
  mutate(location.housing = replace(location.housing, is.na(location.housing), 'neither')) %>% 
  select(-trhsloc)

# period of stop (in minutes)
sqf.data <- sqf.data %>% mutate(stop.length = perstop) %>% select(-perstop)

# type of id and officer in uniform
sqf.data <- sqf.data %>% 
  mutate(identification = recode.factor(typeofid, c('O','P','R','V'),
                                        c('other', 'photo', 'refused', 'verbal')),
         officer.uniform = recode.factor(offunif, c('M', 'N', 'Y'),
                                         c('N', 'N', 'Y')),
         officer.uniform = recode.yn(officer.uniform)) %>% 
  select(-typeofid, -offunif)

# 6) physical force variables
sqf.data <- sqf.data %>% mutate(force.hands = recode.yn(pf_hands),
                                force.wall = recode.yn(pf_wall),
                                force.ground = recode.yn(pf_grnd),
                                force.drawn = recode.yn(pf_drwep),
                                force.pointed = recode.yn(pf_ptwep),
                                force.baton = recode.yn(pf_baton),
                                force.handcuffs = recode.yn(pf_hcuff),
                                force.pepper = recode.yn(pf_pepsp),
                                force.other = recode.yn(pf_other)
) %>% 
  select(-pf_hands, -pf_wall, -pf_grnd, -pf_drwep, -pf_ptwep, -pf_baton, -pf_hcuff,
         -pf_pepsp, -pf_other)

# 7) primary circumstances of stop
sqf.data <- sqf.data %>% mutate(stopped.bc.object = recode.yn(cs_objcs),
                                stopped.bc.desc = recode.yn(cs_descr),
                                stopped.bc.casing = recode.yn(cs_casng),
                                stopped.bc.lookout = recode.yn(cs_lkout),
                                stopped.bc.clothing = recode.yn(cs_cloth),
                                stopped.bc.drugs = recode.yn(cs_drgtr),
                                stopped.bc.furtive = recode.yn(cs_furtv),
                                stopped.bc.violent = recode.yn(cs_vcrim),
                                stopped.bc.bulge = recode.yn(cs_bulge),
                                stopped.bc.other = recode.yn(cs_other)) %>% 
  select(-cs_objcs, -cs_descr, -cs_casng, -cs_lkout, -cs_cloth, - cs_drgtr, 
         -cs_furtv, -cs_vcrim, -cs_bulge, -cs_other)

# 8) reasons for frisk
sqf.data <- sqf.data %>% mutate(frisked.bc.suspected.crime = recode.yn(rf_vcrim),
                                frisked.bc.weapons = recode.yn(rf_othsw),
                                frisked.bc.attire = recode.yn(rf_attir),
                                frisked.bc.actual.crime = recode.yn(rf_vcact),
                                frisked.bc.noncompliance = recode.yn(rf_rfcmp),
                                frisked.bc.threats = recode.yn(rf_verbl),
                                frisked.bc.prior = recode.yn(rf_knowl),
                                frisked.bc.furtive = recode.yn(rf_furt),
                                frisked.bc.bulge = recode.yn(rf_bulg)) %>% 
  select(-rf_vcrim, -rf_othsw, -rf_attir, -rf_vcact, -rf_rfcmp, -rf_verbl, -rf_knowl,
         -rf_furt, -rf_bulg)

# 9) secondary circumstances of stop
sqf.data <- sqf.data %>% mutate(additional.report = recode.yn(ac_rept),
                                additional.investigation = recode.yn(ac_inves),
                                additional.proximity = recode.yn(ac_proxm),
                                additional.evasive = recode.yn(ac_evasv),
                                additional.associating = recode.yn(ac_assoc),
                                additional.direction = recode.yn(ac_cgdir),
                                additional.highcrime = recode.yn(ac_incid),
                                additional.time = recode.yn(ac_time),
                                additional.sights = recode.yn(ac_stsnd),
                                additional.other = recode.yn(ac_other)) %>% 
  select(-ac_rept, -ac_inves, -ac_proxm, -ac_evasv, -ac_assoc, -ac_cgdir, -ac_incid,
         -ac_time, -ac_stsnd, -ac_other)

# 10) basis of search
sqf.data <- sqf.data %>% mutate(searched.hardobject = recode.yn(sb_hdobj),
                                searched.outline = recode.yn(sb_outln),
                                searched.admission = recode.yn(sb_admis),
                                searched.other = recode.yn(sb_other)) %>% 
  select(-sb_hdobj, -sb_outln, -sb_admis, -sb_other)

# 11) results of frisk/search
sqf.data <- sqf.data %>% mutate(found.contraband = recode.yn(contrabn),
                                found.pistol = recode.yn(pistol),
                                found.rifle = recode.yn(riflshot),
                                found.assault = recode.yn(asltweap),
                                found.knife = recode.yn(knifcuti),
                                found.machinegun = recode.yn(machgun),
                                found.other = recode.yn(othrweap)) %>% 
  select(-contrabn, -pistol, -riflshot, -asltweap, -knifcuti, -machgun, -othrweap)

# 12) demographics of stop subject
sqf.data <- sqf.data %>% mutate(suspect.sex = recode.factor(sex, c('M', 'F'),
                                                            c('male', 'female')),
                                suspect.race = recode.factor(race, c('A','B','I','P','Q','W','Z'),
                                                             c('asian','black','native american','black hispanic','white hispanic','white','other')),
                                suspect.hispanic = (suspect.race %in% c('black hispanic','white hispanic'))) %>% 
  select(-sex, -race)

# age and DOB
sqf.data <- sqf.data %>% mutate(suspect.age = age, 
                                suspect.age = replace(suspect.age, suspect.age > 100, NA),
                                dob = sprintf("%08d", as.integer(dob)),
                                suspect.dob = mdy(dob),
                                suspect.dob = replace(suspect.dob, suspect.dob=='1900-12-31', NA)) %>% 
  select(-age, -dob)

# height (in feet) and weight (in lbs)
sqf.data <- sqf.data %>% mutate(suspect.height = (ht_feet + as.numeric(ht_inch)/12),
                                suspect.weight = weight,
                                suspect.weight = replace(suspect.weight, suspect.weight >= 700, NA)) %>% 
  select(-ht_feet, -ht_inch, -weight)

# hair color, eye color, and build
sqf.data <- sqf.data %>% mutate(suspect.hair = recode.factor(haircolr, 
                                                             c('BA','BK','BL','BR','DY','FR','GY', 'RD', 'SN', 'SP', 'WH', 'XX', 'ZZ'),
                                                             c('bald', 'black', 'blond', 'brown', 'dyed', 'frosted', 'gray', 'red', 'sandy', 'salt and pepper', 'white', 'unknown', 'other')),
                                suspect.eye = recode.factor(eyecolor,
                                                            c('BK','BL','BR','GY','GR','HA', 'MA', 'Z', 'ZZ', 'P', 'PK','DF', 'XX',  'MC', 'VI'),
                                                            c('black','blue','brown','gray','green','hazel', 'maroon',  'other', 'other','pink','pink', 'two different','unknown', 'unknown','violet')),
                                suspect.build = recode.factor(build,
                                                              c('H', 'M', 'T', 'U', 'Z'),
                                                              c('heavy', 'medium', 'thin', 'muscular', 'unknown'))) %>% 
  select(-haircolr, -eyecolor, -build)

# 13) add extra useful fields and filter data

# fields for weapon found or gun found
sqf.data <- sqf.data %>% mutate(found.gun = (found.pistol|found.rifle|found.assault|found.machinegun),
                                found.weapon = (found.pistol|found.rifle|found.assault|found.machinegun|found.knife|found.other))
# add a unique id
sqf.data$id <- 1:nrow(sqf.data)

# eliminate all ages except for those between 10 and 80.
sqf.data <- sqf.data %>% filter(suspect.age >= 10 & suspect.age <= 80)

# convert coordinates to lat/lon
coords <- proj4::project(list(sqf.data$xcoord, sqf.data$ycoord), nyc.proj, inverse=TRUE)
sqf.data$lat <- coords$y
sqf.data$lon <- coords$x

# 14) final useful additions/changes
# recode suspect.race for "white hispanic" and "black hispanic" to "hispanic"
levels(sqf.data$suspect.race) <- c("asian", "black", "native.american", "hispanic", "hispanic", "white", "other")

# add weekday, month, and time (6 four-hour-bins denoted by 1 through 6)
sqf.data <- sqf.data %>% mutate(day = wday(timestamp, label = T, abbr = F),
                                month = month(timestamp, label = T, abbr = F),
                                time.period = case_when(
                                  hour(timestamp) < 4 ~ '1',
                                  hour(timestamp) >= 4 & hour(timestamp) < 8 ~ '2',
                                  hour(timestamp) >= 8 & hour(timestamp) < 12 ~ '3',
                                  hour(timestamp) >= 12 & hour(timestamp) < 16 ~ '4',
                                  hour(timestamp) >= 16 & hour(timestamp) < 20 ~ '5',
                                  hour(timestamp) >= 20 ~ '6'
                                ))

# drop remaining irrelevant columns
sqf.data <- sqf.data %>% select(-crimsusp, -repcmd, -revcmd, -othfeatr, -addrtyp, 
                                -rescode, -premtype, -premname, -addrnum, -stname,
                                -stinter, -crossst, -aptnum, -state, -zip, -addrpct,
                                -post, -serial)


############################
#     NAs
############################

# proportion of missing values in all columns by year
# proportion.nas <- sqf.data %>% group_by(year) %>% summarize_all(funs(nas = sum(is.na(.))/n()))

# function to replace NAs with FALSE
update.logical <- function(dt, cols, criteria) {
  require(data.table)
  x <- as.data.frame(which(criteria==TRUE, arr.ind = TRUE))
  y <- as.matrix(subset(x, x$col %in% which((names(dt) %in% cols), arr.ind = TRUE)))
  y
}

# variables with more than 10% missing values
sqf.nas.variables <- c("additional.report", "additional.investigation", "additional.proximity", 
                          "additional.evasive","additional.associating", "additional.direction", 
                          "additional.highcrime", "additional.time","additional.sights", "additional.other",
                          "stopped.bc.desc", "stopped.bc.violent", "stopped.bc.other", "stopped.bc.object",
                          "stopped.bc.casing", "stopped.bc.lookout", "stopped.bc.drugs", "stopped.bc.clothing",
                          "stopped.bc.furtive", "stopped.bc.bulge",
                          "radio.run","frisked.bc.suspected.crime","frisked.bc.weapons","frisked.bc.attire",
                          "frisked.bc.actual.crime","frisked.bc.noncompliance","frisked.bc.threats",
                          "frisked.bc.prior","frisked.bc.furtive","frisked.bc.bulge",
                          "force.hands", "force.wall","force.ground","force.drawn","force.pointed",
                          "force.baton", "force.handcuffs", "force.pepper", "force.other",
                          "searched.hardobject","searched.outline","searched.admission","searched.other",
                          "found.weapon", "found.gun", "arrested", "searched", "frisked","extra.reports")

# run function to replace NAs with FALSE
sqf.data[update.logical(sqf.data, sqf.nas.variables, is.na(sqf.data))] <- FALSE

# view proportion of missing values in all columns by year again
# proportion.nas <- sqf.data %>% group_by(year) %>% summarize_all(funs(nas = sum(is.na(.))/n()))

# write csv
write.csv(sqf.data, "sqf_08_16.csv")
