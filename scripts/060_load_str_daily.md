# Load daily data
Tourism Economics  





```r
fpath <- c("~/Project/R projects/sf_2016/") 
```

Read in data

```r
# City/county data
fname <- paste0(fpath, "input_data/sf_daily/email 2016-05-23/SF City and County- Set Analysis (Jan 2011 - Present).xlsx")
input_dailysf_1 <- readxl::read_excel(fname, sheet=1, col_names = TRUE, skip = 5)

# convention center set
fname <- paste0(fpath, "input_data/sf_daily/email 2016-05-23/Convention Center Set Analysis (Jan 2012 - Present).xlsx")
input_dailyconv_1 <- readxl::read_excel(fname, sheet=1, col_names = TRUE, skip = 5)

# surrounding area
fname <- paste0(fpath, "input_data/surrounding_daily/email 2016-04-22/770778_OaklandBerkeley.xls")
input_dailyoak_1 <- readxl::read_excel(fname, sheet=2, col_names = TRUE, skip = 4)
```

```
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 01 00 00 00 00 00 00 06 3b 05 00 00 00 21 00 00 00 0a 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 02 00 00 00 00 00 00 06 3b 01 00 00 00 bc 05 00 00 15 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 03 00 00 00 00 00 00 06 3b 02 00 00 00 49 01 00 00 2d 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 04 00 00 00 00 00 00 06 3b 03 00 00 00 29 00 00 00 05 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 05 00 00 00 00 00 00 06 3b 04 00 00 00 21 00 00 00 00 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 02 00 00 00 00 00 00 07 3b 01 00 00 00 05 00 00 00 ff 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 03 00 00 00 00 00 00 07 3b 02 00 00 00 05 00 00 00 ff 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 01 00 00 00 00 00 00 06 3b 05 00 00 00 21 00 00 00 0a 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 02 00 00 00 00 00 00 06 3b 01 00 00 00 bc 05 00 00 15 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 03 00 00 00 00 00 00 06 3b 02 00 00 00 49 01 00 00 2d 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 04 00 00 00 00 00 00 06 3b 03 00 00 00 29 00 00 00 05 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 05 00 00 00 00 00 00 06 3b 04 00 00 00 21 00 00 00 00 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 02 00 00 00 00 00 00 07 3b 01 00 00 00 05 00 00 00 ff 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 03 00 00 00 00 00 00 07 3b 02 00 00 00 05 00 00 00 ff 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 01 00 00 00 00 00 00 06 3b 05 00 00 00 21 00 00 00 0a 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 02 00 00 00 00 00 00 06 3b 01 00 00 00 bc 05 00 00 15 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 03 00 00 00 00 00 00 06 3b 02 00 00 00 49 01 00 00 2d 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 04 00 00 00 00 00 00 06 3b 03 00 00 00 29 00 00 00 05 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 05 00 00 00 00 00 00 06 3b 04 00 00 00 21 00 00 00 00 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 02 00 00 00 00 00 00 07 3b 01 00 00 00 05 00 00 00 ff 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 03 00 00 00 00 00 00 07 3b 02 00 00 00 05 00 00 00 ff 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 01 00 00 00 00 00 00 06 3b 05 00 00 00 21 00 00 00 0a 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 02 00 00 00 00 00 00 06 3b 01 00 00 00 bc 05 00 00 15 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 03 00 00 00 00 00 00 06 3b 02 00 00 00 49 01 00 00 2d 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 04 00 00 00 00 00 00 06 3b 03 00 00 00 29 00 00 00 05 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 05 00 00 00 00 00 00 06 3b 04 00 00 00 21 00 00 00 00 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 02 00 00 00 00 00 00 07 3b 01 00 00 00 05 00 00 00 ff 00 
## DEFINEDNAME: 20 00 00 01 0b 00 00 00 03 00 00 00 00 00 00 07 3b 02 00 00 00 05 00 00 00 ff 00
```

```
## Warning in xls_cols(path, sheet, col_names = col_names, col_types =
## col_types, : Expecting numeric in [1464, 2] got `The STR Trend Report is
## a publication of STR, Inc. and STR Global, Ltd., and is intended solely
## for use by paid subscribers. Reproduction or distribution of the STR Trend
## Report, in whole or part, without written permission is prohibited and
## subject to legal action. If you have received this report and are NOT a
## subscriber to the STR Trend report, please contact us immediately. Source:
## 2016 STR, Inc. / STR Global, Ltd. trading as â€œSTRâ€.`
```

Clean up the City County data

```r
input_dailysf_2 <- input_dailysf_1 %>%
  # drop columns that are all NA
  .[, colSums(is.na(.)) != nrow(.)] 

# check column names equal expected length
a <- length(colnames(input_dailysf_2))
stopifnot(a == 31)

# select only the columns we need
input_dailysf_2 <- input_dailysf_2[,5:17] 

# initial clean up of column names
colnames(input_dailysf_2) <- colnames(input_dailysf_2) %>%
	  tolower() %>%
	  gsub(" ", "_", .)

# append the segment to certain columns and rename the columns
temp <- c("",rep("occ",4), rep("adr", 4), rep("revpar", 4))
temp_colnames <- paste(colnames(input_dailysf_2), temp, sep="_")
# fix date column
temp_colnames[1] <- "date"
colnames(input_dailysf_2) <- temp_colnames

# format date column
input_dailysf_2 <- input_dailysf_2 %>%
  mutate(date = as.Date(date))


input_dailysf_3 <- input_dailysf_2 %>%
  gather(variable, value, -date) %>%
  separate(variable, c("seg", "var"), sep = "_") %>%
  spread(var, value) %>%
  mutate(occ = occ/100) %>%
    # these are just blank
  mutate(demd = NA,
         supd = NA,
         rmrev = NA) %>%
  mutate(geo_ttl = "sfcity") %>%
  select(geo_ttl, date, seg, everything())
```


Clean up the convention center cet data

```r
input_dailyconv_2 <- input_dailyconv_1 %>%
  # drop columns that are all NA
  .[, colSums(is.na(.)) != nrow(.)] 

# check column names equal expected length
a <- length(colnames(input_dailyconv_2))
stopifnot(a == 30)

# select only the columns we need
input_dailyconv_2 <- input_dailyconv_2[,6:18] 

# initial clean up of column names
colnames(input_dailyconv_2) <- colnames(input_dailyconv_2) %>%
	  tolower() %>%
	  gsub(" ", "_", .)

# format date column
input_dailyconv_2 <- input_dailyconv_2 %>%
  mutate(date = as.Date(date))


input_dailyconv_3 <- input_dailyconv_2 %>%
  gather(variable, value, -date) %>%
  separate(variable, c("seg", "var"), sep = "_") %>%
  spread(var, value) %>%
  mutate(occ = occ/100) %>%
  # these are just blank
  mutate(demd = NA,
         supd = NA,
         rmrev = NA) %>%
  mutate(geo_ttl = "sfconv") %>%
  select(geo_ttl, date, seg, everything())
```

Clean up the oakland data

```r
input_dailyoak_2 <- input_dailyoak_1 %>%
  # drop columns that are all NA
  .[, colSums(is.na(.)) != nrow(.)] 

# check column names equal expected length
a <- length(colnames(input_dailyoak_2))
stopifnot(a == 21)

# delete the second row
input_dailyoak_2 <- input_dailyoak_2[-c(1), ]

# initial clean up of column names
colnames(input_dailyoak_2) <- colnames(input_dailyoak_2) %>%
	  tolower() %>%
	  gsub(" ", "_", .)

# name certain columns based on position, which is maybe not a 
# great, stable idea.
names(input_dailyoak_2)[8] <- "x8"
names(input_dailyoak_2)[10] <- "x10"
names(input_dailyoak_2)[12] <- "x12"
names(input_dailyoak_2)[14] <- "x14"
names(input_dailyoak_2)[16] <- "x16"
names(input_dailyoak_2)[18] <- "x18"
names(input_dailyoak_2)[19] <- "censprops"
names(input_dailyoak_2)[20] <- "censrooms"
names(input_dailyoak_2)[21] <- "censparticip"



# select columns we want to keep
input_dailyoak_3 <- input_dailyoak_2 %>%
  # drop any rows that are NA in date column
  filter(!is.na(date))	%>%
  select(date, supd=supply, demd=demand, rmrev=revenue, censprops, censrooms, censparticip) %>%
  mutate(supd = as.numeric(supd),
         demd = as.numeric(demd),
         rmrev = as.numeric(rmrev)) %>%
  mutate(occ = demd/supd) %>%
  mutate(adr = rmrev/demd) %>%
  mutate(revpar = rmrev/supd) %>%
  # format date column
  mutate(date = as.Date(date))

# insert geo_ttl
input_dailyoak_4 <- input_dailyoak_3 %>%
  mutate(geo_ttl = "oak") %>%
  mutate(seg = "total") %>%
  select(geo_ttl, date, everything())

# select a few series to drop
input_dailyoak_5 <- input_dailyoak_4 %>%
  select(-censprops, -censrooms, -censparticip)
```


Combine

```r
str_daily <- bind_rows(input_dailysf_3, input_dailyconv_3, input_dailyoak_5)
```

Save output

```r
save(str_daily, file=paste0(fpath,"output_data/str_daily.Rdata"))

write.csv(str_daily, file=paste0(fpath,"output_data/str_daily.csv"), row.names=FALSE)
```

