
if (!require("dplyr",character.only=TRUE)){
  install.packages(pkgs="dplyr",repos="http://cran.r-project.org")
  require("dplyr",character.only=TRUE)}

if (!require("tidyr",character.only=TRUE)){
  install.packages(pkgs="tidyr",repos="http://cran.r-project.org")
  require("tidyr",character.only=TRUE)}

if (!require("car",character.only=TRUE)){
  install.packages(pkgs="car",repos="http://cran.r-project.org")
  require("car",character.only=TRUE)}

# Creates a SQLite3 database for us to copy the ACS data into
#This is more instructional than useful for looking at JUST the Colorado data, which is pretty small.  I have the whole
# US in a PostgreSQL database for each year, but set-up for that is much more involved.
#This command also creas a connection to the DB.  See the commented out command below if you've already made the DB.
acs13COdb=src_sqlite("acs13_CO.sqlite3", create = T)
#acs13COdb=src_sqlite("acs13_CO.sqlite3")

# Reads in csv of Colorado PUMS, change the "Data/" part as needed to point to the file.
ss13pco=read.csv("Data/ss13pco.csv", header=T)

# Moves data to the DB into a table named acs13_p 
acs13_p=copy_to(acs13COdb, ss13pco, name="acs13_p", temporary=FALSE)

#Removes the CSV data from memory to help make things faster.
rm(ss13pco)

# Create a tidy tabulation of Age by Poverty Level for Larimer County 

tab=tbl(acs13COdb, "acs13_p")%>%  # This tells R to look at the DB 'acs13COdb' for the table 'acs13_p' we just made
  select(PWGTP, AGEP, PUMA, POVPIP )%>% #chooses the needed columns from the database before we pull the data to R
  filter(PUMA==102 | PUMA==103) %>% # Chooses the Public Use Microdata Areas that correspond to Larimer County before colleting data to R and removes missing data for Poverty Status
  collect()%>% #pulls the requested data into R
  mutate(age_r=recode(AGEP,"0:9=1;10:19=2;20:29=3;30:39=4;40:49=5;50:59=6;60:69=7;70:79=8;80:89=9;90:115=10"), # Recodes raw age into more usable categories
         age=ordered(age_r, levels=1:10, labels=c("0 to 9", "10 to 19", "20 to 29", "30 to 39","40 to 49","50 to 59", "60 to 69",
                                                  "70 79","80 to 89", "90 and Over")), # adds nice labels to the age recode and makes it a factor (nice for graphing, not nice for numerical work)
         poverty_r=recode(POVPIP, "0:99=1; 100:149=2; 150:199=3; 200:299=4; 300:501=5; NA=6"),
         poverty=ordered(poverty_r, levels=1:6, labels=c("Below 100%", "100% to 149%", "150% to 199%", "200% to 299%", "300% or More", "Missing")))%>% #categorizes Poverty to a custom set of levels
  group_by(age, poverty)%>% # Tells dplyr which variables to group_by...basically what are the crosstab variables
  summarize(total=sum(PWGTP))%>%# tells dplyr to summarize the data by the groups above, but uses the weighting variable to get the weighted estimat
  ungroup() #Makes other operations on the new table easier when it isn't pre-grouped
##NOTE: the '%>%' or 'pipe' operator passes the output from the first command to the first argument of the second command and so on to form an analysis chain.

# Create a table from the tidy tabulation

table=tab%>%
  spread(poverty, total)

print(table)








## dplyr Vignette: http://cran.r-project.org/web/packages/dplyr/vignettes/databases.html