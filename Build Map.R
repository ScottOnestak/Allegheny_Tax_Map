#1 Pull Shapefiles
#Author: Scott Onestak


library(tidycensus)
library(tigris)
library(sf)
library(ggplot2)
library(tidyverse)
library(stringr)
library(xml2)
library(tidyr)
library(httr)
library(rvest)
library(readxl)

#Pull divisions that aren't Pittsburgh
divisions = county_subdivisions(state = "PA",county=3,year=2022) %>%
              filter(NAME != "Pittsburgh") %>%
              mutate(Pittsburgh = 0) %>%
              select(NAMELSAD,Pittsburgh,geometry) %>%
              rename(NAME=NAMELSAD) %>%
              st_transform(.,crs = 4326)
              
#Pull in Pittsburgh neighborhoods and append
Pittsburgh = st_sf(st_read("Data/Pittsburgh Neighborhoods/Neighborhoods_.shp")) %>%
                rename(NAME=hood) %>%
                mutate(Pittsburgh = 1) %>%
                select(NAME,Pittsburgh,geometry) %>%
                st_transform(.,crs = 4326)

#stack the shapefiles
areas = rbind(divisions,Pittsburgh)

areas2 = areas %>% st_drop_geometry() %>% mutate(JOIN_NAME = ifelse(Pittsburgh==0,toupper(NAME),"CITY OF PITTSBURGH"))
write.csv(areas2,"Data/Tax Rates/tax_rates.csv",row.names=F)

#Scrape Property Tax Rates
muni_tax = as.data.frame(read_html("https://apps.alleghenycounty.us/website/millmuni.asp") %>% 
                           html_table(fill=TRUE) %>% .[2]) %>%
              rename('Land_Municipal_Tax' = 'Land.1',
                     'JOIN_NAME'='Municipality',
                     'Municipality'='Millage')

allegheny_tax_rt = as.numeric(unlist(muni_tax %>% filter(JOIN_NAME == "Allegheny County") %>% select(Municipality)))

for(i in seq(from=1,to=dim(muni_tax)[1],by=1)){
  if(str_detect(muni_tax[i,"JOIN_NAME"],"1")){
    muni_tax[i,"JOIN_NAME"] = toupper(trimws(str_replace_all(muni_tax[i,"JOIN_NAME"],"1","")))
  } else if(str_detect(muni_tax[i,"JOIN_NAME"],"2")){
    muni_tax[i,"JOIN_NAME"] = toupper(trimws(str_replace_all(muni_tax[i,"JOIN_NAME"],"2","")))
  } else if(str_detect(muni_tax[i,"JOIN_NAME"],"3")){
    muni_tax[i,"JOIN_NAME"] = toupper(trimws(str_replace_all(muni_tax[i,"JOIN_NAME"],"3","")))
  } else if(str_detect(muni_tax[i,"JOIN_NAME"],"4")){
    muni_tax[i,"JOIN_NAME"] = toupper(trimws(str_replace_all(muni_tax[i,"JOIN_NAME"],"4","")))
  } else if(str_detect(muni_tax[i,"JOIN_NAME"],"5")){
    muni_tax[i,"JOIN_NAME"] = toupper(trimws(str_replace_all(muni_tax[i,"JOIN_NAME"],"5","")))
  } else if(str_detect(muni_tax[i,"JOIN_NAME"],"6")){
    muni_tax[i,"JOIN_NAME"] = toupper(trimws(str_replace_all(muni_tax[i,"JOIN_NAME"],"6","")))
  } else if(str_detect(muni_tax[i,"JOIN_NAME"],"7")){
    muni_tax[i,"JOIN_NAME"] = toupper(trimws(str_replace_all(muni_tax[i,"JOIN_NAME"],"7","")))
  } else if(str_detect(muni_tax[i,"JOIN_NAME"],"8")){
    muni_tax[i,"JOIN_NAME"] = toupper(trimws(str_replace_all(muni_tax[i,"JOIN_NAME"],"8","")))
  } else if(str_detect(muni_tax[i,"JOIN_NAME"],"9")){
    muni_tax[i,"JOIN_NAME"] = toupper(trimws(str_replace_all(muni_tax[i,"JOIN_NAME"],"9","")))
  } else if(str_detect(muni_tax[i,"JOIN_NAME"],"0")){
    muni_tax[i,"JOIN_NAME"] = toupper(trimws(str_replace_all(muni_tax[i,"JOIN_NAME"],"0","")))
  } else {
    muni_tax[i,"JOIN_NAME"] = toupper(trimws(muni_tax[i,"JOIN_NAME"]))
  }
  
  #Some manual overrides
  if(muni_tax[i,"JOIN_NAME"]=="CITY OF DUQUESNE"){
    muni_tax[i,"JOIN_NAME"]="DUQUESNE CITY"
  } else if(muni_tax[i,"JOIN_NAME"]=="BETHEL PARK"){
    muni_tax[i,"JOIN_NAME"]="BETHEL PARK MUNICIPALITY"
  } else if(muni_tax[i,"JOIN_NAME"]=="CITY OF CLAIRTON"){
    muni_tax[i,"JOIN_NAME"]="CLAIRTON CITY"
  } else if(muni_tax[i,"JOIN_NAME"]=="PENNSBURY VILLAGE"){
    muni_tax[i,"JOIN_NAME"]="PENNSBURY VILLAGE BOROUGH"
  } else if(muni_tax[i,"JOIN_NAME"]=="CITY OF MCKEESPORT"){
    muni_tax[i,"JOIN_NAME"]="MCKEESPORT CITY"
  } else if(muni_tax[i,"JOIN_NAME"]=="MOUNT LEBANON"){
    muni_tax[i,"JOIN_NAME"]="MOUNT LEBANON TOWNSHIP"
  }
}

#pull school tax rates
school_tax = as.data.frame(read_html("https://apps.alleghenycounty.us/website/millsd.asp") %>% 
                           html_table(fill=TRUE) %>% .[2]) %>%
                rename('Land_School_Tax' = 'Land..U.0095.',
                       'JOIN_NAME'='Municipality',
                       'School'='Millage',
                       'School District'='School.District') %>%
                mutate(JOIN_NAME = toupper(JOIN_NAME))

for(i in seq(from=1,to=dim(school_tax)[1],by=1)){
  #Some manual overrides
  if(str_detect(school_tax[i,"School District"],"\u0095")){
    school_tax[i,"School District"] = trimws(str_replace_all(school_tax[i,"School District"],"\u0095",""))
  } else if(str_detect(school_tax[i,"School District"],"º")){
    school_tax[i,"School District"] = trimws(str_replace_all(school_tax[i,"School District"],"º",""))
  } else {
    school_tax[i,"School District"] = trimws(school_tax[i,"School District"])
  }
}

school_tax_final = NA
school_tax_started = FALSE
for(i in seq(from=1,to=dim(school_tax)[1],by=1)){
  temp = unlist(str_split(school_tax[i,"JOIN_NAME"],","))
  
  for(j in seq(from=1,to=length(temp),by=1)){
    if(school_tax_started==FALSE){
      school_tax_final = as.data.frame(t(c(trimws(temp[j]),
                                         school_tax[i,"School District"],
                                         school_tax[i,"School"],
                                         school_tax[i,"Land_School_Tax"])))
      school_tax_started = TRUE
    } else {
      school_tax_final = rbind(school_tax_final,
                               as.data.frame(t(c(trimws(temp[j]),
                                                 school_tax[i,"School District"],
                                                 school_tax[i,"School"],
                                                 school_tax[i,"Land_School_Tax"]))))
    }
  }
}
colnames(school_tax_final) = c("JOIN_NAME","School District","School","Land_School_Tax")
school_tax_final$School = as.numeric(school_tax_final$School)
school_tax_final$Land_School_Tax = as.numeric(school_tax_final$Land_School_Tax)

for(i in seq(from=1,to=dim(school_tax_final)[1],by=1)){
  if(school_tax_final[i,"JOIN_NAME"]=="CITY OF DUQUESNE"){
    school_tax_final[i,"JOIN_NAME"]="DUQUESNE CITY"
  } else if(school_tax_final[i,"JOIN_NAME"]=="BETHEL PARK"){
    school_tax_final[i,"JOIN_NAME"]="BETHEL PARK MUNICIPALITY"
  } else if(school_tax_final[i,"JOIN_NAME"]=="CITY OF CLAIRTON"){
    school_tax_final[i,"JOIN_NAME"]="CLAIRTON CITY"
  } else if(school_tax_final[i,"JOIN_NAME"]=="PENNSBURY VILLAGE"){
    school_tax_final[i,"JOIN_NAME"]="PENNSBURY VILLAGE BOROUGH"
  } else if(school_tax_final[i,"JOIN_NAME"]=="CITY OF MCKEESPORT"){
    school_tax_final[i,"JOIN_NAME"]="MCKEESPORT CITY"
  } else if(school_tax_final[i,"JOIN_NAME"]=="MOUNT LEBANON"){
    school_tax_final[i,"JOIN_NAME"]="MOUNT LEBANON TOWNSHIP"
  }
}

#Read in income tax rates
income_tax = read_excel("Data/Tax Rates/Income_Tax_Rates.xlsx") %>%
                rename("Income_Tax" = "Total Resident Income Tax (percent)",
                       "JOIN_NAME" = "Municipality") %>%
                select(c("JOIN_NAME","Income_Tax")) %>%
                unique() 

#Fix for Baldwin-Whitehall SD...which has part inside Pittsburgh City Limits but has its own tax rates
income_tax = income_tax %>% 
                filter(JOIN_NAME != "PITTSBURGH CITY" | (JOIN_NAME == "PITTSBURGH CITY" & Income_Tax >= 3))
income_tax$Income_Tax =income_tax$Income_Tax/100

for(i in seq(from=1,to=dim(income_tax)[1],by=1)){
  if(str_detect(income_tax[i,"JOIN_NAME"]," TWP")){
    income_tax[i,"JOIN_NAME"] = trimws(str_replace_all(income_tax[i,"JOIN_NAME"]," TWP"," TOWNSHIP"))
  } else if(str_detect(income_tax[i,"JOIN_NAME"]," BORO")){
    income_tax[i,"JOIN_NAME"] = trimws(str_replace_all(income_tax[i,"JOIN_NAME"]," BORO"," BOROUGH"))
  }
  
  #Manual changes
  if(income_tax[i,"JOIN_NAME"]=="UPPER ST CLAIR TOWNSHIP"){
    income_tax[i,"JOIN_NAME"] = "UPPER ST. CLAIR TOWNSHIP"
  } else if(income_tax[i,"JOIN_NAME"]=="MONROEVILLE BOROUGH"){
    income_tax[i,"JOIN_NAME"] = "MONROEVILLE MUNICIPALITY"
  } else if(income_tax[i,"JOIN_NAME"]=="BETHEL PARK BOROUGH"){
    income_tax[i,"JOIN_NAME"] = "BETHEL PARK MUNICIPALITY"
  } else if(income_tax[i,"JOIN_NAME"]=="MT LEBANON TOWNSHIP"){
    income_tax[i,"JOIN_NAME"] = "MOUNT LEBANON TOWNSHIP"
  } else if(income_tax[i,"JOIN_NAME"]=="OHARA TOWNSHIP"){
    income_tax[i,"JOIN_NAME"] = "O'HARA TOWNSHIP"
  } else if(income_tax[i,"JOIN_NAME"]=="PITTSBURGH CITY"){
    income_tax[i,"JOIN_NAME"] = "CITY OF PITTSBURGH"
  }
}


#Join it all together
taxes = areas2 %>% mutate(County = allegheny_tax_rt) %>%
          left_join(.,muni_tax %>% select(c("JOIN_NAME","Municipality","Land_Municipal_Tax")),by=c("JOIN_NAME")) %>%
          left_join(.,school_tax_final %>% select(c("JOIN_NAME","School District","School","Land_School_Tax")),by=c("JOIN_NAME")) %>%
          left_join(.,income_tax,by=c("JOIN_NAME")) %>%
          mutate(County = ifelse(is.na(County),NA,County/1000),
                 Municipality = ifelse(is.na(Municipality),NA,Municipality/1000),
                 Land_Municipal_Tax = ifelse(is.na(Land_Municipal_Tax),NA,Land_Municipal_Tax/1000),
                 School = ifelse(is.na(School),NA,School/1000),
                 Land_School_Tax = ifelse(is.na(Land_School_Tax),NA,Land_School_Tax/1000),
                 Library = ifelse(JOIN_NAME == "CITY OF PITTSBURGH",0.00025,NA),
                 Park = ifelse(JOIN_NAME == "CITY OF PITTSBURGH",0.00050,NA)) %>%
          rowwise() %>%
          mutate(Total_Property_Tax = sum(County,Municipality,School,Library,Park,na.rm = T))

taxes_clean = taxes %>%
                rename("Land (Municipality Tax)" = "Land_Municipal_Tax",
                       "Land (School Tax)" = "Land_School_Tax",
                       "Total Property Tax"="Total_Property_Tax",
                       "Income Tax"="Income_Tax") %>%
                select(-c("JOIN_NAME","Pittsburgh"))

#Join back to the shapefile
areas = areas %>% left_join(.,taxes_clean,by="NAME")

#write out file
st_write(areas,"Data/Allegheny County Neighborhoods/Allegheny_County_Neighborhoods.shp")

#Build cases for total tax paid
scenarios = NA
scenarios_found = FALSE
for(i in seq(from=50000,to=300000,by=5000)){
  for(j in seq(from=100000,to=500000,by=5000)){
    if(scenarios_found==FALSE){
      scenarios = as.data.frame(t(c(i,j)))
      scenarios_found = TRUE
    } else {
      scenarios = rbind(scenarios,as.data.frame(t(c(i,j))))
    }
  }
}
colnames(scenarios) = c("Income","Property_Price")

scenarios = taxes %>% full_join(.,scenarios, by = character()) %>%
              mutate(CLR = 1.57) %>%
              mutate(Total_Tax = Income_Tax*Income + ((Property_Price*(1/CLR))-18000)*County +(Property_Price*(1/CLR))*(Total_Property_Tax-County)) %>%
              select(c("NAME","Income","Property_Price","Total_Tax")) %>%
              rename("Property Purchase Price"="Property_Price",
                     "Total Local Tax"="Total_Tax")

write.csv(scenarios,"Data/tax_estimates.csv",row.names = F)

