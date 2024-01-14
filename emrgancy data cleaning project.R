#get data from excel file

df <- read_excel("EM data.xlsx")
df <- as_tibble(df)
df
#explore the data 
colnames(df)
glimpse(df)
#cleaning
df <- clean_names(df)
df
get_dupes(df)
df <-df %>% 
  separate(title_of_emergency,into = c("Type","subType"),": ")
  
df<-df %>% 
  mutate(across(subType,~str_remove(.,"-$"))) %>% # to clean the contant from the last symp
  mutate(across(subType,~str_squish(str_to_title(.))))#to remove spaces& change the case

# check the other col
df$description_of_emergency
df<-df %>% 
  mutate(Station = str_extract(description_of_emergency,"Station[: ]\\w*")) %>% 
  mutate(across(Station,~str_remove(.,"Station[: ]")))
df<-df %>% 
  separate(description_of_emergency,into = letters[1:4],";")
view(df)
df<-df %>% 
  mutate(date_time = str_remove(c ,"-?Station[: ]\\w*")) %>% 
  unite(DateTime ,date_time,d,sep = "") %>% 
  select(-c) %>% 
  rename(Address = a , Town = b) %>% 
  mutate(across(Town,~str_squish(.)))
df
df<-df %>% 
  mutate(across(DateTime,~str_squish(.)))
view(df)

#now we have clean data ready for analysis
df %>% count(Type)
ggplot(df,aes(Type))+ geom_bar(fill = "darkgreen")


