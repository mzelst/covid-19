u <- "https://www.rivm.nl/coronavirus-covid-19/virus/varianten"
webpage <- read_html(u)
table <- as.data.frame(html_table(webpage))

table <- as.data.frame(t(table))

colnames(table) <- table[1,]

table <- table[-c(1:2,nrow(table)),]

table$Week <- row.names(table)
table$Week <- read.table(text = table[1:nrow(table),"Week"], sep = ".", as.is = TRUE)$V2
table$year <- parse_number(str_sub(row.names(table),start = 1, end = 5))
rownames(table) <- c()

colnames(table) <- c("Aantal_monsters","Britse_variant","Britse_variant_E484K","ZuidAfrikaanse_variant","Braziliaanse_variant_P1",
                     "Indiase_Variant_B1.167.2","Indiase_Variant_B1.167.1","Indiase_Variant_B1.167.3","B.1.525_variant_E484K_F888L","B.1.620",
                     "Colombiaanse_variant_B.1.621","Californie_variant","Filipijnen_variant_P3","Bretagne_variant",
                     "Week","Jaar")

table <- table %>% 
  mutate_if(is.character,as.numeric) %>%
  select(Week, Jaar, Aantal_monsters:Bretagne_variant) %>%
  setorder(Jaar,Week)

variants.prevalence <- table %>%
  mutate(prevalentie_britsevariant = round(Britse_variant/Aantal_monsters*100,2)) %>%
  mutate(prevalentie_ZAvariant = round(ZuidAfrikaanse_variant/Aantal_monsters*100,2)) %>%
  mutate(prevalentie_P1_variant = round(Braziliaanse_variant_P1/Aantal_monsters*100,2)) %>%
mutate(prevalentie_B1.167.2 = round(Indiase_Variant_B1.167.2/Aantal_monsters*100,2))

write.csv(variants.prevalence,"data-misc/variants-rivm/prevalence_variants.csv",row.names = F)

variants.old.row <- first(variants.prevalence$Week)+4
variants.old <- read.csv("https://raw.githubusercontent.com/mzelst/covid-19/master/data-misc/variants-rivm/prevalence_variants.csv")[1:variants.old.row,]

variants.new <- read.csv("data-misc/variants-rivm/prevalence_variants.csv")

variants.prevalence <- rbind(variants.old, variants.new)

variants.prevalence <- variants.prevalence %>%
  mutate(prevalentie_B1.167.2 = round(Indiase_Variant_B1.167.2/Aantal_monsters*100,2))

write.csv(variants.prevalence,"data-misc/variants-rivm/prevalence_variants.csv",row.names = F)

week.variants <- isoweek(Sys.Date())

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

##Push to git
repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("Week " , week.variants, " - Weekly (automated) update variants data"))
push(repo, credentials = git.auth)

rm(table,variants.prevalence,webpage,u,week.variants, variants.new, variants.old)

send_webhook_message("De varianten data is weer bijgewerkt en is hier te vinden: https://github.com/mzelst/covid-19/blob/master/data-misc/variants-rivm/prevalence_variants.csv")