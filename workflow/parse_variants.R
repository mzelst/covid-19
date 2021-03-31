u <- "https://www.rivm.nl/coronavirus-covid-19/virus/varianten"
webpage <- read_html(u)
table <- as.data.frame(html_table(webpage))

colnames(table) <- c("Week","Aantal_monsters","Britse_variant","ZuidAfrikaanse_variant","Braziliaanse_variant_P1",
                     "Britse_variant_E484K","B.1.525_variant_E484K_F888L","Californie_variant","Braziliaanse_variant_P2")

variants.prevalence <- table %>%
  mutate(prevalentie_britsevariant = round(Britse_variant/Aantal_monsters*100,2)) %>%
  mutate(prevalentie_ZAvariant = round(ZuidAfrikaanse_variant/Aantal_monsters*100,2))

write.csv(variants.prevalence,"data-misc/prevalence_variants.csv",row.names = F)

week.variants <- isoweek(Sys.Date())

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

##Push to git
repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("Week " , week.variants, " - Weekly (automated) update variants data"))
push(repo, credentials = git.auth)
