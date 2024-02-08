# %% [markdown]
# First let's Import the Data for This Project
merged_mpd_wb <- read.csv("manifesto_worldbank_evs.csv")
# %%



# %%
library(tidyverse)


# %%
library(dplyr)

merged_mpd_wb <- merged_mpd_wb %>%
  mutate(nl_dummy = ifelse(partyname == "Party of Freedom", 1, 0))


# %%
colnames(merged_mpd_wb)

# %%
unique(merged_mpd_wb$nl_dummy)

# %%
merged_mpd_wb$nf_dummy <- ifelse(merged_mpd_wb$partyname == "National Front", 1, 0)

merged_mpd_wb$afd <- ifelse(merged_mpd_wb$partyname == "Alternative for Germany", 1, 0)

merged_mpd_wb$spain_dummy <- ifelse(merged_mpd_wb$partyname == "Voice", 1, 0)

merged_mpd_wb$dk_dummy <- ifelse(merged_mpd_wb$partyname == "Danish People’s Party", 1, 0)

merged_mpd_wb$sd_dummy <- ifelse(merged_mpd_wb$partyname == "Sweden Democrats", 1, 0)


merged_mpd_wb$pd_dummy <- ifelse(merged_mpd_wb$partyname == "Law and Justice", 1, 0)

merged_mpd_wb$pp_dummy <- ifelse(merged_mpd_wb$partyname == "Progress Party", 1, 0)

merged_mpd_wb$italy_nf <- ifelse(merged_mpd_wb$partyname %in% c("Brothers of Italy", "Northern League"), 1, 0)

merged_mpd_wb$hungary_nf <- ifelse(merged_mpd_wb$partyname %in% c(
  "Alliance of Federation of Young Democrats - Hungarian Civic Union - Christian Democratic People's Party",
  "Federation of Young Democrats - Hungarian Civic Party - Hungarian Democratic Forum- Alliance",
  "Federation of Young Democrats"
), 1, 0)


# %%
library(stargazer)
library(lme4)
# Define the models
welfare_year <- lmer(welfare ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf + nl_dummy + (1 | countryname) + (1 | coderyear), data = merged_mpd_wb)
welfare_unemployment <- lmer(welfare ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf + nl_dummy + SL.UEM.TOTL.ZS + (1 | countryname) + (1 | coderyear), data = merged_mpd_wb)
welfare_gdp_per_capita <- lmer(welfare ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf + nl_dummy + NY.GDP.PCAP.PP.KD + (1 | countryname) + (1 | coderyear), data = merged_mpd_wb)

#

# %%
library(stargazer)

nrr_welfare <- lm(welfare ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf + nl_dummy, data = merged_mpd_wb)
dep_var_label <- "Dependent Variable: Party Manifesto Support for Welfare"
covar_labels <- c("National Front", "Alternative for Germany", "Voice", "Danish People's Party", "Sweden Democrats", "Law and Justice", "Progress Party", "Brothers of Italy", "Fidesz", "Party Of Freedom", "Unemployment", "GDP Per Capita")

stargazer_output <-stargazer(nrr_welfare,
          welfare_year,
          welfare_unemployment,
          welfare_gdp_per_capita,
          type = "html",
          title = "Welfare Regression Adjusted by Country and Year",
          covariate.labels = covar_labels,
          dep.var.caption = dep_var_label
)
display_html(stargazer_output)

# %%
# EVS Regression to see if  European Values has any interaction NRR parties being pro-welfare
welfare_evs_2 <- lmer(welfare ~ E037 + nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf + nl_dummy+(E037 |countryname) + (E037 | coderyear), data = merged_mpd_wb)
party_labels <- c("EVS Variable:E037(Pro-Free-Market)", "National Front", "Alternative for Germany", "Voice", "Danish People's Party", "Sweden Democrats", "Law and Justice", "Progress Party", "Brothers of Italy", "Fidesz","Party For Freedom")
output_wefare_evs<-stargazer(welfare_evs_2,
          type = "html",
          title = "EVS Regression Adjusted by Country and Year",
                       omit = c("Constant"),
         omit.labels = c("Constant"),
            covariate.labels = party_labels,
          out = "EVS.md"
          )
display_html(output_wefare_evs)

# %%
# Run linear mixed-effects model
markeco_year <- lmer(markeco ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf +nl_dummy+(1 | coderyear) + (1 | countryname), data = merged_mpd_wb)
## Unemployment
markeco_unemployment <- lmer(markeco ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf + nl_dummy+SL.UEM.TOTL.ZS+(1 | coderyear) + (1 | countryname), data = merged_mpd_wb)
## GDP Per Capita
markeco_gdp_per_capita <- lmer(markeco ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf +nl_dummy+ NY.GDP.PCAP.PP.KD + (1 | coderyear) + (1 | countryname), data = merged_mpd_wb)
## EVS


# Run non-random effects model
nrr_markeco <- lm(markeco ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf+nl_dummy, data = merged_mpd_wb)

# Define labels for the dependent variable and covariates
dep_var_label <- "Dependent Variable: Party Manifesto Support for  Pro-Free Market"
covar_labels <- c("National Front", "Alternative for Germany", "Voice", "Danish People's Party", "Sweden Democrats", "Law and Justice", "Progress Party", "Brothers of Italy", "Fidesz", "Party Of Freedom","Unemployment", "GDP Per Capita")

# Generate summary table using stargazer
stargazer_market_economy<-stargazer(nrr_markeco, markeco_year, markeco_unemployment, markeco_gdp_per_capita,
    type = "html",
    title = "Party Position on Pro-Free Market Adjusted by Country and Year",
    dep.var.caption = dep_var_label,
    covariate.labels = covar_labels,
    column.labels = c("OLS", "Fixed For Year", "Unemployment", "GDP Per Capita")
)
display_html(stargazer_market_economy)

# %%
# fit the model
markeco_evs <- lmer(markeco ~ E036 + nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf + nl_dummy+(E036 | coderyear) + (E036 | partyname), data = merged_mpd_wb)
party_labels <- c("EVS Variable: E036", "National Front", "Alternative for Germany", "Voice", "Danish People's Party", "Sweden Democrats", "Law and Justice", "Progress Party", "Brothers of Italy", "Fidesz","Party For Freedom")

# Use stargazer to display the model summary
output_marketevs<-stargazer(markeco_evs,
          type = "html",
          title = "Pro Free Market EVS vs Party Manifesto Support For Free Market: Adjusted by Party and Year",
          dep.var.caption = "Dependent Variable: Party Manifesto Support for Pro-Free Market",
          covariate.labels = party_labels,
          column.labels = "European Values Study",
          omit.stat = c("f", "ser"),
             omit = c("Constant"),
         omit.labels = c("Constant"),
          out = "market_evs.md"
)
display_html(output_marketevs)


# %% [markdown]
# ##Free Trade Regression Adjusted by Country and Year
# 
# This is one last Regression dealing with Economic positions to see whether NRR Parties become more Protectionalist if the Un-Employment Rate is higher

# %%
protectionism_nrr <- lmer(per406 ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf+(1 | coderyear) + (1 | partyname), data = merged_mpd_wb)
protectionism_unemployment_gdp_per_capita <- lmer(per406 ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf  + hungary_nf+ SL.UEM.TOTL.ZS+NY.GDP.PCAP.PP.KD+(1 | coderyear) + (1 | partyname), data = merged_mpd_wb)

# %%
# Model 1: Without GDP Per Capita
model_data1 <- lmer(per406 ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf +nl_dummy+ (1 | coderyear) + (1 | countryname), data = merged_mpd_wb)

# Model 2: With Unemployment
model_data2 <- lmer(per406 ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf +nl_dummy+ SL.UEM.TOTL.ZS + (1 | coderyear) + (1 | countryname), data = merged_mpd_wb)

# Model 3: With GDP Per Capita
model_data3 <- lmer(per406 ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf +nl_dummy+ SL.UEM.TOTL.ZS +NY.GDP.PCAP.PP.KD+ (1 | coderyear) + (1 | countryname), data = merged_mpd_wb)

# Define covariate labels
covar_labels <- c("National Front", "Alternative for Germany", "Voice", "Danish People's Party", "Sweden Democrats", "Law and Justice", "Progress Party", "Brothers of Italy", "Fidesz", "Party Of Freedom", "Unemployment", "GDP Per Capita")

# Generate summary table with all three models using stargazer
protectionism_models<-stargazer(model_data1, model_data2,model_data3,
  type = "html",
  title = "Protectionism Regression Adjusted by Country and Year",
  dep.var.caption = "Dependent Variable: Party Manifesto Support for Protectionism",
  column.labels = c("Without GDP Per Capita", "With Unemployment", "With GDP Per Capita"),
  covariate.labels = covar_labels,
  omit.stat = c("f", "ser"),
  omit = "Constant"
)
display_html(protectionism_models)


# %% [markdown]
# ## Immigration and MultiCulturalism Manifesto Regression
# 
# This section will do Regression regarding  Multiculturalism and Immigration.

# %%
library(stargazer)

# Fit the models
multicultural_model <- lm(per608_2 ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf + nl_dummy, data = merged_mpd_wb)
mulitcultural_model_migration_rate <- lm(per608_2 ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf + nl_dummy + SM.POP.NETM, data = merged_mpd_wb)
mulitcultural_model_migration_fixed <- lmer(per608_2 ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf + nl_dummy + SM.POP.NETM + (1 | countryname) + (1 | coderyear), data = merged_mpd_wb)

# Define covariate labels
covar_labels <- c("National Front", "Alternative for Germany", "Voice", "Danish People's Party", "Sweden Democrats", "Law and Justice", "Progress Party", "Brothers of Italy", "Fidesz", "Party Of Freedom", "Migration Rate")

# Generate summary table for Model 1 and Model 2 using stargazer
# Store the HTML output from the first stargazer call
# Generate HTML output for the first model
# Generate HTML output for the first model
# Generate HTML output for the first model
model1_output <- stargazer(multicultural_model, 
                           type = "html", 
                           title = "NRR Parties and Anti-Multiculturalism", 
                           dep.var.caption = "Dependent Variable: Party Manifesto Against Multiculturalism", 
                           covariate.labels = covar_labels, 
                           column.labels = "Anti-Multiculturalism", 
                           omit.stat = c("f", "ser"), 
                           omit = "Constant", 
                           omit.labels = "Constant")

# Display HTML output for the first model
display_html(model1_output)

# Generate HTML output for the second model
model2_output <- stargazer(mulitcultural_model_migration_rate, 
                           type = "html", 
                           title = "NRR Parties and Anti-Multiculturalism", 
                           dep.var.caption = "Dependent Variable: Party Manifesto Against Multiculturalism", 
                           covariate.labels = covar_labels, 
                           column.labels = "Anti-Multiculturalism + Migration Rate", 
                           omit.stat = c("f", "ser"), 
                           omit = "Constant", 
                           omit.labels = "Constant")

# Display HTML output for the second model
display_html(model2_output)

# Generate HTML output for the third model
model3_output <- stargazer(mulitcultural_model_migration_fixed, 
                           type = "html", 
                           title = "NRR Parties and Anti-Multiculturalism", 
                           dep.var.caption = "Dependent Variable: Party Manifesto Against Multiculturalism", 
                           covariate.labels = covar_labels, 
                           column.labels = "Anti-Multiculturalism + Migration Rate (Fixed)", 
                           omit.stat = c("f", "ser"), 
                           omit = "Constant", 
                           omit.labels = "Constant")

# Display HTML output for the third model
display_html(model3_output)





# %%
library(stargazer)

# Perform rescaling
scaled_merged_mpd_wb <- merged_mpd_wb
scaled_merged_mpd_wb$SM.POP.NETM <- scale(merged_mpd_wb$SM.POP.NETM)

# Fit the models with rescaled continuous variable
anti_immigrant_model <- lm(per601_2 ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf + nl_dummy, data = scaled_merged_mpd_wb)
anti_immigrant_model_migration_rate <- lm(per601_2 ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf + nl_dummy + SM.POP.NETM, data = scaled_merged_mpd_wb)
anti_immigrant_model_migration_rate <- lm(per601_2 ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf + nl_dummy +SL.UEM.TOTL.ZS, data = scaled_merged_mpd_wb)
anti_immigrant_model_migration_rate_fixed <- lmer(per601_2 ~ nf_dummy + afd + spain_dummy + dk_dummy + sd_dummy + pd_dummy + pp_dummy + italy_nf + hungary_nf + nl_dummy+SL.UEM.TOTL.ZS + SM.POP.NETM + (1 | coderyear)+(1|countryname), data = scaled_merged_mpd_wb)

# Define covariate labels
covar_labels <- c("National Front", "Alternative for Germany", "Voice", "Danish People's Party", "Sweden Democrats", "Law and Justice", "Progress Party", "Brothers of Italy", "Fidesz", "Party Of Freedom","Unemployment", "Migration Rate")

# Generate HTML output for Model 1 and Model 2
model1_2_output <- stargazer(anti_immigrant_model, anti_immigrant_model_migration_rate,
                             type = "html",
                             title = "NRR Parties and Anti-Immigrant Sentiment",
                             dep.var.caption = "Dependent Variable: Party Manifesto Against Immigrants",
                             covariate.labels = covar_labels,
                             column.labels = c("Anti-Immigrant-Model", "Anti-Immigrant-Model-Migration-Rate"),
                             omit.stat = c("f", "ser"),
                             omit = "Constant",
                             omit.labels = "Constant")

# Display HTML output for Model 1 and Model 2
display_html(model1_2_output)

# Generate HTML output for Model 3
model3_output <- stargazer(anti_immigrant_model_migration_rate_fixed,
                           type = "html",
                           title = "NRR Parties and Anti-Immigrant Sentiment",
                           dep.var.caption = "Dependent Variable: Party Manifesto Against Immigrants",
                           covariate.labels = covar_labels,
                           column.labels = "Model Fixed by Country and Year",
                           omit.stat = c("f", "ser"),
                           omit = "Constant",
                           omit.labels = "Constant")

# Display HTML output for Model 3
display_html(model3_output)



