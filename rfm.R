#https://rpubs.com/GiftMtambo/870744
library(stringr)
library(rfm)
library(lubridate)

setwd("your directory")
rfm<-read.csv("rfm.csv", header =TRUE, sep=",")

rfm<-rfm%>%
  rename(customer_id = #your column#,
         client_name = #your column#,
         invoice_id = #your column#,
         invoice_date = #your column#,
         revenue = #your column# 
           )


analysis_date <- max(as.Date(rfm$invoice_date))+days(1)

rfm$invoice_date<-as.Date(rfm$invoice_date)

recency <- rfm %>% 
  dplyr::select(customer_id, invoice_date) %>% 
  mutate(recency = analysis_date - invoice_date)

recency <- recency %>% 
  dplyr::select(customer_id, recency) %>% 
  group_by(customer_id) %>% 
  slice(which.min(recency))

#freq
amount_products <- rfm %>%
  dplyr::select(customer_id, invoice_date) %>% 
  group_by(customer_id, invoice_date) %>% 
  summarize(n_prod = n())

df_frequency <- amount_products %>% 
  dplyr::select(customer_id) %>%
  group_by(customer_id) %>% 
  summarize(frequency = n())

monetary <- rfm %>%
  group_by(customer_id) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE))

rfmcombo <- recency %>% 
  dplyr::inner_join(., df_frequency, by = "customer_id") %>% 
  dplyr::inner_join(., monetary, by = "customer_id")

# drop the days from recency column and transform it into numeric data type
rfmcombo2 <- rfmcombo %>% 
  mutate(recency = str_replace(recency, " days", "")) %>% 
  mutate(recency = as.numeric(recency)) %>% 
  ungroup()

recency_s <- rfm %>% 
  dplyr::select(customer_id, invoice_date) %>% 
  group_by(customer_id) %>% 
  slice(which.max(invoice_date))

rfm_test <- rfmcombo2 %>% inner_join(recency_s, by = "customer_id")

rfm_result <- rfm_table_customer(rfm_test, customer_id, frequency, recency, revenue, analysis_date)

rfm_datatable <- rfm_result$rfm

distinct_clients <- rfm %>%
  distinct(customer_id, client_name)

rfm_datatable_with_names <- rfm_datatable %>%
  left_join(distinct_clients %>% select(customer_id,client_name), by = c("customer_id" = "customer_id"))

classify_rfm <- function(rfm) {
  rfm <- rfm %>%
    mutate(Segment = case_when(
      recency_score >= 4 & frequency_score >= 4 & monetary_score >= 4 ~ "Champions",
      recency_score >= 2 & frequency_score >= 3 & monetary_score >= 3 ~ "Loyal Customers",
      recency_score >= 3 & frequency_score >= 1 & frequency_score <= 3 & monetary_score >= 1 & monetary_score <= 3 ~ "Potential Loyalist",
      recency_score >= 4 & frequency_score <= 1 & monetary_score <= 1 ~ "New Customers",
      recency_score >= 3 & recency_score <= 4 & frequency_score <= 1 & monetary_score <= 1 ~ "Promising",
      recency_score >= 2 & recency_score <= 3 & frequency_score >= 2 & frequency_score <= 3 & monetary_score >= 2 & monetary_score <= 3 ~ "Need Attention",
      recency_score <= 2 & frequency_score <= 2 & monetary_score <= 2 ~ "About To Sleep",
      recency_score <= 2 & frequency_score >= 2 & frequency_score <= 5 & monetary_score >= 2 & monetary_score <= 5 ~ "At Risk",
      recency_score <= 2 & frequency_score >= 4 & monetary_score >= 4 ~ "Can't Lose Them",
      recency_score <= 2 & frequency_score <= 1 & monetary_score <= 1 ~ "Hibernating",
      recency_score <= 2 & frequency_score <= 2 & monetary_score <= 2 ~ "Lost",
      TRUE ~ "Other"
    ))
  return(rfm)
}

# Apply the classification function
rfm_classified <- classify_rfm(rfm_datatable_with_names)

write.csv(rfm_classified, "rfmclassified.csv", row.names = FALSE)



#rm(list = ls())