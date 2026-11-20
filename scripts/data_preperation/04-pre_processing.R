# data preperation

library(tidyverse)
library(tsibble)
library(feasts)



# read data ---------------------------------------------------------------

stock_tidy <- read_rds('data/tidy_data/stock_tidy.rds')
comments_cluster <- read.csv('data/tidy_data/comments_cluster_df.csv')


# check cluster quality and refine ----------------------------------------

## check for duplicates 

stock_tidy |> 
  duplicated() |> 
  sum()

## remove duplicates

stock_tidy <- stock_tidy |> 
  distinct()

## join comments and stock dfs

stock_updated <- stock_tidy |> 
  left_join(comments_cluster |> 
              select(-cleaned_comments),
            by = 'comments')


## clean standard sentences

stock_updated <- stock_updated |> 
  
  mutate(standardized_comments = if_else(standardized_comments == 'urgent_need' & str_detect(translated_comments, "\\bno delivery\\b|\\bordered in progress\\b|\\bordered 0\\b|\\brequested transfer\\b"),
                                         'facility_issues', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'urgent_need' & str_detect(translated_comments, "\\bnot requested\\b|\\bno requested\\b|\\bno csr requested\\b"),
                                         'low_demand', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'urgent_need' & grepl("quantity requested", translated_comments, ignore.case = TRUE),
                                         'normal_consumption', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'urgent_need' & grepl("suggested quantity ordered", translated_comments, ignore.case = TRUE),
                                         'nothing_to_report', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'urgent_need' & grepl("insufficient", translated_comments, ignore.case = TRUE),
                                         'insufficient_stock', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'urgent_need' & str_detect(translated_comments, "\\bdelivery very\\b|\\bdelivery before\\b|\\bwaiting period\\b"),
                                         'delivery_time', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'urgent_need' & str_detect(translated_comments, "\\brequested by\\b|\\brequested family\\b"),
                                         'staff_request', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'urgent_need' & str_detect(translated_comments, "\\bhighly requested\\b|\\brequested high\\b|\\brequested increase\\b|\\brequested is strong\\b"),
                                         'high_consumption', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'urgent_need' & grepl("set up", translated_comments, ignore.case = TRUE),
                                         'new_product', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'urgent_need' & stock_ordered <= stock_required,
                                         'nothing_to_report', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'real_cmm_adjustment' & grepl("not be delivered", translated_comments, ignore.case = TRUE),
                                         'delivery_time', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'real_cmm_adjustment' & grepl("nothing to report", translated_comments, ignore.case = TRUE),
                                         'nothing_to_report', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'sufficient_stock' & str_detect(translated_comments, "\\bunavailable\\b|\\brequested family\\b|\\binsufficient\\b|\\bnot available\\b"), 
                                         'insufficient_stock', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'sufficient_stock' & str_detect(translated_comments, "\\bstored product\\b|\\bno requested\\b|\\bno csr requested\\b"),
                                         'low_demand', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'sufficient_stock' & str_detect(translated_comments, "\\baibef\\b|\\bin pf\\b|\\San Pedro\\b"),
                                         'facility_issues', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'sufficient_stock' & str_detect(translated_comments, "\\baverage stock\\b|\\binitial stock\\b|\\bnormal stock\\b|\\bperfect stock\\b"),
                                         'nothing_to_report', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'sufficient_stock' & str_detect(translated_comments, "\\bsufficient\\b") & stock_ordered > 0,
                                         'nothing_to_report', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'sufficient_stock' & stock_ordered > 0,
                                         'nothing_to_report', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'normal_consumption' & str_detect(translated_comments, "\\baibef\\b|\\bAIBEF\\b|\\bnpsp\\b"),
                                         'facility_issues', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'facility_issues' & str_detect(translated_comments, "\\btraining\\b|\\buntrained \\b|\\btrained\\b"),
                                         'training_staff_competency', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'facility_issues' & str_detect(translated_comments, "\\bsite storage capacity\\b"),
                                         'normal_consumption', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'facility_issues' & str_detect(translated_comments, "\\bexist in hgg\\b|\\bdistrict made\\b"),
                                         'normal_consumption', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'other_unspecified_comments' & str_detect(translated_comments, "\\bmidwives\\b|\\bto serve please\\b|\\bgynecologists\\b|\\bdoctors\\b"),
                                         'staff_request', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'other_unspecified_comments' & str_detect(translated_comments, "\\blow product rotation\\b"),
                                         'low_demand', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'other_unspecified_comments' & str_detect(translated_comments, "\\bstock outage\\b"),
                                         'out_of_stock', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'other_unspecified_comments' & str_detect(translated_comments, "\\bincreasing consumption\\b|\\bdemand is high\\b"),
                                         'high_consumption', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'other_unspecified_comments' & str_detect(translated_comments, "\\bdecreased consumption\\b"),
                                         'low_consumption', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'other_unspecified_comments' & str_detect(translated_comments, "\\bc 33\\b"),
                                         'real_cmm_adjustment', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'other_unspecified_comments' & str_detect(translated_comments, "\\basked strongly\\b"),
                                         'urgent_need', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'cmm_reporting_in_packaging_units' & str_detect(translated_comments, "\\bnothing to report\\b") & stock_ordered > 0,
                                         'nothing_to_report', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'other_unspecified_comments' & str_detect(translated_comments, "\\bstock needed\\b|\\bneed\\b"),
                                         'urgent_need', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'immediate_breakup' & str_detect(translated_comments, "\\bwe order in term\\b|\\bboxes\\b"),
                                         'nothing_to_report', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'immediate_breakup' & str_detect(translated_comments, "\\bcash\\b|\\bprice\\b"),
                                         'pricing_cash_issues', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'order_not_delivered' & str_detect(translated_comments, "\\baibef\\b|\\bwaiting for delivery\\b|\\bdistrict\\b|\\bnever received\\b|\\balready delivered\\b|\\bnpsp\\b|\\bdelivered this\\b|\\bdelivered in\\b"),
                                         'facility_issues', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'order_not_delivered' & str_detect(translated_comments, "\\bdelivery time\\b|\\bnext delivery date\\b"),
                                         'delivery_time', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'positive_inventory_fix' & stock_ordered == 0 & stock_required > 0,
                                         'facility_issues', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'high_consumption' & str_detect(translated_comments, "\\bin stock and not in high demand\\b"),
                                         'sufficient_stock', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'high_consumption' & str_detect(translated_comments, "\\bnot prescribed\\b"),
                                         'low_consumption', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'high_consumption' & str_detect(translated_comments, "\\bprescribed product\\b"),
                                         'staff_request', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'transfer_in' & str_detect(translated_comments, "\\btransfer_out\\b"),
                                         'transfer_out', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'transfer_in' & stock_ordered == 0 & stock_required > 0,
                                         'facility_issues', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'transfer_in' & str_detect(translated_comments, "\\bdata not received\\b"),
                                         'facility_issues', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'low_demand' & str_detect(translated_comments, "\\binsufficient stock\\b"),
                                         'insufficient_stock', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'low_demand' & str_detect(translated_comments, "\\bsufficient stock\\b"),
                                         'sufficient_stock', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'cmm_related_events' & str_detect(translated_comments, "\\bfirst ordered\\b"),
                                         'new_product', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'high_stock' & stock_ordered > 0 & stock_ordered > stock_required,
                                         'high_consumption', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'high_stock' & stock_ordered > 0 & stock_ordered <= stock_required,
                                         'normal_consumption', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'specific_break_period',
                                         'facility_issues', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'revalued_stock',
                                         'other_unspecified_comments', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'atypical_consumption' & stock_ordered == 0,
                                         'nothing_to_report', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'demand_adjustment' & str_detect(translated_comments, "\\bneeds\\b"),
                                         'high_consumption', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'demand_adjustment' & str_detect(translated_comments, "\\benters\\b"),
                                         'new_product', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'long_breakup',
                                         'new_product', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'long_term_breakup',
                                         'new_product', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'demand_patterns',
                                         'high_consumption', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'facility_issues' & str_detect(translated_comments, "\\bno delivery\\b|\\bdelivered in may\\b|substock\\b"),
                                         'sufficient_stock', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'facility_issues' & stock_ordered > 0,
                                         'order_not_delivered', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'facility_issues' & stock_ordered > 0,
                                         'order_not_delivered', standardized_comments),
         
         stock_ordered = if_else(is.na(stock_ordered), stock_required, stock_ordered),
         
         standardized_comments = if_else(standardized_comments == 'calculated_expression_adjustment' & str_detect(translated_comments, "\\bordered qty\\b"),
                                         'nothing_to_report', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'calculated_expression_adjustment' & stock_ordered == 0,
                                         'sufficient_stock', standardized_comments),
         standardized_comments = if_else(standardized_comments == 'calculated_expression_adjustment' & str_detect(translated_comments, "\\b2 months ordered\\b|\\b2 months ago\\b|\\bordered for 2 months\\b|\\bordered for two months\\b|\\bquantity for 2 months\\b|\\btwo months ordered\\b|\\bordered two months\\b|\\bordered for 02 months\\b|\\bdouble\\b|\\bdoubled\\b|\\bbimonthly\\b"),
                                         'delivery_time', standardized_comments),
         
         standardized_comments = if_else(standardized_comments == 'risk_of_rupture',
                                         'out_of_stock', standardized_comments),
         
         standardized_comments = if_else(is.na(standardized_comments),
                                         'nothing_to_report', standardized_comments)
         )


## view comments

stock_updated |> 
  select(cluster) |> 
  unique() |> 
  view()

# Handle cmm adjustments --------------------------------------------------

# extract cmm from comments (cmm means average monthly consumption)

stock_updated <- stock_updated |> 
  mutate(corrected_cmm = if_else(str_detect(translated_comments, "equation"), as.numeric(str_extract(translated_comments, "\\d+(?=\\*)")), NA),
         corrected_cmm = if_else(str_detect(standardized_comments, "positive_inventory_fix"), as.numeric(str_extract(translated_comments, "(?<=\\(\\()\\d+(?=\\*)")), NA),
         corrected_cmm = if_else(is.na(corrected_cmm), (stock_ordered + stock_end)/3, corrected_cmm),
         corrected_cmm = if_else(corrected_cmm < 0 | is.na(corrected_cmm), projected_average_monthly_consumption, corrected_cmm),
         corrected_cmm = round(if_else(str_detect(translated_comments, "\\bmultiplied\\b|\\btwo months\\b|\\b2 months\\b"), projected_average_monthly_consumption, corrected_cmm)),
         
         standardized_comments = case_when(standardized_comments == 'real_cmm_adjustment' & stock_ordered > stock_required ~ 'positive_correction',
                                           standardized_comments == 'real_cmm_adjustment' & stock_ordered < stock_required  ~ 'negative_correction',
                                           standardized_comments == 'real_cmm_adjustment' & stock_ordered == stock_required ~ 'nothing_to_report',
                                           
                                           standardized_comments == 'order_adjustment' & stock_ordered > stock_required ~ 'insufficient_stock',
                                           standardized_comments == 'order_adjustment' & stock_ordered < stock_required  ~ 'low_consumption',
                                           standardized_comments == 'order_adjustment' & stock_ordered == stock_required ~ 'nothing_to_report',
                                           
                                           .default = standardized_comments))

# stock_updated <- stock_updated |> 
#   mutate(corrected_cmm = if_else(str_detect(translated_comments, "cmm"),
#                                  as.numeric(str_extract(str_replace(translated_comments, ".*cmm\\D*", ""), "\\d+")),
#                                  NA), # extract actual monthly consumption based on the comments
#          corrected_cmm = if_else(corrected_cmm == 0, stock_ordered > 0, (stock_ordered + stock_end)/3),
#          cmm_equation = (stock_ordered + stock_end)/3,
#          corrected_cmm = if_else(str_detect(translated_comments, "cmm in"), NA, corrected_cmm), # quality checking
#          corrected_cmm = if_else(str_detect(translated_comments, "equation"), as.numeric(str_extract(translated_comments, "\\d+(?=\\*)")), corrected_cmm),
#          corrected_cmm = if_else(str_detect(standardized_comments, "positive_inventory_fix"), as.numeric(str_extract(translated_comments, "(?<=\\(\\()\\d+(?=\\*)")), corrected_cmm),
#          corrected_cmm = if_else(is.na(corrected_cmm), (stock_ordered + stock_end)/3, corrected_cmm),
#          corrected_cmm = if_else(corrected_cmm < 0 | is.na(corrected_cmm), projected_average_monthly_consumption, corrected_cmm),
#          corrected_cmm = round(if_else(str_detect(translated_comments, "\\bmultiplied\\b|\\btwo months\\b|\\b2 months\\b"), projected_average_monthly_consumption, corrected_cmm))),
# 
# standardized_comments = case_when(standardized_comments == 'real_cmm_adjustment' & stock_ordered > stock_required ~ 'insufficient_stock',
#                                   standardized_comments == 'real_cmm_adjustment' & stock_ordered < stock_required  ~ 'low_consumption',
#                                   standardized_comments == 'real_cmm_adjustment' & stock_ordered == stock_required ~ 'nothing_to_report',
#                                   
#                                   standardized_comments == 'order_adjustment' & stock_ordered > stock_required ~ 'insufficient_stock',
#                                   standardized_comments == 'order_adjustment' & stock_ordered < stock_required  ~ 'low_consumption',
#                                   standardized_comments == 'order_adjustment' & stock_ordered == stock_required ~ 'nothing_to_report',
#                                   
#                                   .default = standardized_comments))
         
# stock indicators ------------------------------------------------------

# creating stock out indicator

stock_updated |> 
  filter(stock_initial > 0, stock_end > 0, stock_distributed == 0) |> 
  pull(standardized_comments) |> 
  unique()

stock_updated <- stock_updated |> 
  mutate(
    stock_out_indicator = case_when(stock_stockout_days > 0 ~ 1,
                                    standardized_comments == 'out_of_stock' ~ 1,
                                    (stock_initial >= 0 & stock_end == 0 & stock_ordered > 0) &
                                      standardized_comments %in% c('atypical_consumption', 'high_consumption', 'urgent_need', 'delivery_time', 'calculated_expression_adjustment', 
                                                                   'staff_request', 'insufficient_stock', 'avoid_breakup', 'normal_consumption') ~ 1,
                                    .default = 0),
    interruption_indicator = case_when(standardized_comments == 'facility_issues' & stock_initial > 0 & stock_distributed == 0 ~ 1,
                                       (stock_initial > 0 & stock_end > 0 & stock_distributed == 0) & 
                                         standardized_comments %in% c('insufficient_stock', 'high_consumption', 'urgent_need', 'staff_request', 'calculated_expression_adjustment', 'order_not_delivered') ~ 1,
                                    stock_out_indicator == 1 & stock_distributed == 0 ~ 1,
                                    .default = 0)
    )



# create new clusters -----------------------------------------------------

stock_updated <- stock_updated |> 
  mutate(
    cluster = case_when(standardized_comments %in% c('normal_consumption', 'nothing_to_report') ~ 'nothing_to_report',
                        standardized_comments %in% c('out_of_stock', 'insufficient_stock', 'avoid_breakup', 'availability_period_consideration') ~ 'stock_out_risk',
                        standardized_comments %in% c('low_consumption', 'low_turnover', 'low_demand', 'dormant_stock') ~ 'low_consumption',
                        standardized_comments %in% c('facility_issues', 'reorder_issues', 'pricing_cash_issues', 'training_staff_competency') ~ 'facility_&_staff_issues',
                        standardized_comments %in% c('sufficient_stock', 'overstock', 'high_stock') ~ 'sufficient_stock',
                        standardized_comments %in% c('staff_request', 'urgent_need', 'order_not_delivered') ~ 'urgent_need',
                        standardized_comments %in% c('positive_inventory_fix', 'transfer_in', 'transfer_out', 'safety_stock') ~ 'inventory_actions',
                        standardized_comments %in% c('conversion_error', 'stock_conversion', 'cmm_reporting_in_packaging_units') ~ 'stock_conversion',
                        standardized_comments %in% c('immediate_breakup', 'pre_breakup') ~ 'breakup',
                        standardized_comments %in% c('atypical_consumption', 'high_consumption') ~ 'high_consumption',
                        standardized_comments %in% c('cmm_related_events', 'holiday_season') ~ 'special_events',
                        .default = standardized_comments),
    
    cluster = if_else(stock_out_indicator == 1, 'stock_out_risk', cluster),
    stock_out_indicator = if_else(interruption_indicator == 1, 0, stock_out_indicator),
    high_consumption_indicator = if_else(standardized_comments == 'high_consumption', 1, 0),
    low_consumption_indicator = if_else(standardized_comments == 'low_consumption', 1, 0)
  )


# data exploration  -------------------------------------------------------

# create a tsibble

stock_tsb <- stock_updated |> 
  select(-product_unit) |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date, key = c(region, district, site, site_code, product_type, product_name, product_code))

# check for temporal gaps

count_gaps(stock_tsb) |> view()

# check for time series length

id_list <- stock_tsb |> 
  as_tibble() |> 
  group_by(site_code, product_code) |> 
  mutate(length = n(),
         id = paste0(site_code, '-', product_code)) |> 
  select(id, length) |> 
  unique() |> 
  filter(length < 40) |>
  pull(id)


# drop time series with less than 40 observations

stock_tsb_filtered <- stock_tsb |> 
  mutate(id = paste0(site_code, '-', product_code)) |> 
  filter(!id %in% id_list) |> 
  fill_gaps(.full = end())


# seems data is not reported during the period. lets keep last observed stock end as the stock initial

stock_tsb_filtered <- stock_tsb_filtered |> 
  mutate(id = paste0(site_code, '-', product_code)) |> 
  group_by(id) |> 
  fill(stock_initial, .direction = 'down') |> 
  mutate(
    stock_distributed = ifelse(is.na(stock_distributed), 0, stock_distributed),
    stock_end = ifelse(is.na(stock_end), stock_initial, stock_end),
    stock_receieved = ifelse(is.na(stock_receieved), 0, stock_receieved),
    stock_adjustment = ifelse(is.na(stock_adjustment), (stock_end - stock_initial - stock_receieved + stock_distributed), stock_adjustment),
    projected_average_monthly_consumption = ifelse(is.na(projected_average_monthly_consumption), 0, projected_average_monthly_consumption),
    stock_required = ifelse(is.na(stock_required), 0, stock_required),
    stock_ordered = ifelse(is.na(stock_ordered), 0, stock_ordered),
    stock_stockout_days = ifelse(is.na(stock_stockout_days), 0, stock_stockout_days),
    
    comments = ifelse(is.na(comments), 'facility_&_staff_issues', comments),
    translated_comments = ifelse(is.na(translated_comments), 'facility_&_staff_issues', translated_comments),
    standardized_comments = ifelse(is.na(standardized_comments), 'facility_&_staff_issues', standardized_comments),
    cluster = ifelse(is.na(cluster), 'facility_&_staff_issues', cluster),
    
    corrected_cmm = ifelse(is.na(corrected_cmm), 0, corrected_cmm),
    stock_out_indicator = ifelse(is.na(stock_out_indicator), 0, stock_out_indicator),
    interruption_indicator = ifelse(is.na(interruption_indicator), 1, interruption_indicator)
  ) |> 
  fill(high_consumption_indicator, .direction = 'down') |> 
  fill(low_consumption_indicator, .direction = 'down') |> 
  ungroup()

write.csv(stock_tsb_filtered, 'data/tidy_data/stock_tsb_filtered.csv', row.names = FALSE)

write_rds(stock_tsb_filtered, 'data/tidy_data/stock_tsb_filtered.rds')
  



