

#Explore Aggregated Outcomes
write(sprintf('\t# Approved (units) by National Funding       = [%i]', NROW(LoanPerformance[LoanPerformance$`Approved by National Funding Flag` == 'TRUE',])), stderr())

write(sprintf('\t# Funded (units) by National Funding         = [%i]', NROW(LoanPerformance[LoanPerformance$`National Funding Portfolio Flag` == 'TRUE',])), stderr())
write(sprintf('\t# Funded ($) by National Funding             = [%f]', sum(LoanPerformance$`Funded Amount`, na.rm = any(!is.na(LoanPerformance$`Funded Amount`)))), stderr())

write(sprintf('\t# National Funding Charged-Off (units)       = [%i]', sum(LoanPerformance$counter & LoanPerformance$`National Funding Charge Off Flag` == 'TRUE', na.rm = any(!is.na(LoanPerformance$`National Funding Charge Off Flag`)))), stderr())
write(sprintf('\t# Charged-Off ($) by National Funding        = [%f]', sum(LoanPerformance$`Principal Charge Off Amount`, na.rm = any(!is.na(LoanPerformance$`Principal Charge Off Amount`)))), stderr())


write(sprintf('\t# National Funding Active (units)            = [%i]', sum(LoanPerformance$counter & LoanPerformance$`National Funding Loan Status` == 'Active Loan', na.rm = any(!is.na(LoanPerformance$`National Funding Loan Status`)))), stderr())

write(sprintf('\t# National Funding Paid-in-Full (units)      = [%i]', sum(LoanPerformance$counter & LoanPerformance$`National Funding Loan Status` == 'Paid in Full', na.rm = any(!is.na(LoanPerformance$`National Funding Loan Status`)))), stderr())





# Subset data by application date
begin_date = as.Date('2014-09-01') 
end_date = as.Date('2017-05-31')
LoanPerformance$Time_Subset = ifelse(LoanPerformance$`Application Date` <= end_date & 
                                       LoanPerformance$`Application Date` >= begin_date,
                                     c("TRUE"),c("FALSE"))

LoanPerformance2 = LoanPerformance[LoanPerformance$Time_Subset == "TRUE", ]

# Funded by National Funding
LoanPerformance3 = LoanPerformance2[!is.na(LoanPerformance2$`National Funding Portfolio Flag`) & LoanPerformance2$`National Funding Portfolio Flag` == 'TRUE', ]


# Derive Features
LoanPerformance3 = GenDerived.Features(LoanPerformance3)




# Charged Off by National Funding
LoanPerformance4 = LoanPerformance3[!is.na(LoanPerformance3$`National Funding Charge Off Flag`) & LoanPerformance3$`National Funding Charge Off Flag` == 'TRUE', ]

# Active or Paid in Full 
LoanPerformance5 = LoanPerformance3[!is.na(LoanPerformance3$`National Funding Charge Off Flag`) & LoanPerformance3$`National Funding Charge Off Flag` == 'FALSE', ]






# Paid Principal Percentage Distribution for Charged Off Loans
table(LoanPerformance4$`Production Channel`)

ggplot(LoanPerformance4, aes(x=`Paid Principal Percentage`, group=`Production Channel`, fill= `Production Channel`))+
  geom_density(position="identity",alpha=0.5)+theme_bw()

quantile(LoanPerformance4$`Paid Principal Percentage`, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)


# Unit Charge-Off Rate for Population Subset
NROW(LoanPerformance4)/NROW(LoanPerformance3)

# Principal Charge-Off Rate for Population Subset
sum(LoanPerformance4$`Principal Charge Off Amount`) / sum(LoanPerformance3$`Funded Amount`)






# Univariate& Bivariate Analysis

colnames(LoanPerformance3)


# Annual Revenue (From SF Credit Review)
ggplot(LoanPerformance3, aes(x=`Annual Revenue`))+
  geom_density(position="identity",alpha=0.5)+xlim(c(0,10000000))+theme_bw()

quantile(LoanPerformance3$`Annual Revenue`, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)
quantile(LoanPerformance4$`Annual Revenue`, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)
quantile(LoanPerformance5$`Annual Revenue`, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)

ggplot(LoanPerformance3, aes(x=`Annual Revenue`, group=`CO`, fill= `CO`))+
  geom_density(position="identity",alpha=0.5)+xlim(c(0,10000000))+theme_bw()


# Average of average daily balance (From aggregated banks)
ggplot(LoanPerformance3, aes(x=`Average of Average Daily Balance`))+
  geom_density(position="identity",alpha=0.5)+xlim(c(0,100000))+theme_bw()

quantile(LoanPerformance3$`Average of Average Daily Balance`, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)
quantile(LoanPerformance4$`Average of Average Daily Balance`, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)
quantile(LoanPerformance5$`Average of Average Daily Balance`, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)

ggplot(LoanPerformance3, aes(x=`Average of Average Daily Balance`, group=`CO`, fill= `CO`))+
  geom_density(position="identity",alpha=0.5)+xlim(c(0,100000))+theme_bw()



# Max FICO (From SF Credit Review)
ggplot(LoanPerformance3, aes(x=`CR Max FICO`))+
  geom_density(position="identity",alpha=0.5)+xlim(c(400,850))+theme_bw()

quantile(LoanPerformance3$`CR Max FICO`, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)
quantile(LoanPerformance4$`CR Max FICO`, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)
quantile(LoanPerformance5$`CR Max FICO`, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)

ggplot(LoanPerformance3, aes(x=`CR Max FICO`, group=`CO`, fill= `CO`))+
  geom_density(position="identity",alpha=0.5)+xlim(c(400,850))+theme_bw()



# Min FICO (derived)
ggplot(LoanPerformance3, aes(x=MinFICO))+
  geom_density(position="identity",alpha=0.5)+xlim(c(400,850))+theme_bw()

quantile(LoanPerformance3$MinFICO, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)
quantile(LoanPerformance4$MinFICO, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)
quantile(LoanPerformance5$MinFICO, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)

ggplot(LoanPerformance3, aes(x=MinFICO, group=`CO`, fill= `CO`))+
  geom_density(position="identity",alpha=0.5)+xlim(c(400,850))+theme_bw()




# Deposit Volatility (From aggregated banks)
ggplot(LoanPerformance3, aes(x=`Deposit Volatility`))+
  geom_density(position="identity",alpha=0.5)+theme_bw()

quantile(LoanPerformance3$`Deposit Volatility`, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)
quantile(LoanPerformance4$`Deposit Volatility`, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)
quantile(LoanPerformance5$`Deposit Volatility`, prob = c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99), na.rm = TRUE)

ggplot(LoanPerformance3, aes(x=`Deposit Volatility`, group=`CO`, fill= `CO`))+
  geom_density(position="identity",alpha=0.5)+theme_bw()





