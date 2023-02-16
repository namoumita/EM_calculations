 ***H1: Firm Earnings Management is related to country governance in NORTH EUROPEAN COUNTRIES***

***Import Command Nafisa***
. import excel "C:\Users\HP\Downloads\DB Europe 3.4.2021.xlsx", sheet("STATA DB") firstrow


*** SET PANEL DATA ***
. xtset Iden Year

. set more off

*** REMOVAL FROM THE DATA SET OBSERVATIONS FROM NON EUROPEAN COUNTRIES ***
. drop if CountryofHeadquarters=="Bermuda"
. drop if CountryofHeadquarters=="Faroe Islands"
. drop if CountryofHeadquarters=="Mexico"
. drop if CountryofHeadquarters=="Singapore"
. drop if CountryofHeadquarters=="United States"
. drop if CountryofHeadquarters=="Russia"

*** REMOVAL FROM THE DATA SET OBSERVATIONS FROM NON NORTH EUROPEAN COUNTRIES***
. drop if CountryofHeadquarters=="Bulgaria"
. drop if CountryofHeadquarters=="Croatia"
. drop if CountryofHeadquarters=="Czech Republic"
. drop if CountryofHeadquarters=="France"
. drop if CountryofHeadquarters=="Hungary"
. drop if CountryofHeadquarters=="Italy"
. drop if CountryofHeadquarters=="Portugal"
. drop if CountryofHeadquarters=="Slovenia"
. drop if CountryofHeadquarters=="Spain"
. drop if CountryofHeadquarters=="Switzerland"

***REMOVAL OF COUNTRIES WITH NO OUTPUT FROM THE DATASET***
. drop if CountryofHeadquarters=="Austria"
. drop if CountryofHeadquarters=="Iceland"


*** REMOVAL FROM THE DATA SET FINANCIAL COMPANIES LIKE BANKS (WE JUST WANT TO WORK WITH NON-FINANCIAL FIRMS)***
. drop if GICSSectorCode==40
. drop if TRBCEconomicSector=="Financials"

*** LABLE VARIABLES WITH MISSING LABLES ***
. label variable region "WB region"

. label variable incomegr "WB Income Group"

. label variable dbacba "Deposits Money Bank Assets to (Deposit + Central) Bank Assets"

. label variable llgdp "Liquid Liabilities to GDP (%)"

. label variable cbagdp "Central Bank Assets to GDP (%)"

. label variable dbagdp "Deposit Money Bank Assets to GDP (%)"

. label variable ofagdp "Other Financial Institions Assets to GDP (%)"

. label variable pcrdbgdp "Private Credit by Depoist Moeny Banks to GDP (%)"

. label variable pcrdbofgdp "Private Credit by Deposit Money Banks and Other Financial Instituions to GDP (%)"

. label variable bdgdp "Bank Deposits to GDP (%)"

. label variable fdgdp "Financial Systems Deposit to GDP (%)"

. label variable bcbd "Bank Credit to Bank Deposits (%)"

. label variable ll_usd "Liquid Liabilities (in Mil. 2010 USD)"

. label variable overhead "Bank Overhead Costs to Toal Assets (%)"

. label variable netintmargin "Net Interest Margin (%)"

. label variable concentration "Bank Concentration (%)"

. label variable roa "Bank ROA"

. label variable roe "Bank ROE"

. label variable costinc "Bank Cost to Income Ratio (%)"

. label variable zscore "Bank Z-Score"

. label variable inslife "Life Insurance Premium Volume to GDP (%)"

. label variable insnonlife "Non-Life Insurance Premium Volume to GDP (%)"

. label variable stmktcap "Stock Market Capitalization to GDP (%)"

. label variable stvaltraded "Stock Market Total Value Traded to GDP (%)"

. label variable stturnover "Stock Market Turnover Ratio (%)"

. label variable listco_pc "No of Listed Companies per 10k Population"

. label variable prbond "Private Bond Market Capitalization to GDP (%)"

. label variable pubond "Private Bond Market Capitalization to GDP (%)"

. label variable pubond "Public Bond Market Capitalization to GDP (%)"

. label variable intldebt "International Debt Issues to GDP (%)"

. label variable intldebtnet "Loans from Non-Resident Banks (Net) to GDP (%)"

. label variable nrbloan "Loans from Non-Resident Banks"

. label variable nrbloan "Loans from Non-Resident Banks (AMT Outstanding) to GDP (%) "

. label variable offdep "Offshore Bank Deposits to Domestic Bank"

. label variable offdep "Offshore Bank Deposits to Domestic Bank Deposits (%)"

. label variable remit "Remitance Inflows to GDP (%)"

. label variable VA "Voice and Accountability"

. label variable PS "Political Stability or No-Violence"

. label variable GE "Government Effectivenes"

. label variable RQ "Regulatory Quality"

. label variable RL "Rule of Law"

. label variable CC "Control of Corruption"

. label variable GGG "Global Gender Gap Index"


***COMPUTATION OF CONTROL VARIABLES***

***Total Asset Turnover
. gen TAT= TotalRevenue/TotalAssetsReported
. label variable TAT "Total Asset Turnover"
. histogram TAT, normal

. gen BoardSize1 = ln(BoardSize)
. label variable BoardSize1 "ln(BoardSize)"
. histogram BoardSize1, normal

. gen size = ln(TotalAssetsReported) if TotalEquity>0
   ***ln(TotalAssetsReported) if TotalEquity>0
. label variable size "Size"
. histogram size, normal

. generate lev1 = TotalDebt / TotalAssetsReported if TotalEquity>0
  ***TotalDebt / TotalAssetsReported if TotalEquity>0
. label variable lev1 "Leverage" 
. swilk lev1
. histogram lev1, normal

. generate ROA = NetIncomeAfterTaxes/ TotalAssetsReported if TotalEquity>0
. label variable ROA "NI / TA"
. winsor ROA, generate(ROA_w1) p(0.01)
. winsor2 ROA, suffix(_w2) cuts(1 99) trim
. label variable ROA_w2 "Profitability, Trimmed faction 0.01"
. histogram ROA_w2, normal

. egen gicsin = group(TRBCEconomicSector)
. tabulate TRBCEconomicSector

. egen c = group(CountryofHeadquarters)
. tabulate CountryofHeadquarters 
. order c, before(CountryofHeadquarters)


***COMPUTING DEPENDENT VARIABLES***
. gen delta_CA = d.TotalCurrentAssets
. label variable delta_CA "Annual change in total current assets"

. gen delta_Cash = d.CashAndSTInvestments
. label variable delta_Cash "Change in cash and equivalent"

. gen delta_CL = d.TotalCurrLiabilities
. label variable delta_CL "Change in total current liabilities"

. gen delta_STD = d.NotesPayableSTDebt
. label variable delta_STD "Change in short-term debt"

. gen delta_REV = d.TotalRevenue
. label variable delta_REV "Change in total revenue"

. gen delta_AR = d.TotalReceivablesNet
. label variable delta_AR "Change in accounts receivable"

. gen delta_Dep = d.DepreciationAmort
. label variable delta_Dep "Change in Depreciation Amortisation"

. gen lagged_assets = l.TotalAssetsReported
. label variable lagged_assets "One-period lagged value of total assets"

. gen ACC_m1 = (delta_CA - delta_Cash) - (delta_CL - delta_STD) - delta_Dep
. label variable ACC_m1 "Total Accruals in Model-1"


***Calculating the first dependant variable Jones Model(1995)***
. gen term_1 = 1/lagged_assets
. gen term_2 = delta_REV/ lagged_assets
. gen term_3 = (delta_REV - delta_AR) / lagged_assets
. gen term_4 = PropertyPlantEquipmentTotalNet / lagged_assets

. gen TACC_2 = (NetIncomeAfterTaxes - CashFromOperatingAct)/ lagged_assets

. gen NDA =.
. reg TACC_2 term_1 term_3 term_4, noconstant
. predict r, resid
. replace NDA=r
. drop r 

***Discretionary Accruals***
. gen DA = (ACC_m1/lagged_assets) - NDA

. winsor DA, generate (DA_w1) p(0.01)
. winsor2 DA, suffix(_w2) cuts(2 98) trim
. label variable DA_w2 "Earnings Management-1 Winsorized @2%(Trimmed)"
. histogram DA_w2, normal

***Absolute Discretionary Accruals***
. gen ABS_DA = abs(DA_w2)
. histogram ABS_DA, normal


***Calculating the second dependant variable Kothari Model(2005).***

. gen lagged_ROA = l.ROA_w2
. label variable lagged_ROA "One-period lagged value of total return on assets"

. gen EM2 =.
. regress TACC_2 term_1 term_3 term_4 lagged_ROA, noconstant
. predict r, resid
. replace EM2=r 
. drop r 

. winsor EM2, generate (EM2_w1) p(0.01)
. winsor2 EM2, suffix(_w2) cuts(2 98) trim
. label variable EM2_w2 "Earnings Management2 Winsorized @2%(Trimmed)"
. histogram EM2_w2, normal



***COMPUTATION OF INDEPENDENT VARIABLES***
   **COUNTRY GOVERNANCE INDEX (0-1)**
. correlate VA PS GE RQ RL CC
. alpha VA PS GE RQ RL CC, std

** Range of each governance index is -2.5 to 2.5. So, to neutralize that efffect 2.5 is being added to each index (2.5 * 6 = 15) and then divided by total effect of (12 * 2.5 = 30) for CGI calculation.**
. generate CGI = (VA + PS + GE + RQ + RL + CC + 15)/30		
. label variable CGI "Country Governance Index (0-1)"
. order CGI, before(VA)
. histogram CGI, normal



. factor VA PS GE RQ RL CC, pcf
. rotate

*** Since two factors have Eigenvalue>1 you have to predict two, not one. Hence, I modified your predict command and the subsequents for two factors ***
. predict factorcgi1 factorcgi2 
. summarize factorcgi1 factorcgi2
. correlate VA PS GE RQ RL CC factorcgi1 factorcgi2
. label variable factorcgi1 "Factor1 Country Governance Index"
. label variable factorcgi2 "Factor2 Country Governance Index"
. order factorcgi1 factorcgi2, before(CGI)



***DESCRIPTIVE STATISICS***
. summarize ABS_DA EM2_w2 CGI TAT BoardSize1 ROA_w2 lev1 size 
. asdoc summarize ABS_DA EM2_w2 CGI TAT BoardSize1 ROA_w2 lev1 size, replace label tzok dec(3) save(Statistics CGI)

. tabstat ABS_DA, statistics(N mean median sd min max) by ( CountryofHeadquarters ) format(%9.2fc)
. bysort CountryofHeadquarters: asdoc tabstat ABS_DA, stat(N mean median sd p25 p75) append label tzok dec(3) save(Statistics CGI)

. tabstat EM2_w2, statistics(N mean median sd min max) by ( CountryofHeadquarters ) format(%9.2fc)
. bysort CountryofHeadquarters: asdoc tabstat EM2_w2, stat(N mean median sd p25 p75) append label tzok dec(3) save(Statistics CGI)


. summarize ABS_DA EM2_w2 CGI factorcgi1 factorcgi2 TAT ROA_w2 JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 lev1 size 
. asdoc summarize ABS_DA EM2_w2 CGI factorcgi1 factorcgi2 TAT ROA_w2 JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 lev1 size , append label tzok dec(3) save(Statistics CGI)


. pwcorr ABS_DA EM2_w2 CGI TAT ROA_w2 JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 lev1 size, sig star(0.01) 
. asdoc pwcorr ABS_DA EM2_w2 CGI TAT ROA_w2 JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 lev1 size, sig star(0.01) append label tzok  dec(3) save(Statistics CGI)

. tabstat ABS_DA CGI TAT ROA_w2 JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 lev1 size, stat(N mean sd q) format(%9.2fc) column(stat)
. asdoc tabstat ABS_DA CGI TAT ROA_w2 JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 lev1 size, stat(N mean sd p25 p50 p75) format(%9.2fc) column(stat) append label tzok  dec(3) save(Statistics CGI) 



***REGRESSION ANALYSIS/ DIAGNOSTIC TEST***
. xtreg ABS_DA CGI Individualismindex powerdistanceindex TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 ROA_w2 lev1 size, fe
. predict e
. tabstat e, stat(skewness kurtosis)
. sktest e 

*** MULTICOLINEALITY ***
. regress ABS_DA CGI Individualismindex powerdistanceindex TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 ROA_w2 lev1 size
. estat vif

***MEASUREMENT ERROR***
. xtreg ABS_DA CGI Individualismindex powerdistanceindex TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 ROA_w2 lev1 size, fe
**. predict CooksD, cooksd
**. xtreg ABS_DA  CGI Individualismindex powerdistanceindex TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 ROA_w2 lev1 size if CooksD<1, fe

***Heteroskedasticity***
. regress ABS_DA CGI Individualismindex powerdistanceindex TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 ROA_w2 lev1 size
. hettest
****higher ch(2) value = Heteroskedastic so the null hypothesis of homoskedasticity is rejected. 
. estat imtest, white
**I have checked both the tests and both of them seems to work
. whitetst

***HAUSMAN***
. xtreg ABS_DA CGI Individualismindex powerdistanceindex TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 ROA_w2 lev1 size i.c i.gicsin, fe
. estimate store fe
. xtreg ABS_DA CGI Individualismindex powerdistanceindex  TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 ROA_w2 lev1 size i.c i.gicsin, re
. estimate store re
. hausman fe re 


***Variable testing***
. correlate ABS_DA CGI Individualismindex powerdistanceindex TAT ROA_w2 JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 lev1 size

. pwcorr ABS_DA CGI Individualismindex powerdistanceindex TAT ROA_w2 JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom OverallScoreEconomicFreedomI BoardSize1 lev1 size, sig star(0.05)




***REGRESSION MODEL*** 
***MAIN MODELS FOR ROBUSTNESS COMPARISON***



***OLS-FE***
***** DEPENDENT VARIABLE ABS_DA(Absolute Discretionary Accruals) Using Modified Jones Model 1995***

. xtreg ABS_DA CGI lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, replace label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA CGI ROA_w2 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA CGI ROA_w2 TAT lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA CGI ROA_w2 TAT JudicialEffectiveness lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA CGI ROA_w2 TAT JudicialEffectiveness TaxBurden lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)





. xtreg ABS_DA factorcgi1 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi1 ROA_w2 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi1 ROA_w2 TAT lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi1 ROA_w2 TAT JudicialEffectiveness lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)



. xtreg ABS_DA factorcgi2 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi2 ROA_w2 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi2 ROA_w2 TAT lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi2 ROA_w2 TAT JudicialEffectiveness lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)





***** DEPENDENT VARIABLE EM2_w2(Earnings Management proxy) Using kothari Model 2005***

. xtreg EM2_w2 CGI lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 CGI ROA_w2 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 CGI ROA_w2 TAT lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 CGI ROA_w2 TAT JudicialEffectiveness lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 CGI ROA_w2 TAT JudicialEffectiveness TaxBurden lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)



. xtreg EM2_w2 factorcgi1 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi1 ROA_w2 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi1 ROA_w2 TAT lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi1 ROA_w2 TAT JudicialEffectiveness lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)



. xtreg EM2_w2 factorcgi2 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi2 ROA_w2 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi2 ROA_w2 TAT lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi2 ROA_w2 TAT JudicialEffectiveness lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI i.c i.gicsin, fe
outreg2 using outputFE1.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)


. xtreg EM2_w2 factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, fe
. outreg2 using outputFE1.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)


***What is p-value in Stata?
**The p-value is a matter of convenience for us. STATA automatically takes into 
**account the number of degrees of freedom and tells us at what level our coefficient is significant. 
**If it is significant at the 95% level, then we have P < 0.05. If it is significant at the 0.01 level, then P < 0.01.






***OLS-FE COMPARATIVE ANALYSIS***
***** DEPENDENT VARIABLE ABS_DA(Absolute Discretionary Accruals) Using Modified Jones Model 1995***
. xtreg ABS_DA CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, fe
. outreg2 using outputFE2.xls, replace label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, fe
. outreg2 using outputFE2.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg ABS_DA factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, fe
. outreg2 using outputFE2.xls, append label ctitle(EM1) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)


***** DEPENDENT VARIABLE EM2_w2(Earnings Management proxy) Using kothari Model 2005***
. xtreg EM2_w2 CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, fe
. outreg2 using outputFE2.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, fe
. outreg2 using outputFE2.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)

. xtreg EM2_w2 factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, fe
. outreg2 using outputFE2.xls, append label ctitle(EM2) addstat(Sigma_u, e(sigma_u), Sigma_e, e(sigma_e), Adj-R^2, e(r2_a), F-test, e(F), p-value, e(p), Rho, e(rho)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with OLS-FE Panel Data) addnote(This is for rubustness comparison)



***FGLS-FE***

***** DEPENDENT VARIABLE ABS_DA(Absolute Discretionary Accruals) Using Modified Jones Model 1995***
. xtgls ABS_DA CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, p(h) c(ar1) force
. outreg2 using outputFE3.xls, replace label ctitle(EM1) addstat(Durbin–Wu–Hausman, e(chi2)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with FGLLS-FE Panel Data) addnote(This is for rubustness comparison) 

. xtgls ABS_DA factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, p(h) c(ar1) force
. outreg2 using outputFE3.xls, append label ctitle(EM1) addstat(Durbin–Wu–Hausman, e(chi2)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with FGLLS-FE Panel Data) addnote(This is for rubustness comparison) 

. xtgls ABS_DA factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, p(h) c(ar1) force
. outreg2 using outputFE3.xls, append label ctitle(EM1) addstat(Durbin–Wu–Hausman, e(chi2)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with FGLLS-FE Panel Data) addnote(This is for rubustness comparison) 


***** DEPENDENT VARIABLE EM2_w2(Earnings Management proxy) Using kothari Model 2005***
. xtgls EM2_w2 CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, p(h) c(ar1) force
. outreg2 using outputFE3.xls, append label ctitle(EM1) addstat(Durbin–Wu–Hausman, e(chi2)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with FGLLS-FE Panel Data) addnote(This is for rubustness comparison) 

. xtgls EM2_w2 factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, p(h) c(ar1) force
. outreg2 using outputFE3.xls, append label ctitle(EM1) addstat(Durbin–Wu–Hausman, e(chi2)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with FGLLS-FE Panel Data) addnote(This is for rubustness comparison) 

. xtgls EM2_w2 factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, p(h) c(ar1) force
. outreg2 using outputFE3.xls, append label ctitle(EM1) addstat(Durbin–Wu–Hausman, e(chi2)) addtext(Ind. FE, Yes, Country FE, YES) dec(4) title(Estimations with FGLLS-FE Panel Data) addnote(This is for rubustness comparison) 



***** For some reason I obtain here an error message statting "invalid syntax" with when trying to compute the AR-2 value in outreg2 command. Please, check this out ****
***Two-step system GMM***
***** DEPENDENT VARIABLE ABS_DA(Absolute Discretionary Accruals) Using Modified Jones Model 1995***
. xtabond2 ABS_DA l.ABS_DA CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, gmm(L2.(ABS_DA), collapse) iv(CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin) robust twostep
. outreg2 using outputFE4.xls, replace label ctitle(EM1) addstat(AR(1), e(ar1p), ARTests, e(artests), Hansen, e(hansenp), Sargen, e(sarganp), Number of Instruments, e(j)) dec(4) title(Estimations with Two-step system GMM-FE Panel Data) addnote(This is for rubustness comparison) 


. xtabond2 ABS_DA l.ABS_DA factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, gmm(L2.(ABS_DA), collapse) iv(factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin) robust twostep
. outreg2 using outputFE4.xls, append label ctitle(EM1) addstat(AR(1), e(ar1p), ARTests, e(artests), Hansen, e(hansenp), Sargen, e(sarganp), Number of Instruments, e(j)) dec(4) title(Estimations with Two-step system GMM-FE Panel Data) addnote(This is for rubustness comparison) 

. xtabond2 ABS_DA l.ABS_DA factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, gmm(L2.(ABS_DA), collapse) iv(factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin) robust twostep
. outreg2 using outputFE4.xls, append label ctitle(EM1) addstat(AR(1), e(ar1p), ARTests, e(artests), Hansen, e(hansenp), Sargen, e(sarganp), Number of Instruments, e(j)) dec(4) title(Estimations with Two-step system GMM-FE Panel Data) addnote(This is for rubustness comparison) 


***** DEPENDENT VARIABLE EM2_w2(Earnings Management proxy) Using kothari Model 2005***
. xtabond2 EM2_w2 l.EM2_w2 CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, gmm(L2.(EM2_w2), collapse) iv(CGI ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin) robust twostep
. outreg2 using outputFE4.xls, append label ctitle(EM1) addstat(AR(1), e(ar1p), ARTests, e(artests), Hansen, e(hansenp), Sargen, e(sarganp), Number of Instruments, e(j)) dec(4) title(Estimations with Two-step system GMM-FE Panel Data) addnote(This is for rubustness comparison) 

. xtabond2 EM2_w2 l.EM2_w2 factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, gmm(L2.(EM2_w2), collapse) iv(factorcgi1 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin) robust twostep
. outreg2 using outputFE4.xls, append label ctitle(EM1) addstat(AR(1), e(ar1p), ARTests, e(artests), Hansen, e(hansenp), Sargen, e(sarganp), Number of Instruments, e(j)) dec(4) title(Estimations with Two-step system GMM-FE Panel Data) addnote(This is for rubustness comparison) 

. xtabond2 EM2_w2 l.EM2_w2 factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin, gmm(L2.(EM2_w2), collapse) iv(factorcgi2 ROA_w2 TAT JudicialEffectiveness TaxBurden TradeFreedom MonetaryFreedom BoardSize1 lev1 size OverallScoreEconomicFreedomI Individualismindex powerdistanceindex i.c i.gicsin) robust twostep
. outreg2 using outputFE4.xls, append label ctitle(EM1) addstat(AR(1), e(ar1p), ARTests, e(artests), Hansen, e(hansenp), Sargen, e(sarganp), Number of Instruments, e(j)) dec(4) title(Estimations with Two-step system GMM-FE Panel Data) addnote(This is for rubustness comparison) 






