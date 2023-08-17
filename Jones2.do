//step1:prepare data
use /Users/liuxintian/Desktop/9019comp.dta

//counrty
keep if fic=="USA"
//firm
encode gvkey, gen(id)
destring gvkey, replace
//industry
destring sic,replace
keep if (sic<6000|sic>6999)
gen sic2 = int(sic/100)
sort sic2
tostring(sic2),replace
encode sic2,gen(industry)
summarize(industry)

//at least 10 obs per industry
drop if fyear<1993
bysort industry gvkey: gen sumyear=_N
drop if sumyear<26
bysort industry fyear:gen n=_N
drop if n<10
egen n_1=group(industry)
drop industry
rename n_1 industry


//change panel data to balance 
duplicates drop fyear id, force
xtset id fyear
xtbalance, range(1993 2019)

ds, has(type string)
local string_vars `r(varlist)'
foreach v of varlist `string_vars' {
    replace `v' = "0" if `v' == "Null"
    replace `v' = "0" if `v' =="."
	replace `v' = "0" if missing(`v')
	destring `v',replace
}

replace at=0 if at==.
replace sale=0 if sale==.
replace ibc=0 if ibc==.
replace oancf=0 if oancf==.
replace csho=0 if csho==.
replace xrd=0 if xrd==.
replace at=0 if at==.
replace capx=0 if capx==.
replace aqc=0 if aqc==.
replace sppe=0 if sppe==.
replace ppegt=0 if ppegt ==.
replace dp=0 if dp ==.
replace rect=0 if rect ==.

//jones
by id: gen Total_acc = (ibc - oancf) / l.at 
by id: gen CONST=1/l.at
by id: gen PPE=ppegt/l.at
by id: gen DREV=(sale-l.sale)/l.at
//dd
by id: gen TCACC=((act-l.act)-(che-l.che)-(lct-l.lct)+(dlc-l.dlc))/l.at
by id: gen lag_cfo=l.oancf/ l.at
by id: gen ccfo=oancf/ l.at
by id: gen cfo_1=oancf[_n+1]/ l.at
by id: gen drev= (sale-l.sale)/l.at
by id: gen ppe = ppegt/L.at
//modified jones
by id: gen TA = (ibc- oancf)/l.at
by id: gen x0 = 1 / l.at
by id: gen x1= (d.sale-d.rect)/l.at
by id: gen x2 = ppegt/L.at


by id: gen invest=(xrd+capx+aqc-sppe)/l.at
by id : gen growth_sale=(sale/l.sale-1)*100
by id: gen noncapx=(xrd+aqc)/l.at*100
by id: gen capx1=capx*100/l.ppent
//control varibale
by id: gen firm_size=log(at)
by id: gen mtb=(at+(csho*prcc_f)-ceq-txdb)/at
by id: gen div=0
by id: replace div=1 if dvc>0
by id: gen cfosale=oancf/sale
by id: gen loss=0
replace loss=1 if ib<0
by id: gen slack=che/ppent
by id: gen cash=che/at
by id: gen tangibility=ppent/at
by id: gen z_score=3.3*pi+sale+0.25*re+0.5*(act-lct)/at
by id: gen opercyle=log(((rect/sale)+(invt/cogs))*360)
by id: gen k_structure=dltt/(dltt+csho*prcc_f)
by id: gen lev=dltt/at


winsor2 Total_acc CONST PPE DREV TCACC lag_cfo ccfo cfo_1 drev ppe TA x0 x1 x2 invest growth_sale noncapx capx1 firm_size mtb div cfosale  loss slack cash tangibility z_score opercyle k_structure, cut(1 99) trim

local varlist1 Total_acc CONST PPE DREV TCACC lag_cfo ccfo cfo_1 drev ppe TA x0 x1 x2 invest growth_sale noncapx capx1 firm_size mtb div cfosale  loss slack cash tangibility z_score opercyle k_structure
foreach v of local varlist1{
    replace `v' = 0 if missing(`v')	
}

//robust test: unit root test
AQ_Jones AQ_modified_Jones AQ_DD k_structure loss tangibility slack cfosale div invest_eff over_investment under_investment
foreach v of local varlist1 {
    xtunitroot ht `v',trend demean
}
//drop sd_cfo sd_sale


eststo clear

//step 2: Jones
//Jones
gen AQ_Jones=.
forval y = 1993(1)2019{
   forval j = 1(1) 24{
      display `j'
      display `y'
      capture noisily{
         reg Total_acc CONST DREV PPE if `j'==industry & `y'==fyear, noconstant
         predict r if `j'==industry & `y'==fyear, resid
         replace AQ_Jones=r if `j'==industry & `y'==fyear
         drop r
      }
   }
}
replace AQ_Jones=abs(AQ_Jones)*-1


//Step 3: Modified Jones
gen AQ_modified_Jones=.
forval y = 1993(1)2019{
   forval j = 1(1) 24 {
      display `j'
      display `y'
      capture noisily{
         reg TA x0 x1 x2 if `j'==industry & `y'==fyear, noconstant
         predict r if `j'==industry & `y'==fyear, resid
         replace AQ_modified_Jones=r if `j'==industry & `y'==fyear
         drop r
      }
   }
}
replace AQ_modified_Jones=abs(AQ_modified_Jones)*-1


//step4: DD model

gen AQ_DD=.
forval y = 1993(1)2019{
   forval j = 1(1) 24 {
      display `j'
      display `y'
      capture noisily{
         reg TCACC lag_cfo ccfo cfo_1 ppe drev if `j'==industry & `y'==fyear, noconstant
         predict r if `j'==industry & `y'==fyear, resid
         replace AQ_DD=r if `j'==industry & `y'==fyear
         drop r
      }
   }
}
replace AQ_DD=abs(AQ_DD)*-1


//step 6 classify the investment
gen neg=.
replace neg=1 if growth_sale<0
replace neg=0 if growth_sale>=0
xtset id fyear
gen neg_growth=neg*growth_sale

gen invest_eff=.
forval y = 1993(1)2019{
   forval j = 1(1) 24 {
      display `j'
      display `y'
      capture noisily{
         reg invest neg growth_sale neg_growth if `j'==industry & `y'==fyear, noconstant
         predict r if `j'==industry & `y'==fyear, resid
         replace invest_eff=r if `j'==industry & `y'==fyear
         drop r
      }
   }
}
drop if missing(AQ_DD,AQ_modified_Jones,AQ_Jones,invest_eff)

xtset id fyear
//over- or under- invest
gen invest_state=.
replace invest_state=1 if invest_eff>0
replace invest_state=0 if invest_eff<0

gen over_investment=invest_eff if invest_eff>0
gen under_investment=invest_eff*-1 if invest_eff<=0
summarize(over_investment)
summarize(under_investment)

//under
replace invest_eff=abs(invest_eff)


//summarize

asdoc summarize(AQ_Jones AQ_modified_Jones AQ_DD k_structure loss tangibility slack cfosale div invest_eff over_investment under_investment), save(summary.doc) replace title(Descriptive statistics)

asdoc pwcorr AQ_Jones AQ_modified_Jones AQ_DD k_structure loss tangibility slack cfosale div invest_eff over_investment under_investment, sig star(all) bonferroni, save(correlation.doc) title(Correlation Matrix)


//step 7:hausman test
xtbalance,range(1993 2019)
global control1 firm_size mtb div cfosale loss slack cash tangibility z_score opercyle k_structure

reg invest_eff AQ_Jones $control1
est sto m2
ivregress 2sls invest_eff (AQ_Jones=$control1 i.industry)
est sto m1
hausman m2 m1


reg invest_eff AQ_modified_Jones $control1
est sto m3
ivregress 2sls invest_eff (AQ_modified_Jones=$control1 i.industry)
est sto m4
hausman m4 m3

reg invest_eff AQ_DD $control1
est sto m5
ivregress 2sls invest_eff (AQ_DD=$control1 i.industry)
est sto m6
hausman m6 m5


//step 8: regression
global control1 k_structure loss tangibility slack cfosale div



keep if invest_state==1 
reghdfe  invest_eff AQ_Jones $control1, a(fyear industry) cl(industry)
est store m1
reghdfe invest_eff AQ_modified_Jones $control1, a(fyear industry) cl(industry)
est store m2
reghdfe invest_eff AQ_DD $control1, a(fyear industry) cl(industry)
est store m3
esttab m1 m2 m3 
esttab m1 m2 m3 using reg.doc,replace b(%6.3f) t(%6.2f) s(N r2_a) compress nogap stats(Fixed_Effect Cluster N r2_a, fmt(%3s %3s %12.0f  %9.3f)) varwidth(20)

keep if invest_state==0 
reghdfe  invest_eff AQ_Jones $control1, a(fyear industry) cl(industry)
est store m1
reghdfe invest_eff AQ_modified_Jones $control1, a(fyear industry) cl(industry)
est store m2
reghdfe invest_eff AQ_DD $control1, a(fyear industry) cl(industry)
est store m3
esttab m1 m2 m3 
esttab m1 m2 m3, star(* 0.1 ** 0.05 *** 0.01) b(%6.3f) t(%6.3f) compress nogap stats(Fixed_Effect Cluster N r2_a, fmt(%3s %3s %12.0f  %9.3f)) varwidth(20) indicate("Year FE=yd*") mtitle("OLS" "OLS" "FE")

// oneclick invest_eff $control1, fix(AQ_DD i.industry i.fyear) p(0.1) m(xtreg) o(re)
