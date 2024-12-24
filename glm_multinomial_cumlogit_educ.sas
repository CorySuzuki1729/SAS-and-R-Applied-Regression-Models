data health_survey;
    length health$ 10.;
    input gender$ age marital$ educ$ health$ @@;
    cards;
    M 46 yes <HS good M 72 yes <HS poor
    M 52 yes HSgrad excellent M 50 no <HS fair
    F 44 no HSgrad+ poor F 68 no HSgrad fair
    F 50 no HSgrad+ fair F 93 no <HS poor
    M 50 yes HSgrad excellent M 88 no HSgrad+ good
    M 58 yes HSgrad excellent M 52 yes HSgrad good
    F 64 yes HSgrad+ good F 49 yes HSgrad good
    F 41 yes HSgrad+ excellent M 32 no HSgrad+ good
    F 88 no HSgrad poor F 36 yes HSgrad+ excellent
    M 35 no HSgrad+ excellent F 38 no HSgrad+ fair
    M 39 yes HSgrad+ excellent F 43 no <HS good
    M 61 yes HSgrad good F 41 yes HSgrad+ excellent
    F 36 yes <HS good F 44 yes HSgrad+ excellent
    M 41 no HSgrad good M 55 yes <HS good
    M 57 no <HS fair M 28 yes HSgrad+ excellent
    F 40 yes HSgrad good F 97 no HSgrad poor
    ;

proc format;
    value $healthfmt 'poor'='1poor' 'fair'='2fair'
    'good'='3good' 'excellent'='4excellent';
run;

proc genmod;
    class gender(ref='M') marital(ref='yes')
    educ(ref='HSgrad+');
    model health=gender age marital
    educ/dist=multinomial link=cumlogit;
    format health $healthfmt.;
run;

proc genmod;
    model health=/dist=multinomial link=cumlogit;
run;

data deviance_test;
    deviance=-2*(-41.9644-(-24.0727));
    pvalue = 1-probchi(deviance, 5);
run;

proc print;
run;

data prediction;
    input gender$ age marital$ educ$;
    cards;
    M 52 yes HSgrad
    ;

data health_survey;
    set health_survey prediction;
run;

proc genmod;
    class gender(ref='M') marital(ref='yes')
    educ(ref='HSgrad+');
    model health=gender age marital
    educ/dist=multinomial link=cumlogit;
    output out=dataout p=pred_prob;
    format health $healthfmt.;
run;

proc print data=dataout(firstobs=97 obs=99);
var _level_  pred_prob;
run;