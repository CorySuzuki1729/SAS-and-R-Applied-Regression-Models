data companies;
    input ownership$ nemployees approach$ @@;
    cards;
    partner 88 comp sole 60 coll stock 24 comp
    partner 108 coll stock 88 coll stock 119 comp
    partner 25 comp partner 53 comp stock 82 comp
    stock 69 coll stock 24 comp partner 94 coll
    stock 86 coll stock 46 coll partner 106 coll
    stock 92 coll stock 22 coll stock 94 coll
    sole 20 coll stock 30 comp partner 92 coll
    sole 90 coll stock 114 coll sole 66 coll
    sole 26 coll stock 59 comp sole 54 coll
    sole 69 comp partner 75 comp partner 62 coll
    stock 26 comp sole 112 coll sole 87 comp
    stock 104 comp partner 25 comp stock 63 comp
    stock 21 comp sole 93 comp stock 91 coll
    sole 33 comp sole 97 coll sole 93 coll
    stock 41 comp partner 73 comp partner 32 comp
    partner 56 comp partner 98 comp partner 97 comp
    stock 117 coll stock 75 comp
    ;

proc genmod;
    class ownership(ref='partner');
    model approach=ownership nemployees/
    dist=binomial link=probit;
run;

proc genmod;
    class ownership(ref='partner');
    model approach=/dist=binomial link=probit;
run;

data deviance_test;
    deviance=-2*(-34.6173-(-29.9519));
    pvalue=1-probchi(deviance, 3);
run;

proc print;
run;

data prediction;
    input ownership$ nemployees;
    cards;
    sole 40
    ;

data companies;
    set companies prediction;
run;

proc genmod;
    class ownership(ref='partner');
    model approach =ownership nemployees/
    dist=binomial link=probit;
    output out=outdata p=pred_probcoll;
run;

proc print data=outdata (firstobs=51 obs=51);
    var pred_probcoll;
run;
