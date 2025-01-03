data job_satisfaction;
    input gender$ age educ$ score @@;
    cards;
    M 53 doctoral 93 M 48 bachelor 66 M 47 master 82
    M 34 bachelor 95 F 35 master 78 M 25 master 62
    F 31 bachelor 87 F 25 master 76 M 26 master 71
    M 58 bachelor 80 F 41 master 75 F 55 bachelor 75
    M 40 bachelor 93 M 22 bachelor 96 F 49 master 63
    F 39 bachelor 77 F 56 doctoral 73 F 49 master 56
    M 45 master 77 M 23 bachelor 77 M 46 bachelor 79
    M 25 master 94 F 62 bachelor 76 M 32 master 90
    F 42 master 74 M 53 bachelor 92 F 29 master 91
    F 47 bachelor 87 F 47 bachelor 55 M 27 bachelor 92
    F 30 master 69 M 36 master 62 M 64 bachelor 77
    F 40 bachelor 65 F 34 master 81 M 48 bachelor 64
    M 46 bachelor 97 M 43 bachelor 80 F 37 bachelor 60
    F 33 master 81 F 55 doctoral 68 M 22 bachelor 100
    M 24 bachelor 68 M 54 bachelor 76 M 42 doctoral 81
    F 63 bachelor 51 F 32 master 75 M 51 doctoral 81
    ;

proc univariate;
    var score;
    histogram/normal;
run;

proc genmod;
    class gender(ref='F') educ(ref='master');
    model score = gender age educ/dist=normal link=identity;
run;

proc genmod;
    model score=/dist=normal link=identity;
run;

data deviance_test;
    deviance=-2*(-187.0063-(-180.4720));
    pvalue=1-probchi(deviance, 4);
run;

proc print;
run;

data prediction;
    input gender$ age educ$;
    cards;
    F 40 bachelor
    ;

data job_satisfaction;
    set job_satisfaction prediction;
run;

proc genmod;
    class gender(ref='F') educ(ref='master');
    model score=gender age educ/dist=normal link=identity;
    output out=outdata p=pred_score;
run;

proc print data=outdata (firstobs=49 obs=49);
    var pred_score;
run;
