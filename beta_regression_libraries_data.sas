data libraries;
    input nbooks ncardholders location$ propontime @@;
    cards;
    9.8 0.7 rural 0.39 25.4 7.8 urban 0.81
    14.7 1.7 urban 0.71 38.2 9.5 urban 0.85
    35.5 5.8 urban 0.74 14.1 2.6 rural 0.47
    16.3 2.9 rural 0.72 33.3 7.7 urban 0.83
    19.9 3.1 rural 0.69 38.0 4.6 urban 0.76
    44.1 8.3 urban 0.85 34.2 5.4 urban 0.86
    12.7 2.7 rural 0.53 28.7 3.4 urban 0.78
    9.5 1.0 rural 0.55 31.8 8.7 urban 0.78
    21.1 6.2 urban 0.82 12.1 2.3 rural 0.42
    26.3 0.9 rural 0.88 16.4 8.4 urban 0.88
    11.8 1.6 rural 0.45 31.6 8.8 rural 0.64
    24.6 4.5 rural 0.81 25.3 1.8 urban 0.73
    12.4 1.9 rural 0.38 16.2 7.3 rural 0.66
    13.7 6.2 urban 0.84 29.4 6.3 urban 0.72
    ;

proc glimmix;
    class location(ref='rural');
    model propontime=nbooks ncardholders location/
    dist=beta link=logit solution;
run;

proc glimmix;
    model propontime=/dist=beta link=logit;
run;

data deviance_test;
    deviance= -30.66-(-53.40);
    pvalue=1-probchi(deviance, 3);
run;

proc print;
run;

data prediction;
    input nbooks ncardholders location$;
    cards;
    15 2.5 rural
    ;

data libraries;
    set libraries prediction;
run;

proc glimmix;
    class location(ref='rural');
    model propontime=nbooks ncardholders location /
    dist=beta link=logit solution;
    output out=outdata pred(ilink)=p_propontime;
run;

proc print data=outdata (firstobs=29 obs=29);
    var p_propontime;
run;