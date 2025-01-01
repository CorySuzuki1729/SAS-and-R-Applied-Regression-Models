data psoriasis;
    input patid group$ day1 week1 week2 week5 month3 @@;
    cards;
    1 Tx 15 12 9 3 0 2 Tx 24 17 9 8 8
    3 Cx 14 14 15 15 14 4 Cx 23 20 19 19 15
    5 Tx 11 10 3 2 0 6 Cx 11 10 8 7 5
    7 Tx 7 6 5 3 0 8 Tx 9 6 2 0 0
    9 Cx 9 9 9 9 9 10 Cx 21 16 16 15 15
    ;

data longform;
    set psoriasis;
    array w{5}(0.14 1 2 5 13);
    array x{5} day1 week1 week2 week5 month3;
    do i=1 to 5;
    weeks=w{i};
    npatches=x{i};
    output;
    end;
    keep patid group weeks npatches;
run;

/*fitting GEE with Toeplitz working correlation matrix*/
proc genmod;
    class patid group;
    model npatches=group weeks/dist=poisson link=log;
    repeated subject=patid/type=mdep(3) corrw;
run;

/*fitting GEE with autoregressive working correlation matrix*/
proc genmod;
    class patid group;
    model npatches=group weeks/dist=poisson link=log;
    repeated subject=patid/type=ar corrw;
run;

/*fitting GEE with exchangeable working correlation matrix*/
proc genmod;
    class patid group;
    model npatches=group weeks/dist=poisson link=log;
    repeated subject=patid/type=cs corrw;
run;

/*fitting GEE with independent working correlation matrix*/
proc genmod;
    class patid group;
    model npatches=group weeks/dist=poisson link=log;
    repeated subject=patid/type=ind corrw;
run;

data prediction;
    input patid group$ weeks;
    cards;
    11 Tx 5
    ;

data longform;
    set longform prediction;
run;

proc genmod;
    class patid group;
    model npatches=group weeks/dist=poisson link=log;
    output out=outdata p=p_npatches;
    repeated subject=patid/type=ar corrw;
run;

proc print data=outdata (firstobs=51 obs=51);
    var p_npatches;
run;