data real_estate;
    input price beds baths sqft heating$ AC$ lot;
    price10K=price/10000;
    sqftK=sqft/1000;
    central=(heating='central');
    electric=(heating='electric');
    ACyes=(AC='yes');
    lotK=lot/1000;
    cards;
    669000 3 2 1733 central no 5641
    715000 4 3.5 1812 none yes 4995
    634900 5 3 2217 none no 8019
    640000 3 2 1336 none no 7283
    966000 5 3 4000 central no 7424
    889000 3 2 2005 central no 7130
    745000 4 3.5 2276 none no 7936
    685000 2 1.5 1018 central yes 6141
    549500 2 1 920 central no 5545
    868999 5 2.5 1670 electric yes 5750
    624900 3 2 1519 electric no 8267
    549900 2 1 956 none no 4978
    589900 3 2 1601 central no 5005
    829000 5 3 2652 central yes 5601
    599900 4 2 1802 none yes 5262
    875000 6 2.5 3414 electric yes 6534
    635000 3 2 1565 central no 5619
    599999 2 1 832 none no 5601
    734997 3 2.5 1780 central yes 5400
    699999 3 2 1969 electric no 5488
    759000 4 2 1530 central yes 6446
    684900 3 2 1519 central no 8267
    888000 5 2.75 2039 central yes 5976
    599999 4 2 1513 electric no 5937
    565000 2 2 1616 central no 5227
    825000 3 2.5 1421 central yes 5871
    659900 3 2 1547 electric yes 4791
    746000 3 2 1130 central no 5301
    1089000 5 2.5 3314 central yes 7129
    1195499 5 3.5 3760 central yes 6000
    ;

    proc genmod;
        class heating(ref='none') AC(ref='no');
        model price10K = beds baths sqftK heating AC lotK
        /dist=gamma link=log;
    run;

    proc genmod;
        model price10K=/dist=gamma link=log;
    run;

    data deviance_test;
        deviance=-2*(-122.8174-(-100.0395));
        pvalue=1-probchi(deviance, 7);
    run;

    proc print;
    run;


    data prediction;
        input beds baths sqftK heating$ AC$ lotK;
        cards;
        4 2 1.68 central no 5
        ;
    
    data real_estate;
        set real_estate prediction;
    run;

    proc genmod;
        class heating(ref='none') AC(ref='no');
        model price10K = beds baths sqftK heating
        AC lotK/dist=gamma link=log;
        output out=outdata p=pprice;
    run;

    data outdata;
        set outdata;
        pred_price=10000*pprice;
    run;

    proc print data=outdata (firstobs=31 obs=31);
        var pred_price;
    run;