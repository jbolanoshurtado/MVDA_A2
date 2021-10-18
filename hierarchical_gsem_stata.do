*quick check on mvda a2 ddbb

gl 			a "C:\Users\Jean Pierre\Documents\0-Local Workstation\0-RUG\0-courses\1a courses\Multivariate\Assignments\Assignment2"

importsav 	"$a\assign2_ModMed&HLM_advertisingrecall(1).sav"

svyset 		commerc, strata(block)

mixed		purchase || block: 

estat icc   

mixed		unaided || block: 

estat icc

mixed 		aided || block: 

estat icc

mixed 		aided unaided || block: 

mixed 		aided c.unaided##c.unaided || block: 


estat icc

mixed 		purchase i.primacy##c.duration  c.duration##i.recency || block: 






*********** Learning to use GSEM - Multilevel model

regress		purchase duration unaided
regress		purchase duration unaided, robust

/*
Mixed-effects ML regression                     Number of obs     =      2,152
Group variable: block                           Number of groups  =        165
                                                Obs per group:
                                                              min =          5
                                                              avg =       13.0
                                                              max =         17
                                                Wald chi2(2)      =     288.71
Log likelihood = -8292.1386                     Prob > chi2       =     0.0000

------------------------------------------------------------------------------
    purchase | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
-------------+----------------------------------------------------------------
    duration |   .0643339   .0350689     1.83   0.067       -.0044    .1330677
     unaided |    .345919   .0241584    14.32   0.000     .2985695    .3932686
       _cons |   16.96645   .8909534    19.04   0.000     15.22021    18.71268
------------------------------------------------------------------------------

------------------------------------------------------------------------------
  Random-effects parameters  |   Estimate   Std. err.     [95% conf. interval]
-----------------------------+------------------------------------------------
block: Identity              |
                  var(_cons) |   2.44e-08   8.95e-06             0    9.9e+304
-----------------------------+------------------------------------------------
               var(Residual) |   130.1349   3.968514      122.5846    138.1502
------------------------------------------------------------------------------
LR test vs. linear model: chibar2(01) = 0.00          Prob >= chibar2 = 1.0000

*/

* Same results from SEM and OLS as expected

mixed		purchase duration unaided || block:

/*
Generalized structural equation model                    Number of obs = 2,152
Response: purchase
Family:   Gaussian
Link:     Identity
Log likelihood = -8292.1386

 ( 1)  [purchase]M1[block] = 1
---------------------------------------------------------------------------------
                | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
----------------+----------------------------------------------------------------
purchase        |
       duration |    .064334   .0350689     1.83   0.067    -.0043998    .1330678
        unaided |    .345919   .0241584    14.32   0.000     .2985695    .3932685
                |
      M1[block] |          1  (constrained)
                |
          _cons |   16.96644    .890953    19.04   0.000     15.22021    18.71268
----------------+----------------------------------------------------------------
  var(M1[block])|   5.40e-31   2.49e-15                             .           .
----------------+----------------------------------------------------------------
 var(e.purchase)|   130.1348   3.967226                      122.5869    138.1474
---------------------------------------------------------------------------------

*/
*Exactly the same output


*Creating variables needed
gen 		marginal_recall = aided - unaided
*creating the dummy variables
tabulate 	day, gen(dday)
tabulate 	year, gen(dyear)
tabulate 	prodtype, gen(dprodtype)



*checking that results are the same
mixed		purchase duration unaided marginal_recall || block:

/* Output mixed
       duration |   .0622239   .0349692     1.78   0.075    -.0063144    .1307622
        unaided |   .3638871   .0246001    14.79   0.000     .3156718    .4121023
marginal_recall |   .1119157   .0311531     3.59   0.000     .0508567    .1729747
          _cons |   12.61331   1.502465     8.40   0.000     9.668536    15.55809
*Output SEM
purchase        |
       duration |   .0622239   .0349692     1.78   0.075    -.0063144    .1307622
        unaided |   .3638871   .0246001    14.79   0.000     .3156718    .4121023
marginal_recall |   .1119157   .0311531     3.59   0.000     .0508567    .1729747
          _cons |   12.61331   1.502465     8.40   0.000     9.668536    15.55809
*
***Identical 
*/

*checking that results are the same
mixed		purchase duration unaided marginal_recall 	///
			primacy recency 							///
			dday1 dday3 - dday7  						/// 
			dyear2 - dyear12 							///
			dprodtype2 - dprodtype4 					///
			|| block:
			
estat icc // basically zero as we knew



*Nice results Now let's see if the SEM can replicate to then go on and test 
* Multilevel mediation there!
* They are indeed the same, check the excel file



* Running the MLSEM with mediation

gsem 		(duration -> unaided, ) (duration -> purchase, ) (unaided -> purchase, ) (M1[block] -> unaided, ) (marginal_recall -> purchase, ) (primacy -> 	  ///
			purchase, ) (recency -> purchase, ) (dday1 -> purchase, ) (dday3 -> purchase, ) (dday4 -> purchase, ) (dday5 -> purchase, ) (dday6 -> purchase, ) ///
			(dday7 -> purchase, ) (dyear2 -> purchase, ) (dyear3 -> purchase, ) (dyear4 -> purchase, ) (dyear5 -> purchase, ) (dyear6 -> purchase, ) (dyear7  ///
			 -> purchase, ) (dyear8 -> purchase, ) (dyear9 -> purchase, ) (dyear10 -> purchase, ) (dyear11 -> purchase, ) (dyear12 -> purchase, ) (dprodtype2 ///
			 -> purchase, ) (dprodtype3 -> purchase, ) (dprodtype4 -> purchase, ) (M2[block] -> purchase, ), covstruct(_lexogenous, diagonal) technique(bfgs) ///
			 iterate(3000) latent(M1 M2 ) nocapslatent


* Testing the significance of the interaction term in the multilevel SEM
/* 

>>> Indirect effect is
From duration to unaided : _b[unaided:duration]
From unaided to purchase : _b[purchase:unaided]
>>> Direct effect is 
From duration to purchase : _b[purchase:duration]
**Can also check coeff legends with command:
gsem, coeflegend
*/
* Significance test for the indirect effect
nlcom 			_b[unaided:duration]*_b[purchase:unaided]

*Total effect
nlcom 			_b[purchase:duration] + _b[unaided:duration]*_b[purchase:unaided]






gsem (duration -> unaided, ) (duration -> purchase, ) (unaided -> purchase, ) ///
		(M1[block] -> unaided, ) (marginal_recall -> unaided, ) /// 
		(marginal_recall -> purchase, ) (primacy -> unaided, )  ///
		(primacy -> purchase, ) (recency -> unaided, ) (recency -> ///
		purchase, ) (dday1 -> unaided, ) (dday1 -> purchase,) ///
		(dday3 -> unaided, ) (dday3 -> purchase, ) (dday4 -> unaided, ) ///
		(dday4 -> purchase, ) (dday5 -> unaided, ) (dday5 -> purchase, ) ///
		(dday6 -> unaided,  ) (dday6 -> purchase, ) (dday7 -> unaided, ) ///
		(dday7 -> purchase, ) (dyear2 -> unaided, ) (dyear2 -> purchase, ) ///
		(dyear3 -> unaided, ) (dyear3 -> purchase, ) (dyear4 -> unaided, ) ///
		(dyear4 -> purchase, ) (dyear5 -> unaided, ) (dyear5 -> purchase, ) ///
		(dyear6 -> unaided, ) (dyear6 -> purchase, ) (dyear7 -> unaided, ) ///
		(dyear7 -> purchase, ) (dyear8 -> unaided, ) (dyear8 -> purchase, ) ///
		(dyear9 -> unaided, ) (dyear9 -> purchase, ) (dyear10 -> unaided, ) ///
		(dyear10 -> purchase, ) (dyear11 -> unaided, ) (dyear11 -> purchase, ) ///
		(dyear12 -> unaided, ) (dyear12 -> purchase, ) (dprodtype2 -> unaided, ) ///
		(dprodtype2 -> purchase, ) (dprodtype3 -> unaided, ) (dprodtype3 -> ///
		purchase, ) (dprodtype4 -> unaided, ) (dprodtype4 -> purchase, ) ///
		(M2[block] -> purchase, ), covstruct(_lexogenous, diagonal) ///
		technique(bfgs) iterate(3000) latent(M1 M2 ) nocapslatent
* Significance test for the indirect effect
nlcom 			_b[unaided:duration]*_b[purchase:unaided]

*Total effect
nlcom 			_b[purchase:duration] + _b[unaided:duration]*_b[purchase:unaided]



