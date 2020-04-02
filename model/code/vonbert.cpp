// Fit Von Bertalanffy Growth function to the data
#include <TMB.hpp>
template<class Type>

Type objective_function<Type>::operator() ()
{

// DATA
DATA_VECTOR(age);     // ages in data
DATA_VECTOR(wgt);     // weights in data
//DATA_IVECTOR(cohort); // cohort reference 
DATA_VECTOR(age_q);   // ages to predict
//DATA_IVECTOR(years);  // cohort_years for the predictions

// PARAMETERS
PARAMETER(logK);                // growth rate
PARAMETER(t0);                  // time at weight 0
PARAMETER(Sinf);                // size at inf
PARAMETER(logSigma);            // sd of weight
//PARAMETER_VECTOR(cohort_ef);    // cohort effect

// Transformations
Type K = exp(logK);

// PRELIM

vector<Type> pred(wgt.size());

// PROCEDURE
for(int i = 0; i < wgt.size(); i++) {
//pred(i) = Sinf * (pow((Type(1.0) - exp(-K * (age(i) - t0))), Type(3.0))) + cohort_ef(cohort(i));
pred(i) = Sinf * (pow((Type(1.0) - exp(-K * (age(i) - t0))), Type(3.0)));
}

Type nll = -sum(dnorm(wgt, pred, exp(logSigma), true));

// DERIVED QUANTITIES 
vector<Type> residuals = (wgt - pred)/exp(logSigma);  // residuals
vector<Type> fitted = pred;  // fitted values

vector<Type> predictions(age_q.size()); // quarterly predictions
//matrix<Type> predictions(years.size() , age_q.size()); // quarterly predictions
//for(int y = 0; y < years.size(); y++) {
for(int j = 0; j < age_q.size(); j++) {
predictions(j) = Sinf * (pow((Type(1.0) - exp(-K * (age_q(j) - t0))), Type(3.0)));
//predictions(y,j) = Sinf * (pow((Type(1.0) - exp(-K * (age_q(j) - t0))), Type(3.0))) + cohort_ef(cohort(y));
} 
//}

// REPORT
REPORT(residuals);
REPORT(fitted);
REPORT(predictions);

REPORT(K);

return nll;

}
