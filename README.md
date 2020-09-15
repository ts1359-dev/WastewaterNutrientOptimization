# Can Wastewater Nutrient Discharge be optimized using machine learning?
1. Perform a feasibility study that relies on a classification tree to predict whether an industrial wastewater plant would be able to meet new total nitrogen limits of 10 mg/L.
 DATA USED IS A 535 X 35 MATRIX
- TN_PASS -> A CATEGORICAL DEPENDENT VARIABLE INDICATING WHETHER DAILY TN
CONCENTRATIONS MET THE NEW DISCHARGE LIMITS (10 MG/L).
- X -> VARIABLE FOR 34 CONTINUOUS PREDICTORS OF TN_PASS (SELECTED FROM
RAW DATASET BASED ON KNOWLEDGE OF WASTEWATER PLANT OPERATION).
- 107-FOLD CROSS VALIDATION USED TO CREATE 107 CLASSIFICATION TREES, EACH TRAINED ON 530 DATA
POINTS AND TESTED ON THE FIVE (5) REMAINING OBSERVATIONS.
- PRUNING DONE USING THE CP OF SMALLEST TREE THAT IS WITHIN ONE STANDARD DEVIATION OF THE TREE
WITH THE SMALLEST XERROR.

2. Leveraging on constrained ordination methods, identify "constraining variables" that may be prominent drivers for effluent ammonia and nitrate concentration behaviors.
