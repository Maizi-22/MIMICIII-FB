SET SEARCH_PATH TO mimiciii;

WITH combidity AS (
SELECT DISTINCT subject_id,
       hadm_id,
  CASE WHEN icd9_code LIKE '250%' OR icd9_code LIKE '249%' THEN 1 END AS diabetes,
  CASE WHEN icd9_code LIKE 'V180' THEN 1 END AS family_diabetes_history,
  CASE WHEN icd9_code LIKE '401%' OR icd9_code LIKE '405%' THEN 1 END AS hypertension,
  CASE WHEN icd9_code LIKE '410%' OR icd9_code LIKE '411%' OR icd9_code LIKE '412%'
  OR icd9_code LIKE '413%' OR icd9_code LIKE '414%' THEN 1 END AS ischemic_heart_disease,
  CASE WHEN icd9_code LIKE '412%' THEN 1 END AS old_myocardual_infarction,
  CASE WHEN icd9_code LIKE '410%' THEN 1 END AS acute_myocardual_infarction,
  CASE WHEN icd9_code LIKE '585%' THEN 1 END AS chronic_kidney_disease,
  CASE WHEN icd9_code LIKE '571%' THEN 1 END AS chronic_liver_disease,
  CASE WHEN icd9_code LIKE 'V8764'  THEN 1 END AS immunosuppression_history,
  CASE WHEN icd9_code LIKE '496.0%' THEN 1 END AS COPD,
  CASE WHEN icd9_code LIKE '042%' OR icd9_code LIKE '07953' THEN 1 END AS AIDS
  FROM diagnoses_icd
 ORDER BY subject_id
)

SELECT cb.subject_id,
       cb.hadm_id,
       max(cb.diabetes) AS diabetes,
       max(cb.family_diabetes_history) AS family_diabetes_history,
       max(cb.hypertension) AS hypertension,
       max(cb.ischemic_heart_disease) AS ischemic_heart_disease,
       max(cb.old_myocardual_infarction) AS old_myocardual_infarction,
       max(cb.acute_myocardual_infarction) AS acute_myocardual_infarction,
       max(cb.chronic_kidney_disease) AS CKD,
       max(cb.chronic_liver_disease) AS CLD,
       max(cb.immunosuppression_history) AS immunosuppression_history,
       max(cb.COPD) AS COPD,
       max(cb.AIDS) AS AIDS

  FROM combidity cb
 GROUP BY cb.subject_id,
          cb.hadm_id
 ORDER BY cb.subject_id
