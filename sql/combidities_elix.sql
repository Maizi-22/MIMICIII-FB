set search_path to mimiciii;

select subject_id
       , hadm_id
       , case when diabetes_uncomplicated = 1 or diabetes_complicated = 1 then 1 else 0 end as DIABETES
       , hypertension
       , congestive_heart_failure
       , renal_failure
       , liver_disease
       , case when metastatic_cancer = 1 or lymphoma = 1 then 1 else 0 end as CANCER
       , aids
       , chronic_pulmonary
       , weight_loss
       , obesity
  from elixhauser_ahrq