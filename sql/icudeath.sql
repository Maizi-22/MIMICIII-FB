SET SEARCH_PATH TO mimiciii;

SELECT icus.subject_id,
       icus.hadm_id,
       icus.icustay_id,
       --icus.intime,
       --icus.outtime,
       --ad.deathtime,
       CASE WHEN( ad.deathtime between icus.intime AND icus.outtime) THEN 'Y' ELSE 'N' END AS icu_expire_flag
  FROM icustays icus
  LEFT JOIN admissions ad
       ON ad.subject_id = icus.subject_id
       AND ad.hadm_id = icus.hadm_id
