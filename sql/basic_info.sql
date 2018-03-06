SET SEARCH_PATH TO mimiciii;

SELECT id.subject_id,
       id.hadm_id,
       id.icustay_id,
       id.age,
       gender,
       weight,
       height,
       ethnicity,
       admission_type,
       extract(epoch from dischtime - admittime)/60/60  AS los_hospital,
       first_hosp_stay,
       hospital_expire_flag,
       extract(epoch from id.outtime - id.intime)/60/60  AS los_icu,
       icus.first_careunit,
       icus.last_careunit,
       first_icu_stay,
       vent.duration_hours,
       gcs.gcseyes,
       gcs.gcsmotor,
       gcs.gcsverbal,
       gcs.mingcs
FROM icustay_detail id
  LEFT JOIN icustays icus
       ON icus.subject_id = id.subject_id
       AND icus.hadm_id = id.hadm_id
       AND icus.icustay_id = id.icustay_id
  LEFT JOIN (SELECT subject_id,
                    icustay_id,
                    sum(ve.duration_hours) AS duration_hours
               FROM ventdurations ve
              GROUP BY subject_id,
                       icustay_id) AS vent
       ON vent.subject_id = id.subject_id
       AND vent.icustay_id = id.icustay_id
  LEFT JOIN gcsfirstday gcs
       ON icus.subject_id = gcs.subject_id
       AND icus.icustay_id = gcs.icustay_id
  LEFT JOIN heightfirstday hfd
       ON icus.icustay_id = hfd.icustay_id
  LEFT JOIN weightfirstday wfd
       ON icus.icustay_id = wfd.icustay_id
 ORDER BY subject_id
