SET SEARCH_PATH TO mimiciii;

SELECT pvt.subject_id,
    pvt.hadm_id,
    pvt.icustay_id,

    min(
        CASE
            WHEN (pvt.label = 'CREATININE'::text) THEN pvt.valuenum
            ELSE NULL::double precision
        END) AS creatinine_min,
    max(
        CASE
            WHEN (pvt.label = 'CREATININE'::text) THEN pvt.valuenum
            ELSE NULL::double precision
        END) AS creatinine_max,
    --CASE WHEN (pvt.label = 'CREATININE'::text) THEN pvt.valueuom END AS creatinine_uom,
    min(
        CASE
            WHEN (pvt.label = 'GLUCOSE'::text) THEN pvt.valuenum
            ELSE NULL::double precision
        END) AS glucose_min,
    max(
        CASE
            WHEN (pvt.label = 'GLUCOSE'::text) THEN pvt.valuenum
            ELSE NULL::double precision
        END) AS glucose_max,

    min(
        CASE
            WHEN (pvt.label = 'HEMOGLOBIN'::text) THEN pvt.valuenum
            ELSE NULL::double precision
        END) AS hemoglobin_min,
    max(
        CASE
            WHEN (pvt.label = 'HEMOGLOBIN'::text) THEN pvt.valuenum
            ELSE NULL::double precision
        END) AS hemoglobin_max,
    min(
        CASE
            WHEN (pvt.label = 'LACTATE'::text) THEN pvt.valuenum
            ELSE NULL::double precision
        END) AS lactate_min,
    max(
        CASE
            WHEN (pvt.label = 'LACTATE'::text) THEN pvt.valuenum
            ELSE NULL::double precision
        END) AS lactate_max,

    min(
        CASE
            WHEN (pvt.label = 'WBC'::text) THEN pvt.valuenum
            ELSE NULL::double precision
        END) AS wbc_min,
    max(
        CASE
            WHEN (pvt.label = 'WBC'::text) THEN pvt.valuenum
            ELSE NULL::double precision
        END) AS wbc_max
   FROM ( SELECT ie.subject_id,
            ie.hadm_id,
            ie.icustay_id,
                CASE
                    WHEN (le.itemid = 50912) THEN 'CREATININE'::text
                    WHEN (le.itemid = 50809) THEN 'GLUCOSE'::text
                    WHEN (le.itemid = 50931) THEN 'GLUCOSE'::text
                    WHEN (le.itemid = 50811) THEN 'HEMOGLOBIN'::text
                    WHEN (le.itemid = 51222) THEN 'HEMOGLOBIN'::text
                    WHEN (le.itemid = 50813) THEN 'LACTATE'::text
                    WHEN (le.itemid = 51300) THEN 'WBC'::text
                    WHEN (le.itemid = 51301) THEN 'WBC'::text
                    ELSE NULL::text
                END AS label,
                CASE
                    WHEN ((le.itemid = 50912) AND (le.valuenum > (150)::double precision)) THEN NULL::double precision
                    WHEN ((le.itemid = 50809) AND (le.valuenum > (10000)::double precision)) THEN NULL::double precision
                    WHEN ((le.itemid = 50931) AND (le.valuenum > (10000)::double precision)) THEN NULL::double precision
                    WHEN ((le.itemid = 50811) AND (le.valuenum > (50)::double precision)) THEN NULL::double precision
                    WHEN ((le.itemid = 51222) AND (le.valuenum > (50)::double precision)) THEN NULL::double precision
                    WHEN ((le.itemid = 50813) AND (le.valuenum > (50)::double precision)) THEN NULL::double precision
                    WHEN ((le.itemid = 51300) AND (le.valuenum > (1000)::double precision)) THEN NULL::double precision
                    WHEN ((le.itemid = 51301) AND (le.valuenum > (1000)::double precision)) THEN NULL::double precision
                    ELSE le.valuenum
                END AS valuenum,
                valueuom
           FROM (mimiciii.icustays ie
             LEFT JOIN mimiciii.labevents le ON (((le.subject_id = ie.subject_id) AND (le.hadm_id = ie.hadm_id) AND ((le.charttime >= (ie.intime - '06:00:00'::interval hour)) AND (le.charttime <= (ie.intime + '1 day'::interval day))) AND (le.itemid = ANY (ARRAY[50868, 50862, 51144, 50882, 50885, 50912, 50902, 50806, 50931, 50809, 51221, 50810, 51222, 50811, 50813, 51265, 50971, 50822, 51275, 51237, 51274, 50983, 50824, 51006, 51301, 51300])) AND (le.valuenum IS NOT NULL) AND (le.valuenum > (0)::double precision))))) pvt
  GROUP BY pvt.subject_id, pvt.hadm_id, pvt.icustay_id
    --, pvt.label, pvt.valueuom
  ORDER BY pvt.subject_id, pvt.hadm_id, pvt.icustay_id;
