SET SEARCH_PATH TO mimiciii;

WITH fbtime_rank AS (
    SELECT
      *,
      ROW_NUMBER()
      OVER (
        PARTITION BY ft.subject_id
        ORDER BY ft.chartday ) AS chart_rank
    FROM (SELECT DISTINCT
            subject_id,
            date_trunc('DAY', ope.charttime)  AS chartday
          FROM outputevents ope) AS ft
)

SELECT fbr.subject_id,
       hadm_id,
       icustay_id,
       fbr.chartday,
       chart_rank,
       --itemid,
       sum(value) AS cumvolume,
       valueuom
  FROM fbtime_rank fbr
  LEFT JOIN (SELECT
               *,
               date_trunc('DAY', charttime)  AS chartday
               FROM outputevents ) AS opt
    ON fbr.subject_id = opt.subject_id
   AND fbr.chartday = opt.chartday
 GROUP BY fbr.subject_id,
          hadm_id,
          icustay_id,
          fbr.chartday,
          chart_rank,
          --itemid,
          valueuom
 ORDER BY  fbr.subject_id,
           chartday
