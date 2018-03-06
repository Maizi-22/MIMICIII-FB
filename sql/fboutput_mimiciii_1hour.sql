SET SEARCH_PATH TO mimiciii;

DROP MATERIALIZED VIEW IF EXISTS totalbalevents_output CASCADE;
CREATE MATERIALIZED VIEW totalbalevents_output as

WITH fbtime_rank AS (
    SELECT
      *,
      ROW_NUMBER()
      OVER (
        PARTITION BY ft.subject_id
        ORDER BY ft.charthour ) AS chart_rank
    FROM (SELECT DISTINCT
            subject_id,
            date_trunc('hour', ope.charttime)  AS charthour
          FROM outputevents ope) AS ft
)

SELECT fbr.subject_id,
       hadm_id,
       icustay_id,
       fbr.charthour,
       chart_rank,
       --itemid,
       sum(value) AS cumvolume,
       valueuom
  FROM fbtime_rank fbr
  LEFT JOIN (SELECT
               *,
               date_trunc('hour', charttime)  AS charthour
               FROM outputevents ) AS opt
    ON fbr.subject_id = opt.subject_id
   AND fbr.charthour = opt.charthour
 WHERE value <> '0'
 GROUP BY fbr.subject_id,
          hadm_id,
          icustay_id,
          fbr.charthour,
          chart_rank,
          --itemid,
          valueuom
 ORDER BY  fbr.subject_id,
           charthour