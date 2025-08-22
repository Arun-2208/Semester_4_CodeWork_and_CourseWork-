SET hive.exec.dynamic.partition.mode=nonstrict;
SET hive.exec.compress.output=false;
SET hive.cli.print.header=true;
SELECT '=== START OF SCRIPT ===';

DROP TABLE accidents_raw;
-- Creates an external table over the csv file
CREATE EXTERNAL TABLE accidents_raw (ACCIDENT_NO string,
	ACCIDENTDATE string,
	ACCIDENTTIME string,
	ACCIDENT_TYPE string,
	Accident_Type_Desc string,
	DAY_OF_WEEK string,
	Day_Week_Description string,
	DCA_CODE string,
	DCA_Description string,
	DIRECTORY string,
	EDITION string,
	PAGE string,
	GRID_REFERENCE_X string,
	GRID_REFERENCE_Y string,
	LIGHT_CONDITION string,
	Light_Condition_Desc string,
	NODE_ID string,
	NO_OF_VEHICLES float,
	NO_PERSONS float,
	NO_PERSONS_INJ_2 string,
	NO_PERSONS_INJ_3 string,
	NO_PERSONS_KILLED float,
	NO_PERSONS_NOT_INJ float,
	POLICE_ATTEND float,
	ROAD_GEOMETRY string,
	Road_Geometry_Desc string,
	SEVERITY string,
	SPEED_ZONE float)
-- The following lines describe the format and location of the file
ROW FORMAT DELIMITED FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
STORED AS TEXTFILE
LOCATION '/accidents/data';

SELECT '=== Completed first query ===';

-- Drop the accidents_in_hive table if it exists
DROP TABLE accidents_in_hive;
-- Create the accidents_in_hive table and populate it with data
-- pulled in from the CSV file (via the external table defined previously)
CREATE TABLE accidents_in_hive
LOCATION '/accidents/processed'
AS
SELECT ACCIDENT_NO AS accident_no,
	ACCIDENTDATE AS accidentdate,
	ACCIDENTTIME AS accidenttime,
	ACCIDENT_TYPE AS accident_type,
	Accident_Type_Desc AS accident_type_desc,
	DAY_OF_WEEK AS day_of_week,
	Day_Week_Description AS day_week_description,
	DCA_CODE AS dca_code,
	DCA_Description AS dca_description,
	DIRECTORY AS directory,
	EDITION AS edition,
	PAGE AS page,
	GRID_REFERENCE_X AS grid_reference_x,
	GRID_REFERENCE_Y AS grid_reference_y,
	LIGHT_CONDITION AS light_condition,
	Light_Condition_Desc AS light_condition_desc,
	NODE_ID AS node_id,
	NO_OF_VEHICLES AS no_of_vehicles,
	NO_PERSONS AS no_persons,
	NO_PERSONS_INJ_2 AS no_persons_inj_2,
	NO_PERSONS_INJ_3 AS no_persons_inj_3,
	NO_PERSONS_KILLED AS no_persons_killed,
	NO_PERSONS_NOT_INJ AS no_persons_not_inj,
	POLICE_ATTEND AS police_attend,
	ROAD_GEOMETRY AS road_geometry,
	Road_Geometry_Desc AS road_geometry_desc,
	SEVERITY AS severity,
	SPEED_ZONE AS speed_zone
FROM accidents_raw;
SELECT '=== END OF SCRIPT ===';
