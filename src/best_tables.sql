--
-- PostgreSQL database dump
--

-- Dumped from database version 14.12 (Debian 14.12-1.pgdg110+1)
-- Dumped by pg_dump version 14.15 (Homebrew)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: madi_lumi_reader_outliers; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA madi_lumi_reader_outliers;


--
-- Name: madi_lumi_users; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA madi_lumi_users;


--
-- Name: madi_results; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA madi_results;


--
-- Name: database_control_samples(character varying, character varying, character varying, integer); Type: PROCEDURE; Schema: madi_results; Owner: -
--

CREATE PROCEDURE madi_results.database_control_samples(IN study_accession character varying, IN experiment_accession character varying, IN result_schema character varying, IN workspace_id integer)
    LANGUAGE plpgsql
    AS $$begin
        CREATE TABLE IF NOT EXISTS madi_results.db_control_samples AS
SELECT
xmap_buffer_id AS control_sample_accession, CONCAT(database_control_samples.study_accession,'_plates') AS assay_group_id, REPLACE(split_part(plate_id,'\',-1),' ','.') AS assay_id, NULL AS catalog_id, 
CASE WHEN dilution IS NULL THEN '1' ELSE dilution END AS dilution_factor, database_control_samples.experiment_accession AS experiment_accession, NULL AS lot_number, 
database_control_samples.result_schema AS result_schema, CONCAT('negative_control|buffer') AS source, 'in_QC_database' AS upload_result_status, database_control_samples.workspace_id AS workspace_id 
	FROM madi_results.xmap_buffer
	WHERE xmap_buffer.study_accession = database_control_samples.study_accession
UNION
SELECT 
xmap_control_id AS control_sample_accession, CONCAT(database_control_samples.study_accession,'_plates') AS assay_group_id, REPLACE(split_part(plate_id,'\',-1),' ','.') AS assay_id, NULL AS catalog_id, 
CASE WHEN dilution IS NULL THEN '1' ELSE dilution END AS dilution_factor, database_control_samples.experiment_accession AS experiment_accession, NULL AS lot_number, 
database_control_samples.result_schema AS result_schema, CONCAT('positive_control|',source) AS source, 'in_QC_database' AS upload_result_status, database_control_samples.workspace_id AS workspace_id 
	FROM madi_results.xmap_control
	WHERE xmap_control.study_accession = database_control_samples.study_accession
UNION
SELECT 
xmap_standard_id AS control_sample_accession, CONCAT(database_control_samples.study_accession,'_plates') AS assay_group_id, REPLACE(split_part(plate_id,'\',-1),' ','.') AS assay_id, NULL AS catalog_id, 
CASE WHEN dilution IS NULL THEN '1' ELSE dilution END AS dilution_factor, database_control_samples.experiment_accession AS experiment_accession, NULL AS lot_number, 
database_control_samples.result_schema AS result_schema, CONCAT('standard_curve|',source) AS source, 'in_QC_database' AS upload_result_status, database_control_samples.workspace_id AS workspace_id 
	FROM madi_results.xmap_standard
	WHERE xmap_standard.study_accession = database_control_samples.study_accession;

end;$$;


--
-- Name: database_std_curves(character varying, character varying, character varying, character varying, integer); Type: PROCEDURE; Schema: madi_results; Owner: -
--

CREATE PROCEDURE madi_results.database_std_curves(IN study_accession character varying, IN experiment_accession character varying, IN result_schema character varying, IN limit_unit character varying, IN workspace_id integer)
    LANGUAGE plpgsql
    AS $$begin
        CREATE TABLE IF NOT EXISTS madi_results.db_standard_curves AS
SELECT xmap_standard_fits AS standard_curve_accession, CONCAT (antigen, '_', analyte) AS analyte_reported, CONCAT(database_std_curves.study_accession, '_plates') AS assay_group_id, plateid AS assay_id,
	database_std_curves.experiment_accession AS experiment_accession, formula AS formula, llod as lower_limit, database_std_curves.limit_unit as lower_limit_unit, database_std_curves.result_schema as result_schema, 
	'in_QC_database' AS upload_result_status, ulod AS upper_limit, database_std_curves.limit_unit as upper_limit_unit, database_std_curves.workspace_id as workspace_id
	--antigen, iter, status, crit, l_asy, r_asy, x_mid, scale, bendlower, bendupper, llod, ulod, loglik, aic, bic, deviance, dfresidual, nobs, rsquare_fit, source, g, mse, cv, lloq, uloq, loq_method, bkg_method, is_log_mfi_axis, linear_center
	FROM madi_results.xmap_standard_fits xsf
	where xsf.study_accession=database_std_curves.study_accession;

end;$$;


--
-- Name: db_control_results(character varying, character varying, integer, character varying, character varying); Type: PROCEDURE; Schema: madi_results; Owner: -
--

CREATE PROCEDURE madi_results.db_control_results(IN experiment_accession character varying, IN study_accession character varying, IN workspace_id integer, IN study_id character varying, IN concentration_unit_reported character varying)
    LANGUAGE plpgsql
    AS $$begin
        CREATE TABLE IF NOT EXISTS madi_results.db_control_results AS
SELECT CONCAT (xmap_buffer.experiment_accession, '|', antigen) AS analyte_reported, NULL AS arm_accession, xmap_buffer.experiment_accession AS assay_group_id,  
CONCAT(xmap_buffer.experiment_accession, '_', dilution, '_', REPLACE(split_part(plate_id,'\',-1),' ','.')) AS assay_id, NULL AS biosample_accession, db_control_results.concentration_unit_reported, 
1/dilution AS concentration_value_reported, db_control_results.experiment_accession, antibody_mfi AS mfi, well AS mfi_coordinate, CONCAT('blank', xmap_buffer_id) AS source_accession, 'CONTROL SAMPLE' AS source_type, db_control_results.study_accession, 
NULL AS study_time_collected, 'Not Specified' AS study_time_collected_unit, NULL AS subject_accession, db_control_results.workspace_id
	FROM madi_results.xmap_buffer
	WHERE xmap_buffer.study_accession = db_control_results.study_id
UNION
SELECT CONCAT (xmap_control.experiment_accession, '|', antigen) AS analyte_reported, NULL AS arm_accession, xmap_control.experiment_accession AS assay_group_id,  
CONCAT(xmap_control.experiment_accession, '_', dilution, '_', REPLACE(split_part(plate_id,'\',-1),' ','.')) AS assay_id, NULL AS biosample_accession, db_control_results.concentration_unit_reported, 
1/dilution AS concentration_value_reported, db_control_results.experiment_accession, antibody_mfi AS mfi, well AS mfi_coordinate, CONCAT('posc', xmap_control_id) AS source_accession, 'CONTROL SAMPLE' AS source_type, db_control_results.study_accession, 
NULL AS study_time_collected, 'Not Specified' AS study_time_collected_unit, NULL AS subject_accession, db_control_results.workspace_id
	FROM madi_results.xmap_control
	WHERE xmap_control.study_accession = db_control_results.study_id
UNION
SELECT CONCAT (xmap_standard.experiment_accession, '|', antigen) AS analyte_reported, NULL AS arm_accession, xmap_standard.experiment_accession AS assay_group_id,  
CONCAT(xmap_standard.experiment_accession, '_', dilution, '_', REPLACE(split_part(plate_id,'\',-1),' ','.')) AS assay_id, NULL AS biosample_accession, db_control_results.concentration_unit_reported, 
1/dilution AS concentration_value_reported, db_control_results.experiment_accession, antibody_mfi AS mfi, well AS mfi_coordinate, CONCAT('stand', xmap_standard_id) AS source_accession, 'STANDARD CURVE' AS source_type, db_control_results.study_accession, 
NULL AS study_time_collected, 'Not Specified' AS study_time_collected_unit, NULL AS subject_accession, db_control_results.workspace_id
	FROM madi_results.xmap_standard
	WHERE xmap_standard.study_accession = db_control_results.study_id;
end;$$;


--
-- Name: mbaa_for_database(character varying, character varying, character varying, character varying, character varying, character varying, character varying, integer); Type: PROCEDURE; Schema: madi_results; Owner: -
--

CREATE PROCEDURE madi_results.mbaa_for_database(IN study_accession character varying, IN experiment_accession character varying, IN concentration_unit_reported character varying, IN source_type character varying, IN study_time_collected_unit character varying, IN result_schema character varying, IN study_time_t0_event character varying, IN workspace_id integer)
    LANGUAGE plpgsql
    AS $$
begin
        CREATE TABLE IF NOT EXISTS madi_results.mbaa_samples AS
		SELECT
        xs.xmap_sample_id AS result_id, 
        subj.study_accession,
		mbaa_for_database.experiment_accession,
        xs.experiment_accession AS assay_group_id,
        CONCAT(xs.experiment_accession, '_', xs.dilution, '_', xmap_header.plateid) AS assay_id,
        xs.dilution,
        CONCAT (xs.experiment_accession, '|', xs.antigen) AS analyte_reported, 
        xs.antibody_au AS concentration_value_reported,
        mbaa_for_database.concentration_unit_reported,
        xs.antibody_mfi AS mfi, 
        xs.well AS mfi_coordinate, 
        mbaa_for_database.source_type,

        subj.subject_accession AS subject_accession,
        visit.planned_visit_accession AS planned_visit_accession,
        subj.arm_accession AS arm_accession,
        timing.actual_visit_day AS study_time_collected,
        mbaa_for_database.study_time_collected_unit,
        
        CONCAT('ES', SUBSTRING(mbaa_for_database.experiment_accession, 4, LENGTH(ispi_to_madi_database.experiment_accession)),'_', ROW_NUMBER() OVER (ORDER BY xs.xmap_sample_id)) AS source_accession,
        CONCAT('BS', SUBSTRING(subj.subject_accession, 7, LENGTH(subj.subject_accession)), SUBSTRING(xs.timeperiod, 1, 3), '_', ROW_NUMBER() OVER (ORDER BY xs.xmap_sample_id)) AS biosample_accession,
        mbaa_for_database.result_schema,
        CONCAT(subj.subject_accession, '_', xs.timeperiod) AS biosample_name,
        CONCAT(xs.experiment_accession, '_', xs.dilution, subj.subject_accession, xs.timeperiod) AS expsample_name,
		visit.type as biosample_type,
		mbaa_for_database.study_time_t0_event,
        mbaa_for_database.workspace_id 

    FROM madi_results.xmap_sample xs
    JOIN madi_results.xmap_subjects subj ON xs.patientid = subj.xmap_patientid
    JOIN madi_results.xmap_planned_visit visit ON xs.timeperiod = visit.timepoint_name
    JOIN madi_results.xmap_sample_timing timing ON xs.patientid = timing.patientid AND xs.timeperiod = timing.timeperiod
	JOIN madi_results.xmap_header ON xs.plate_id = xmap_header.plate_id AND xs.study_accession=xmap_header.study_accession
	WHERE xs.study_accession = mbaa_for_database.study_accession;
end;$$;


--
-- Name: add_project_access_key(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.add_project_access_key() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    INSERT INTO madi_lumi_users.project_access_keys (project_id, access_key)
    VALUES (NEW.project_id, gen_random_uuid());
    RETURN NEW;
END;
$$;


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: comparisons; Type: TABLE; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE TABLE madi_lumi_reader_outliers.comparisons (
    id integer NOT NULL,
    context_id integer NOT NULL,
    antigen text NOT NULL,
    visit_1 text NOT NULL,
    visit_2 text NOT NULL,
    serialized_plot bytea NOT NULL,
    value_type text DEFAULT 'MFI'::text NOT NULL
);


--
-- Name: comparisons_id_seq; Type: SEQUENCE; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE SEQUENCE madi_lumi_reader_outliers.comparisons_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: comparisons_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_lumi_reader_outliers; Owner: -
--

ALTER SEQUENCE madi_lumi_reader_outliers.comparisons_id_seq OWNED BY madi_lumi_reader_outliers.comparisons.id;


--
-- Name: main_context; Type: TABLE; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE TABLE madi_lumi_reader_outliers.main_context (
    id integer NOT NULL,
    workspace_id integer NOT NULL,
    study text NOT NULL,
    experiment text NOT NULL,
    context_name text,
    value_type text DEFAULT 'MFI'::text NOT NULL,
    job_status text DEFAULT 'pending'::text NOT NULL
);


--
-- Name: main_context_id_seq; Type: SEQUENCE; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE SEQUENCE madi_lumi_reader_outliers.main_context_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: main_context_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_lumi_reader_outliers; Owner: -
--

ALTER SEQUENCE madi_lumi_reader_outliers.main_context_id_seq OWNED BY madi_lumi_reader_outliers.main_context.id;


--
-- Name: outliers; Type: TABLE; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE TABLE madi_lumi_reader_outliers.outliers (
    id integer NOT NULL,
    comparison_id integer NOT NULL,
    subject_accession text NOT NULL,
    visit_1 text NOT NULL,
    visit_2 text NOT NULL,
    gate_class_1 text,
    hample_outlier boolean,
    bagplot_outlier boolean,
    kde_outlier boolean,
    antigen text NOT NULL,
    feature text NOT NULL,
    additional_info jsonb,
    visit_1_name text,
    visit_2_name text,
    context_id integer,
    lab_confirmed boolean DEFAULT false,
    gate_class_2 text
);


--
-- Name: outliers_id_seq; Type: SEQUENCE; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE SEQUENCE madi_lumi_reader_outliers.outliers_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: outliers_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_lumi_reader_outliers; Owner: -
--

ALTER SEQUENCE madi_lumi_reader_outliers.outliers_id_seq OWNED BY madi_lumi_reader_outliers.outliers.id;


--
-- Name: project_access_keys; Type: TABLE; Schema: madi_lumi_users; Owner: -
--

CREATE TABLE madi_lumi_users.project_access_keys (
    project_id integer NOT NULL,
    access_key uuid DEFAULT gen_random_uuid() NOT NULL
);


--
-- Name: project_users; Type: TABLE; Schema: madi_lumi_users; Owner: -
--

CREATE TABLE madi_lumi_users.project_users (
    project_id integer NOT NULL,
    user_id character varying NOT NULL,
    is_owner boolean DEFAULT false NOT NULL,
    joined_date timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: projects; Type: TABLE; Schema: madi_lumi_users; Owner: -
--

CREATE TABLE madi_lumi_users.projects (
    project_id integer NOT NULL,
    project_name character varying(255) NOT NULL,
    creation_date timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: projects_project_id_seq; Type: SEQUENCE; Schema: madi_lumi_users; Owner: -
--

CREATE SEQUENCE madi_lumi_users.projects_project_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: projects_project_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_lumi_users; Owner: -
--

ALTER SEQUENCE madi_lumi_users.projects_project_id_seq OWNED BY madi_lumi_users.projects.project_id;


--
-- Name: users; Type: TABLE; Schema: madi_lumi_users; Owner: -
--

CREATE TABLE madi_lumi_users.users (
    user_id integer NOT NULL,
    auth0_id character varying(255) NOT NULL,
    email character varying(255) NOT NULL
);


--
-- Name: users_user_id_seq; Type: SEQUENCE; Schema: madi_lumi_users; Owner: -
--

CREATE SEQUENCE madi_lumi_users.users_user_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: users_user_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_lumi_users; Owner: -
--

ALTER SEQUENCE madi_lumi_users.users_user_id_seq OWNED BY madi_lumi_users.users.user_id;


--
-- Name: db_control_results; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.db_control_results (
    analyte_reported text,
    arm_accession text,
    assay_group_id character varying(15),
    assay_id text,
    biosample_accession text,
    concentration_unit_reported character varying,
    concentration_value_reported numeric,
    experiment_accession character varying,
    mfi numeric(8,0),
    mfi_coordinate character varying(6),
    source_accession text,
    source_type text,
    study_accession character varying,
    study_time_collected text,
    study_time_collected_unit text,
    subject_accession text,
    workspace_id integer
);


--
-- Name: db_control_samples; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.db_control_samples (
    control_sample_accession bigint,
    assay_group_id text,
    assay_id text,
    catalog_id text,
    dilution_factor numeric,
    experiment_accession character varying,
    lot_number text,
    result_schema character varying,
    source text,
    upload_result_status text,
    workspace_id integer
);


--
-- Name: db_standard_curves; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.db_standard_curves (
    standard_curve_accession bigint,
    analyte_reported text,
    assay_group_id text,
    assay_id character varying(100),
    experiment_accession character varying,
    formula text,
    lower_limit numeric,
    lower_limit_unit character varying,
    result_schema character varying,
    upload_result_status text,
    upper_limit numeric,
    upper_limit_unit character varying,
    workspace_id integer
);


--
-- Name: mbaa_samples; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.mbaa_samples (
    result_id bigint,
    study_accession character varying(15),
    experiment_accession character varying,
    assay_group_id character varying(15),
    assay_id text,
    dilution numeric(9,0),
    analyte_reported text,
    concentration_value_reported double precision,
    concentration_unit_reported character varying,
    mfi numeric(8,0),
    mfi_coordinate character varying(6),
    source_type character varying,
    subject_accession character varying(15),
    planned_visit_accession character varying(15),
    arm_accession character varying(15),
    study_time_collected integer,
    study_time_collected_unit character varying,
    source_accession text,
    biosample_accession text,
    result_schema character varying,
    biosample_name text,
    expsample_name text,
    biosample_type character varying(50),
    study_time_t0_event character varying,
    workspace_id integer
);


--
-- Name: xmap_antigen_family_id_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_antigen_family_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_antigen_family; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_antigen_family (
    xmap_antigen_family_id bigint DEFAULT nextval('madi_results.xmap_antigen_family_id_seq'::regclass) NOT NULL,
    study_accession character varying(15) NOT NULL,
    antigen character varying(64) NOT NULL,
    antigen_family character varying(15),
    standard_curve_concentration numeric DEFAULT 10000
);


--
-- Name: xmap_arm_reference; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_arm_reference (
    xmap_arm_reference_id integer NOT NULL,
    study_accession character varying(15) NOT NULL,
    agroup character varying(64) NOT NULL,
    referent boolean DEFAULT false NOT NULL
);


--
-- Name: xmap_arm_reference_xmap_arm_reference_id_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_arm_reference_xmap_arm_reference_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_arm_reference_xmap_arm_reference_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_arm_reference_xmap_arm_reference_id_seq OWNED BY madi_results.xmap_arm_reference.xmap_arm_reference_id;


--
-- Name: xmap_buffer; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_buffer (
    xmap_buffer_id bigint NOT NULL,
    study_accession character varying(15) NOT NULL,
    experiment_accession character varying(15) NOT NULL,
    plate_id character varying(640) NOT NULL,
    well character varying(6) NOT NULL,
    stype character varying(6) NOT NULL,
    pctaggbeads numeric(8,0),
    samplingerrors character varying(64),
    antigen character varying(64) NOT NULL,
    antibody_mfi numeric(8,0),
    antibody_n integer,
    antibody_name character varying(15),
    dilution numeric(9,0) DEFAULT 1 NOT NULL,
    feature character varying(15)
);


--
-- Name: xmap_buffer_xmap_buffer_id_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_buffer_xmap_buffer_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_buffer_xmap_buffer_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_buffer_xmap_buffer_id_seq OWNED BY madi_results.xmap_buffer.xmap_buffer_id;


--
-- Name: xmap_control; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_control (
    xmap_control_id bigint NOT NULL,
    study_accession character varying(15) NOT NULL,
    experiment_accession character varying(15) NOT NULL,
    plate_id character varying(640) NOT NULL,
    well character varying(6) NOT NULL,
    stype character varying(6) NOT NULL,
    sampleid character varying(15) NOT NULL,
    source character varying(25),
    dilution numeric(9,0),
    pctaggbeads numeric(8,0),
    samplingerrors character varying(64),
    antigen character varying(64) NOT NULL,
    antibody_mfi numeric(8,0),
    antibody_n integer,
    antibody_name character varying(15),
    feature character varying(15)
);


--
-- Name: xmap_control_xmap_control_id_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_control_xmap_control_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_control_xmap_control_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_control_xmap_control_id_seq OWNED BY madi_results.xmap_control.xmap_control_id;


--
-- Name: xmap_dilution_analysis; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_dilution_analysis (
    xmap_dilution_analysis_id bigint NOT NULL,
    study_accession character varying(15) NOT NULL,
    experiment_accession character varying(15) NOT NULL,
    plateid character varying(640),
    timeperiod character varying(15),
    patientid character varying(15),
    agroup character varying(25),
    dilution numeric(9,0),
    antigen character varying(64) NOT NULL,
    n_pass_dilutions numeric(9,0),
    concentration_status character varying(40),
    au_treatment character varying(25),
    decision_nodes character varying(50),
    bkg_method character varying(50),
    processed_au numeric
);


--
-- Name: xmap_dilution_analysis_xmap_dilution_analysis_id_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

ALTER TABLE madi_results.xmap_dilution_analysis ALTER COLUMN xmap_dilution_analysis_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME madi_results.xmap_dilution_analysis_xmap_dilution_analysis_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: xmap_dilution_parameters_id_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_dilution_parameters_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_dilution_parameters; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_dilution_parameters (
    xmap_dilution_parameters_id bigint DEFAULT nextval('madi_results.xmap_dilution_parameters_id_seq'::regclass) NOT NULL,
    study_accession character varying(15) NOT NULL,
    node_order character varying(64) NOT NULL,
    valid_gate_class character varying(64),
    is_binary_gate boolean
);


--
-- Name: xmap_header; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_header (
    xmap_header_id bigint NOT NULL,
    study_accession character varying(15) NOT NULL,
    experiment_accession character varying(15) NOT NULL,
    plate_id character varying(640) NOT NULL,
    file_name character varying(1028),
    acquisition_date timestamp with time zone,
    reader_serial_number character varying(64),
    rp1_pmt_volts numeric(8,0),
    rp1_target numeric(8,0),
    auth0_user text,
    workspace_id bigint,
    plateid character varying(2024),
    plate character varying(15),
    sample_dilution_factor numeric,
    n_wells numeric
);


--
-- Name: xmap_header_xmap_header_id_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_header_xmap_header_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_header_xmap_header_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_header_xmap_header_id_seq OWNED BY madi_results.xmap_header.xmap_header_id;


--
-- Name: xmap_planned_visit; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_planned_visit (
    xmap_timeperiod integer NOT NULL,
    study_accession character varying(15) NOT NULL,
    timepoint_name character varying(125),
    subtype character varying(50),
    type character varying(50) NOT NULL,
    planned_visit_accession character varying(15),
    order_number integer,
    min_start_day real,
    max_start_day real,
    end_rule character varying(256),
    start_rule character varying(256)
);


--
-- Name: TABLE xmap_planned_visit; Type: COMMENT; Schema: madi_results; Owner: -
--

COMMENT ON TABLE madi_results.xmap_planned_visit IS 'Describes an STUDY indicated encounter with a SUBJECT.';


--
-- Name: COLUMN xmap_planned_visit.xmap_timeperiod; Type: COMMENT; Schema: madi_results; Owner: -
--

COMMENT ON COLUMN madi_results.xmap_planned_visit.xmap_timeperiod IS 'Primary key';


--
-- Name: COLUMN xmap_planned_visit.study_accession; Type: COMMENT; Schema: madi_results; Owner: -
--

COMMENT ON COLUMN madi_results.xmap_planned_visit.study_accession IS 'Foreign key reference to the STUDY table';


--
-- Name: COLUMN xmap_planned_visit.timepoint_name; Type: COMMENT; Schema: madi_results; Owner: -
--

COMMENT ON COLUMN madi_results.xmap_planned_visit.timepoint_name IS 'Name of timepoint';


--
-- Name: COLUMN xmap_planned_visit.order_number; Type: COMMENT; Schema: madi_results; Owner: -
--

COMMENT ON COLUMN madi_results.xmap_planned_visit.order_number IS 'Order of event';


--
-- Name: COLUMN xmap_planned_visit.min_start_day; Type: COMMENT; Schema: madi_results; Owner: -
--

COMMENT ON COLUMN madi_results.xmap_planned_visit.min_start_day IS 'Initial day in the study timeline';


--
-- Name: COLUMN xmap_planned_visit.max_start_day; Type: COMMENT; Schema: madi_results; Owner: -
--

COMMENT ON COLUMN madi_results.xmap_planned_visit.max_start_day IS ' Last day that the visit may occur';


--
-- Name: COLUMN xmap_planned_visit.end_rule; Type: COMMENT; Schema: madi_results; Owner: -
--

COMMENT ON COLUMN madi_results.xmap_planned_visit.end_rule IS 'Description of the conditions that define the end of a Planned Visit.';


--
-- Name: COLUMN xmap_planned_visit.start_rule; Type: COMMENT; Schema: madi_results; Owner: -
--

COMMENT ON COLUMN madi_results.xmap_planned_visit.start_rule IS 'Description of the conditions that define the beginning of a Planned Visit.';


--
-- Name: xmap_planned_visit_xmap_timeperiod_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_planned_visit_xmap_timeperiod_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_planned_visit_xmap_timeperiod_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_planned_visit_xmap_timeperiod_seq OWNED BY madi_results.xmap_planned_visit.xmap_timeperiod;


--
-- Name: xmap_profile; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_profile (
    xmap_profile_id bigint NOT NULL,
    study_accession character varying(15) NOT NULL,
    experiment_accession character varying(15) NOT NULL,
    stype character varying(6) NOT NULL,
    dilution_stor character varying(100),
    group_stor character varying(100),
    timeperiod_stor character varying(100),
    patientid_stor character varying(100),
    source_stor character varying(100)
);


--
-- Name: xmap_profile_xmap_profile_id_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_profile_xmap_profile_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_profile_xmap_profile_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_profile_xmap_profile_id_seq OWNED BY madi_results.xmap_profile.xmap_profile_id;


--
-- Name: xmap_sample; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_sample (
    xmap_sample_id bigint NOT NULL,
    study_accession character varying(15) NOT NULL,
    experiment_accession character varying(15) NOT NULL,
    plate_id character varying(640) NOT NULL,
    timeperiod character varying(15),
    patientid character varying(15),
    well character varying(6) NOT NULL,
    stype character varying(6) NOT NULL,
    sampleid character varying(15) NOT NULL,
    id_imi character varying(15),
    agroup character varying(25),
    dilution numeric(9,0) NOT NULL,
    pctaggbeads numeric(8,0),
    samplingerrors character varying(64),
    antigen character varying(64) NOT NULL,
    antibody_mfi numeric(8,0),
    antibody_n integer,
    antibody_name character varying(15),
    feature character varying(15),
    gate_class character varying(25),
    antibody_au double precision,
    antibody_au_se double precision,
    reference_dilution numeric,
    gate_class_dil character varying(25),
    norm_mfi numeric DEFAULT 0.1,
    in_linear_region boolean,
    gate_class_loq character varying(25),
    in_quantifiable_range boolean,
    gate_class_linear_region character varying(50),
    quality_score numeric
);


--
-- Name: xmap_sample_timing; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_sample_timing (
    xmap_sample integer NOT NULL,
    study_accession character varying(15) NOT NULL,
    subject_accession character varying(15) NOT NULL,
    patientid character varying(15) NOT NULL,
    timeperiod character varying(50) NOT NULL,
    actual_visit_day integer
);


--
-- Name: xmap_sample_timing_xmap_sample_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_sample_timing_xmap_sample_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_sample_timing_xmap_sample_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_sample_timing_xmap_sample_seq OWNED BY madi_results.xmap_sample_timing.xmap_sample;


--
-- Name: xmap_sample_xmap_sample_id_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_sample_xmap_sample_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_sample_xmap_sample_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_sample_xmap_sample_id_seq OWNED BY madi_results.xmap_sample.xmap_sample_id;


--
-- Name: xmap_standard; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_standard (
    xmap_standard_id bigint NOT NULL,
    study_accession character varying(15) NOT NULL,
    experiment_accession character varying(15) NOT NULL,
    plate_id character varying(640) NOT NULL,
    well character varying(6) NOT NULL,
    stype character varying(6) NOT NULL,
    sampleid character varying(15) NOT NULL,
    source character varying(25),
    dilution numeric(9,0) NOT NULL,
    pctaggbeads numeric(9,0),
    samplingerrors character varying(64),
    antigen character varying(64) NOT NULL,
    antibody_mfi numeric(8,0),
    antibody_n integer,
    antibody_name character varying(15),
    feature character varying(15),
    predicted_mfi numeric
);


--
-- Name: xmap_standard_fit_tab; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_standard_fit_tab (
    xmap_standard_fit_tab bigint NOT NULL,
    study_accession character varying(15) NOT NULL,
    experiment_accession character varying(15) NOT NULL,
    plateid character varying(100) NOT NULL,
    antigen character varying(64),
    term character varying(15),
    estimate numeric,
    std_error numeric,
    signif character varying(15),
    source character varying(25)
);


--
-- Name: xmap_standard_fit_tab_xmap_standard_fit_tab_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_standard_fit_tab_xmap_standard_fit_tab_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_standard_fit_tab_xmap_standard_fit_tab_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_standard_fit_tab_xmap_standard_fit_tab_seq OWNED BY madi_results.xmap_standard_fit_tab.xmap_standard_fit_tab;


--
-- Name: xmap_standard_fits; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_standard_fits (
    xmap_standard_fits bigint NOT NULL,
    study_accession character varying(15) NOT NULL,
    experiment_accession character varying(15) NOT NULL,
    plateid character varying(100) NOT NULL,
    antigen character varying(64),
    iter numeric,
    status character varying(100),
    crit character varying(20),
    l_asy numeric,
    r_asy numeric,
    x_mid numeric,
    scale numeric,
    bendlower numeric,
    bendupper numeric,
    llod numeric,
    ulod numeric,
    loglik numeric,
    aic numeric,
    bic numeric,
    deviance numeric,
    dfresidual numeric,
    nobs numeric,
    rsquare_fit numeric,
    source character varying(25),
    g numeric,
    mse double precision,
    cv double precision,
    lloq numeric,
    uloq numeric,
    loq_method character varying,
    bkg_method character varying,
    is_log_mfi_axis boolean,
    analyte character varying(40),
    formula text,
    x_inflection numeric,
    y_inflection numeric
);


--
-- Name: COLUMN xmap_standard_fits.mse; Type: COMMENT; Schema: madi_results; Owner: -
--

COMMENT ON COLUMN madi_results.xmap_standard_fits.mse IS 'mean square error between the fitted and the observed values';


--
-- Name: COLUMN xmap_standard_fits.cv; Type: COMMENT; Schema: madi_results; Owner: -
--

COMMENT ON COLUMN madi_results.xmap_standard_fits.cv IS 'coefficient of variation';


--
-- Name: xmap_standard_fits_xmap_standard_fits_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_standard_fits_xmap_standard_fits_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_standard_fits_xmap_standard_fits_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_standard_fits_xmap_standard_fits_seq OWNED BY madi_results.xmap_standard_fits.xmap_standard_fits;


--
-- Name: xmap_standard_preds; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_standard_preds (
    xmap_standard_preds bigint NOT NULL,
    study_accession character varying(15) NOT NULL,
    experiment_accession character varying(15) NOT NULL,
    antigen character varying(64),
    plateid character varying(100),
    fitted numeric,
    log_dilution numeric,
    source character varying(25)
);


--
-- Name: xmap_standard_preds_xmap_standard_preds_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_standard_preds_xmap_standard_preds_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_standard_preds_xmap_standard_preds_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_standard_preds_xmap_standard_preds_seq OWNED BY madi_results.xmap_standard_preds.xmap_standard_preds;


--
-- Name: xmap_standard_stor; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_standard_stor (
    xmap_standard_stor bigint NOT NULL,
    study_accession character varying(15) NOT NULL,
    experiment_accession character varying(15) NOT NULL,
    plate_id character varying(100) NOT NULL,
    stype character(1),
    dilution numeric,
    antigen character varying(64),
    mfi numeric,
    nbeads numeric,
    feature character varying(15),
    log_dilution numeric,
    plateid character varying(100),
    weights numeric,
    source character varying(25)
);


--
-- Name: xmap_standard_stor_xmap_standard_stor_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_standard_stor_xmap_standard_stor_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_standard_stor_xmap_standard_stor_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_standard_stor_xmap_standard_stor_seq OWNED BY madi_results.xmap_standard_stor.xmap_standard_stor;


--
-- Name: xmap_standard_xmap_standard_id_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_standard_xmap_standard_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_standard_xmap_standard_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_standard_xmap_standard_id_seq OWNED BY madi_results.xmap_standard.xmap_standard_id;


--
-- Name: xmap_study_config; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_study_config (
    xmap_study_config_id integer NOT NULL,
    study_accession character varying(15) NOT NULL,
    param_group character varying(24),
    param_name character varying(50),
    param_label character varying(256),
    param_data_type character varying(15),
    param_char_len numeric(8,0),
    param_control_type character varying(64),
    param_choices_list character varying(256),
    param_integer_value integer,
    param_boolean_value boolean,
    param_character_value character varying(256),
    param_user text
);


--
-- Name: xmap_study_config_xmap_study_config_id_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_study_config_xmap_study_config_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_study_config_xmap_study_config_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_study_config_xmap_study_config_id_seq OWNED BY madi_results.xmap_study_config.xmap_study_config_id;


--
-- Name: xmap_subjects; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_subjects (
    xmap_patientid character varying(15) NOT NULL,
    study_accession character varying(15) NOT NULL,
    subject_accession character varying(15),
    arm_accession character varying(15),
    agroup character varying(15),
    patientid character varying(15)
);


--
-- Name: xmap_subjects_xmap_patientid_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_subjects_xmap_patientid_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_subjects_xmap_patientid_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_subjects_xmap_patientid_seq OWNED BY madi_results.xmap_subjects.xmap_patientid;


--
-- Name: xmap_users; Type: TABLE; Schema: madi_results; Owner: -
--

CREATE TABLE madi_results.xmap_users (
    xmap_users_id bigint NOT NULL,
    auth0_user text,
    project_name text,
    workspace_id bigint NOT NULL
);


--
-- Name: xmap_users_workspace_id_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_users_workspace_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_users_workspace_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_users_workspace_id_seq OWNED BY madi_results.xmap_users.workspace_id;


--
-- Name: xmap_users_xmap_users_id_seq; Type: SEQUENCE; Schema: madi_results; Owner: -
--

CREATE SEQUENCE madi_results.xmap_users_xmap_users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: xmap_users_xmap_users_id_seq; Type: SEQUENCE OWNED BY; Schema: madi_results; Owner: -
--

ALTER SEQUENCE madi_results.xmap_users_xmap_users_id_seq OWNED BY madi_results.xmap_users.xmap_users_id;


--
-- Name: comparisons id; Type: DEFAULT; Schema: madi_lumi_reader_outliers; Owner: -
--

ALTER TABLE ONLY madi_lumi_reader_outliers.comparisons ALTER COLUMN id SET DEFAULT nextval('madi_lumi_reader_outliers.comparisons_id_seq'::regclass);


--
-- Name: main_context id; Type: DEFAULT; Schema: madi_lumi_reader_outliers; Owner: -
--

ALTER TABLE ONLY madi_lumi_reader_outliers.main_context ALTER COLUMN id SET DEFAULT nextval('madi_lumi_reader_outliers.main_context_id_seq'::regclass);


--
-- Name: outliers id; Type: DEFAULT; Schema: madi_lumi_reader_outliers; Owner: -
--

ALTER TABLE ONLY madi_lumi_reader_outliers.outliers ALTER COLUMN id SET DEFAULT nextval('madi_lumi_reader_outliers.outliers_id_seq'::regclass);


--
-- Name: projects project_id; Type: DEFAULT; Schema: madi_lumi_users; Owner: -
--

ALTER TABLE ONLY madi_lumi_users.projects ALTER COLUMN project_id SET DEFAULT nextval('madi_lumi_users.projects_project_id_seq'::regclass);


--
-- Name: users user_id; Type: DEFAULT; Schema: madi_lumi_users; Owner: -
--

ALTER TABLE ONLY madi_lumi_users.users ALTER COLUMN user_id SET DEFAULT nextval('madi_lumi_users.users_user_id_seq'::regclass);


--
-- Name: xmap_arm_reference xmap_arm_reference_id; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_arm_reference ALTER COLUMN xmap_arm_reference_id SET DEFAULT nextval('madi_results.xmap_arm_reference_xmap_arm_reference_id_seq'::regclass);


--
-- Name: xmap_buffer xmap_buffer_id; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_buffer ALTER COLUMN xmap_buffer_id SET DEFAULT nextval('madi_results.xmap_buffer_xmap_buffer_id_seq'::regclass);


--
-- Name: xmap_control xmap_control_id; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_control ALTER COLUMN xmap_control_id SET DEFAULT nextval('madi_results.xmap_control_xmap_control_id_seq'::regclass);


--
-- Name: xmap_header xmap_header_id; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_header ALTER COLUMN xmap_header_id SET DEFAULT nextval('madi_results.xmap_header_xmap_header_id_seq'::regclass);


--
-- Name: xmap_planned_visit xmap_timeperiod; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_planned_visit ALTER COLUMN xmap_timeperiod SET DEFAULT nextval('madi_results.xmap_planned_visit_xmap_timeperiod_seq'::regclass);


--
-- Name: xmap_profile xmap_profile_id; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_profile ALTER COLUMN xmap_profile_id SET DEFAULT nextval('madi_results.xmap_profile_xmap_profile_id_seq'::regclass);


--
-- Name: xmap_sample xmap_sample_id; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_sample ALTER COLUMN xmap_sample_id SET DEFAULT nextval('madi_results.xmap_sample_xmap_sample_id_seq'::regclass);


--
-- Name: xmap_sample_timing xmap_sample; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_sample_timing ALTER COLUMN xmap_sample SET DEFAULT nextval('madi_results.xmap_sample_timing_xmap_sample_seq'::regclass);


--
-- Name: xmap_standard xmap_standard_id; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_standard ALTER COLUMN xmap_standard_id SET DEFAULT nextval('madi_results.xmap_standard_xmap_standard_id_seq'::regclass);


--
-- Name: xmap_standard_fit_tab xmap_standard_fit_tab; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_standard_fit_tab ALTER COLUMN xmap_standard_fit_tab SET DEFAULT nextval('madi_results.xmap_standard_fit_tab_xmap_standard_fit_tab_seq'::regclass);


--
-- Name: xmap_standard_fits xmap_standard_fits; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_standard_fits ALTER COLUMN xmap_standard_fits SET DEFAULT nextval('madi_results.xmap_standard_fits_xmap_standard_fits_seq'::regclass);


--
-- Name: xmap_standard_preds xmap_standard_preds; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_standard_preds ALTER COLUMN xmap_standard_preds SET DEFAULT nextval('madi_results.xmap_standard_preds_xmap_standard_preds_seq'::regclass);


--
-- Name: xmap_standard_stor xmap_standard_stor; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_standard_stor ALTER COLUMN xmap_standard_stor SET DEFAULT nextval('madi_results.xmap_standard_stor_xmap_standard_stor_seq'::regclass);


--
-- Name: xmap_study_config xmap_study_config_id; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_study_config ALTER COLUMN xmap_study_config_id SET DEFAULT nextval('madi_results.xmap_study_config_xmap_study_config_id_seq'::regclass);


--
-- Name: xmap_subjects xmap_patientid; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_subjects ALTER COLUMN xmap_patientid SET DEFAULT nextval('madi_results.xmap_subjects_xmap_patientid_seq'::regclass);


--
-- Name: xmap_users xmap_users_id; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_users ALTER COLUMN xmap_users_id SET DEFAULT nextval('madi_results.xmap_users_xmap_users_id_seq'::regclass);


--
-- Name: xmap_users workspace_id; Type: DEFAULT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_users ALTER COLUMN workspace_id SET DEFAULT nextval('madi_results.xmap_users_workspace_id_seq'::regclass);


--
-- Name: comparisons comparisons_pkey; Type: CONSTRAINT; Schema: madi_lumi_reader_outliers; Owner: -
--

ALTER TABLE ONLY madi_lumi_reader_outliers.comparisons
    ADD CONSTRAINT comparisons_pkey PRIMARY KEY (id);


--
-- Name: main_context main_context_pkey; Type: CONSTRAINT; Schema: madi_lumi_reader_outliers; Owner: -
--

ALTER TABLE ONLY madi_lumi_reader_outliers.main_context
    ADD CONSTRAINT main_context_pkey PRIMARY KEY (id);


--
-- Name: outliers outliers_pkey; Type: CONSTRAINT; Schema: madi_lumi_reader_outliers; Owner: -
--

ALTER TABLE ONLY madi_lumi_reader_outliers.outliers
    ADD CONSTRAINT outliers_pkey PRIMARY KEY (id);


--
-- Name: comparisons unique_antigen_visits; Type: CONSTRAINT; Schema: madi_lumi_reader_outliers; Owner: -
--

ALTER TABLE ONLY madi_lumi_reader_outliers.comparisons
    ADD CONSTRAINT unique_antigen_visits UNIQUE (context_id, antigen, visit_1, visit_2);


--
-- Name: main_context unique_context_combination; Type: CONSTRAINT; Schema: madi_lumi_reader_outliers; Owner: -
--

ALTER TABLE ONLY madi_lumi_reader_outliers.main_context
    ADD CONSTRAINT unique_context_combination UNIQUE (workspace_id, study, experiment, value_type);


--
-- Name: project_access_keys project_access_keys_project_id_key; Type: CONSTRAINT; Schema: madi_lumi_users; Owner: -
--

ALTER TABLE ONLY madi_lumi_users.project_access_keys
    ADD CONSTRAINT project_access_keys_project_id_key UNIQUE (project_id);


--
-- Name: project_users project_users_pkey; Type: CONSTRAINT; Schema: madi_lumi_users; Owner: -
--

ALTER TABLE ONLY madi_lumi_users.project_users
    ADD CONSTRAINT project_users_pkey PRIMARY KEY (project_id, user_id);


--
-- Name: projects projects_pkey; Type: CONSTRAINT; Schema: madi_lumi_users; Owner: -
--

ALTER TABLE ONLY madi_lumi_users.projects
    ADD CONSTRAINT projects_pkey PRIMARY KEY (project_id);


--
-- Name: users users_auth0_id_key; Type: CONSTRAINT; Schema: madi_lumi_users; Owner: -
--

ALTER TABLE ONLY madi_lumi_users.users
    ADD CONSTRAINT users_auth0_id_key UNIQUE (auth0_id);


--
-- Name: users users_email_key; Type: CONSTRAINT; Schema: madi_lumi_users; Owner: -
--

ALTER TABLE ONLY madi_lumi_users.users
    ADD CONSTRAINT users_email_key UNIQUE (email);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: madi_lumi_users; Owner: -
--

ALTER TABLE ONLY madi_lumi_users.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (user_id);


--
-- Name: xmap_antigen_family xmap_antigen_family_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_antigen_family
    ADD CONSTRAINT xmap_antigen_family_pkey PRIMARY KEY (xmap_antigen_family_id);


--
-- Name: xmap_arm_reference xmap_arm_reference_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_arm_reference
    ADD CONSTRAINT xmap_arm_reference_pkey PRIMARY KEY (xmap_arm_reference_id);


--
-- Name: xmap_buffer xmap_buffer_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_buffer
    ADD CONSTRAINT xmap_buffer_pkey PRIMARY KEY (xmap_buffer_id);


--
-- Name: xmap_control xmap_control_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_control
    ADD CONSTRAINT xmap_control_pkey PRIMARY KEY (xmap_control_id);


--
-- Name: xmap_dilution_analysis xmap_dilution_analysis_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_dilution_analysis
    ADD CONSTRAINT xmap_dilution_analysis_pkey PRIMARY KEY (xmap_dilution_analysis_id);


--
-- Name: xmap_dilution_parameters xmap_dilution_parameters_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_dilution_parameters
    ADD CONSTRAINT xmap_dilution_parameters_pkey PRIMARY KEY (xmap_dilution_parameters_id);


--
-- Name: xmap_header xmap_headerr_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_header
    ADD CONSTRAINT xmap_headerr_pkey PRIMARY KEY (xmap_header_id);


--
-- Name: xmap_planned_visit xmap_planned_visit_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_planned_visit
    ADD CONSTRAINT xmap_planned_visit_pkey PRIMARY KEY (xmap_timeperiod);


--
-- Name: xmap_profile xmap_profile_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_profile
    ADD CONSTRAINT xmap_profile_pkey PRIMARY KEY (xmap_profile_id);


--
-- Name: xmap_sample xmap_sample_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_sample
    ADD CONSTRAINT xmap_sample_pkey PRIMARY KEY (xmap_sample_id);


--
-- Name: xmap_sample_timing xmap_sample_timing_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_sample_timing
    ADD CONSTRAINT xmap_sample_timing_pkey PRIMARY KEY (xmap_sample);


--
-- Name: xmap_standard_fit_tab xmap_standard_fit_tab_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_standard_fit_tab
    ADD CONSTRAINT xmap_standard_fit_tab_pkey PRIMARY KEY (xmap_standard_fit_tab);


--
-- Name: xmap_standard_fits xmap_standard_fits_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_standard_fits
    ADD CONSTRAINT xmap_standard_fits_pkey PRIMARY KEY (xmap_standard_fits);


--
-- Name: xmap_standard xmap_standard_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_standard
    ADD CONSTRAINT xmap_standard_pkey PRIMARY KEY (xmap_standard_id);


--
-- Name: xmap_standard_preds xmap_standard_preds_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_standard_preds
    ADD CONSTRAINT xmap_standard_preds_pkey PRIMARY KEY (xmap_standard_preds);


--
-- Name: xmap_standard_stor xmap_standard_stor_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_standard_stor
    ADD CONSTRAINT xmap_standard_stor_pkey PRIMARY KEY (xmap_standard_stor);


--
-- Name: xmap_study_config xmap_study_config_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_study_config
    ADD CONSTRAINT xmap_study_config_pkey PRIMARY KEY (xmap_study_config_id);


--
-- Name: xmap_subjects xmap_subjects_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_subjects
    ADD CONSTRAINT xmap_subjects_pkey PRIMARY KEY (xmap_patientid);


--
-- Name: xmap_users xmap_users_pkey; Type: CONSTRAINT; Schema: madi_results; Owner: -
--

ALTER TABLE ONLY madi_results.xmap_users
    ADD CONSTRAINT xmap_users_pkey PRIMARY KEY (xmap_users_id);


--
-- Name: idx_comparisons_antigen; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_comparisons_antigen ON madi_lumi_reader_outliers.comparisons USING btree (antigen);


--
-- Name: idx_comparisons_context_id; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_comparisons_context_id ON madi_lumi_reader_outliers.comparisons USING btree (context_id);


--
-- Name: idx_comparisons_value_type; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_comparisons_value_type ON madi_lumi_reader_outliers.comparisons USING btree (value_type);


--
-- Name: idx_comparisons_visit_1; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_comparisons_visit_1 ON madi_lumi_reader_outliers.comparisons USING btree (visit_1);


--
-- Name: idx_comparisons_visit_2; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_comparisons_visit_2 ON madi_lumi_reader_outliers.comparisons USING btree (visit_2);


--
-- Name: idx_main_context_experiment; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_main_context_experiment ON madi_lumi_reader_outliers.main_context USING btree (experiment);


--
-- Name: idx_main_context_job_status; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_main_context_job_status ON madi_lumi_reader_outliers.main_context USING btree (job_status);


--
-- Name: idx_main_context_study; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_main_context_study ON madi_lumi_reader_outliers.main_context USING btree (study);


--
-- Name: idx_main_context_value_type; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_main_context_value_type ON madi_lumi_reader_outliers.main_context USING btree (value_type);


--
-- Name: idx_main_context_workspace_id; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_main_context_workspace_id ON madi_lumi_reader_outliers.main_context USING btree (workspace_id);


--
-- Name: idx_outliers_antigen; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_outliers_antigen ON madi_lumi_reader_outliers.outliers USING btree (antigen);


--
-- Name: idx_outliers_comparison_id; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_outliers_comparison_id ON madi_lumi_reader_outliers.outliers USING btree (comparison_id);


--
-- Name: idx_outliers_feature; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_outliers_feature ON madi_lumi_reader_outliers.outliers USING btree (feature);


--
-- Name: idx_outliers_gate_class_1; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_outliers_gate_class_1 ON madi_lumi_reader_outliers.outliers USING btree (gate_class_1);


--
-- Name: idx_outliers_gate_class_2; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_outliers_gate_class_2 ON madi_lumi_reader_outliers.outliers USING btree (gate_class_2);


--
-- Name: idx_outliers_lab_confirmed; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_outliers_lab_confirmed ON madi_lumi_reader_outliers.outliers USING btree (lab_confirmed);


--
-- Name: idx_outliers_subject_accession; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_outliers_subject_accession ON madi_lumi_reader_outliers.outliers USING btree (subject_accession);


--
-- Name: idx_outliers_visit_1; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_outliers_visit_1 ON madi_lumi_reader_outliers.outliers USING btree (visit_1);


--
-- Name: idx_outliers_visit_2; Type: INDEX; Schema: madi_lumi_reader_outliers; Owner: -
--

CREATE INDEX idx_outliers_visit_2 ON madi_lumi_reader_outliers.outliers USING btree (visit_2);


--
-- Name: projects after_project_insert; Type: TRIGGER; Schema: madi_lumi_users; Owner: -
--

CREATE TRIGGER after_project_insert AFTER INSERT ON madi_lumi_users.projects FOR EACH ROW EXECUTE FUNCTION public.add_project_access_key();


--
-- Name: comparisons comparisons_context_id_fkey; Type: FK CONSTRAINT; Schema: madi_lumi_reader_outliers; Owner: -
--

ALTER TABLE ONLY madi_lumi_reader_outliers.comparisons
    ADD CONSTRAINT comparisons_context_id_fkey FOREIGN KEY (context_id) REFERENCES madi_lumi_reader_outliers.main_context(id);


--
-- Name: outliers outliers_comparison_id_fkey; Type: FK CONSTRAINT; Schema: madi_lumi_reader_outliers; Owner: -
--

ALTER TABLE ONLY madi_lumi_reader_outliers.outliers
    ADD CONSTRAINT outliers_comparison_id_fkey FOREIGN KEY (comparison_id) REFERENCES madi_lumi_reader_outliers.comparisons(id);


--
-- Name: outliers outliers_context_id_fkey; Type: FK CONSTRAINT; Schema: madi_lumi_reader_outliers; Owner: -
--

ALTER TABLE ONLY madi_lumi_reader_outliers.outliers
    ADD CONSTRAINT outliers_context_id_fkey FOREIGN KEY (context_id) REFERENCES madi_lumi_reader_outliers.main_context(id);


--
-- Name: project_access_keys project_access_keys_project_id_fkey; Type: FK CONSTRAINT; Schema: madi_lumi_users; Owner: -
--

ALTER TABLE ONLY madi_lumi_users.project_access_keys
    ADD CONSTRAINT project_access_keys_project_id_fkey FOREIGN KEY (project_id) REFERENCES madi_lumi_users.projects(project_id);


--
-- Name: project_users project_users_project_id_fkey; Type: FK CONSTRAINT; Schema: madi_lumi_users; Owner: -
--

ALTER TABLE ONLY madi_lumi_users.project_users
    ADD CONSTRAINT project_users_project_id_fkey FOREIGN KEY (project_id) REFERENCES madi_lumi_users.projects(project_id);


--
-- PostgreSQL database dump complete
--
