-- Deploy postgrest-demo:public/extensions/temporal-tables to pg

BEGIN;

CREATE EXTENSION temporal_tables;

COMMIT;
