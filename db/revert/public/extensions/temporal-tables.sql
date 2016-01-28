-- Revert postgrest-demo:public/extensions/temporal-tables from pg

BEGIN;

DROP EXTENSION temporal_tables;

COMMIT;
