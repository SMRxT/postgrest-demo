-- Revert postgrest-demo:postgrest/tables/private-logs from pg

BEGIN;

SET search_path TO postgrest, public;

DROP TABLE private_log;

COMMIT;
