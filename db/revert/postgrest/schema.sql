-- Revert postgrest-demo:postgrest/schema from pg

BEGIN;

DROP SCHEMA postgrest;

COMMIT;
