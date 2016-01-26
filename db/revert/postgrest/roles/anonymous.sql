-- Revert postgrest-demo:postgrest/roles/anonymous from pg

BEGIN;

DROP ROLE postgrest_anonymous;

COMMIT;
