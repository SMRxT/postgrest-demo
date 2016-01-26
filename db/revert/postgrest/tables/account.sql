-- Revert postgrest-demo:postgrest/tables/account from pg

BEGIN;

SET search_path TO postgrest, public;

DROP TABLE account;

COMMIT;
