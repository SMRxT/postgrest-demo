-- Revert postgrest-demo:postgrest/roles/account from pg

BEGIN;

DROP ROLE postgrest_account;

COMMIT;
