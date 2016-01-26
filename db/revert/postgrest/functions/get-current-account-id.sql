-- Revert postgrest-demo:postgrest/functions/get-current-account-id from pg

BEGIN;

SET search_path TO postgrest, public;

DROP FUNCTION get_current_account_id();

COMMIT;
