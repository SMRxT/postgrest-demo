-- Revert postgrest-demo:postgrest/functions/register from pg

BEGIN;

SET search_path TO postgrest, public;

DROP FUNCTION register_account(text, text, text);

COMMIT;
