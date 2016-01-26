-- Verify postgrest-demo:postgrest/functions/register on pg

BEGIN;

SET search_path TO postgrest, public;

DO $$
DECLARE
   rstr text;
BEGIN
   SET ROLE postgrest_anonymous;
   rstr := register_account('andrew.rademacher@smrxt.com', 'Andrew Rademacher', '12345');

   EXECUTE format('SET ROLE %s;', rstr);
   PERFORM FROM account;
   IF NOT FOUND THEN
      RAISE EXCEPTION 'Account creation failed.';
   END IF;
END$$;

ROLLBACK;
