-- Verify postgrest-demo:postgrest/functions/get-current-account-id on pg

BEGIN;

SET search_path TO postgrest, public;

DO $$
DECLARE
   rstr text;
   tmpaid uuid;
BEGIN
   rstr := register_account('andrew.rademacher@smrxt.com', 'Andrew Rademacher', '12345');

   EXECUTE format('SET ROLE %s;', rstr);
   
   SELECT account_id INTO tmpaid
   FROM account;

   IF NOT (tmpaid = get_current_account_id()) THEN 
      RAISE EXCEPTION 'Account id from function and table are not equal.';
   END IF;
END$$;

ROLLBACK;
