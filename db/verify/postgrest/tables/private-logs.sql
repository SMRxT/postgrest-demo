-- Verify postgrest-demo:postgrest/tables/private-logs on pg

BEGIN;

SET search_path TO postgrest, public;

DO $$
DECLARE
   rstr text;
BEGIN
   rstr := register_account('andrew.rademacher@smrxt.com', 'Andrew Rademacher', '12345');
   EXECUTE format('SET ROLE %s;', rstr);

   INSERT INTO private_log(body) VALUES ('This is my test log entry.');
END$$;

ROLLBACK;
