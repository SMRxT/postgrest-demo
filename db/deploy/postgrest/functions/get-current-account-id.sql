-- Deploy postgrest-demo:postgrest/functions/get-current-account-id to pg
-- requires: postgrest/schema
-- requires: postgrest/tables/account

BEGIN;

SET search_path TO postgrest, public;

CREATE FUNCTION get_current_account_id()
   RETURNS uuid AS
$$
DECLARE
   aid uuid;
BEGIN
   SELECT account_id INTO aid
   FROM account
   WHERE role_string = current_user;

   RETURN aid;
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;

GRANT EXECUTE
   ON FUNCTION get_current_account_id()
      TO postgrest_account;

COMMIT;
