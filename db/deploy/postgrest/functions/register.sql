-- Deploy postgrest-demo:postgrest/functions/register to pg
-- requires: postgrest/tables/account
-- requires: postgrest/roles/account

BEGIN;

SET search_path TO postgrest, public;

CREATE FUNCTION register_account(email text, name text, password text)
   RETURNS text AS  
$$
DECLARE
   aid uuid;
   rstr text;
BEGIN
   aid  := uuid_generate_v4();
   rstr := 'postgrest_account_' || replace(aid::text, '-', '_');

   INSERT INTO account (account_id, email, name, password, role_string)
   VALUES (aid, email, name, password, rstr);

   EXECUTE format('CREATE ROLE %s;', rstr);
   EXECUTE format('GRANT postgrest_account TO %s;', rstr);

   RETURN rstr;
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;

GRANT EXECUTE
   ON FUNCTION register_account(email text, name text, password text)
      TO postgrest_anonymous;

COMMIT;
