-- Deploy postgrest-demo:postgrest/functions/register to pg
-- requires: postgrest/tables/account
-- requires: postgrest/roles/account

BEGIN;

SET search_path TO postgrest, public;

CREATE FUNCTION register_account(email text, name text, password text)
   RETURNS uuid AS  
$$
DECLARE
   aid uuid;
   rstr text;
BEGIN
   rstr := 'postgrest_account|' + email; 
   INSERT INTO account (email, name, password, role_string)
   VALUES (email, name, password, rstr)
   RETURNING acccount_id into aid;

   EXECUTE format('CREATE ROLE %s;', rstr);
   EXECUTE format('GRANT postgrest_account TO %s', rstr);
END;
$$ LANGUAGE plpgsql;

GRANT EXECUTE
   ON FUNCTION register_account(text,text,text);
      TO postgrest_anonymous;

COMMIT;
