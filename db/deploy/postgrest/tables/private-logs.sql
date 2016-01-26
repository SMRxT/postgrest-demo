-- Deploy postgrest-demo:postgrest/tables/private-logs to pg
-- requires: postgrest/schema
-- requires: postgrest/tables/account
-- requires: postgrest/roles/account

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

--

CREATE TABLE private_log(
   private_log_id          bigserial      NOT NULL,
   entered_at              timestamptz    NOT NULL DEFAULT now(),
   body                    text           NOT NULL,
   owned_by                uuid           NOT NULL DEFAULT get_current_account_id(),

   PRIMARY KEY (private_log_id)
);

CREATE INDEX private_log_idx_entered_at
   ON private_log(entered_at);

CREATE INDEX private_log_idx_owned_by
   ON private_log(owned_by);

ALTER TABLE private_log
   ENABLE ROW LEVEL SECURITY;

CREATE POLICY view_owned ON private_log 
   FOR SELECT TO postgrest_account
   USING (owned_by = (SELECT account_id FROM account WHERE role_string = current_user));

CREATE POLICY no_owner_on_insert ON private_log
   FOR INSERT TO postgrest_account
      WITH CHECK (owned_by = null);

-- CREATE POLICY free_insert ON private_log
   -- FOR INSERT TO postgrest_account
      -- WITH CHECK (true);

GRANT SELECT, INSERT
   ON private_log TO postgrest_account;

GRANT USAGE
   ON private_log_private_log_id_seq to postgrest_account;

--

-- CREATE FUNCTION private_log_owner_tag()
   -- RETURNS trigger AS
-- $$
-- DECLARE
   -- aid uuid;
-- BEGIN
   -- SELECT account_id INTO aid
   -- FROM account 
   -- WHERE role_string = current_user;

   -- NEW.owned_by := aid;
   -- RETURN NEW;
-- END;
-- $$ LANGUAGE plpgsql SECURITY DEFINER;

-- CREATE TRIGGER private_log_set_owner 
   -- BEFORE INSERT ON private_log
   -- FOR EACH ROW
   -- EXECUTE PROCEDURE private_log_owner_tag();
   
COMMIT;
