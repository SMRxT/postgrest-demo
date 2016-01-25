-- Deploy postgrest-demo:postgrest/tables/account to pg
-- requires: postgrest/schema

BEGIN;

SET search_path TO postgrest, public;

CREATE TABLE account(
   account_id           uuid     NOT NULL DEFAULT uuid_generate_v4(),
   email                text     NOT NULL,
   name                 text     NOT NULL,
   password             text     NOT NULL,
   role_string          text     NOT NULL,

   PRIMARY KEY (account_id),
   UNIQUE (email),
   UNIQUE (role_string)
);

-- CREATE ROLE postgrest_anonymous;
-- CREATE ROLE postgrest_account;

-- ALTER TABLE account ENABLE ROW LEVEL SECURITY;

-- CREATE POLICY view_owned ON account
   -- FOR SELECT TO postgrest_account
      -- USING (account_id = (SELECT account_id FROM account WHERE role_string = current_user))

-- GRANT SELECT ON account TO postgrest_account;

COMMIT;
