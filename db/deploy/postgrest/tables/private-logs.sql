-- Deploy postgrest-demo:postgrest/tables/private-logs to pg
-- requires: postgrest/schema
-- requires: postgrest/tables/account
-- requires: postgrest/roles/account

BEGIN;

SET search_path TO postgrest, public;

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
   TO postgrest_account
      USING (owned_by = get_current_account_id());

GRANT SELECT, INSERT
   ON private_log TO postgrest_account;

GRANT USAGE
   ON private_log_private_log_id_seq to postgrest_account;

COMMIT;
