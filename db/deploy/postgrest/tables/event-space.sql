-- Deploy postgrest-demo:postgrest/tables/event-space to pg
-- requires: public/extensions/temporal-tables
-- requires: postgrest/roles/account
-- requires: postgrest/functions/get-current-account-id
-- requires: postgrest/tables/account

BEGIN;

SET search_path TO postgrest, public;

-- History Table

CREATE TABLE event_space_history(
   event_space_id       uuid           NOT NULL DEFAULT uuid_generate_v4(),
   name                 text           NOT NULL,
   address              text           NOT NULL,
   description          text,

   valid_between        tstzrange      NOT NULL DEFAULT tstzrange(current_timestamp, null),
   owned_by             uuid           NOT NULL DEFAULT get_current_account_id()
);

CREATE INDEX event_space_history_idx_event_space_id
   ON event_space_history(event_space_id);

CREATE INDEX event_space_history_idx_valid_between
   ON event_space_history(valid_between);

CREATE INDEX event_space_history_idx_owned_by
   ON event_space_history(owned_by);

ALTER TABLE event_space_history
   ENABLE ROW LEVEL SECURITY;

CREATE POLICY view_owned ON event_space_history
   TO postgrest_account
      USING (owned_by = get_current_account_id());

GRANT SELECT
   ON event_space_history TO postgrest_account;

-- Current Table

CREATE TABLE event_space(
   PRIMARY KEY (event_space_id),
   UNIQUE (name, owned_by)
) INHERITS (event_space_history);

CREATE INDEX event_space_idx_event_space_id
   ON event_space(event_space_id);

CREATE INDEX event_space_idx_owned_by
   ON event_space(owned_by);

ALTER TABLE event_space
   ENABLE ROW LEVEL SECURITY;

CREATE POLICY view_owned ON event_space
   TO postgrest_account
      USING (owned_by = get_current_account_id());

GRANT SELECT, INSERT, UPDATE, DELETE
   ON event_space TO postgrest_account;

-- Versioning Trigger

CREATE TRIGGER tr_event_space_versioning
BEFORE INSERT OR UPDATE OR DELETE ON event_space
   FOR EACH ROW EXECUTE PROCEDURE versioning(
      'valid_between', 'event_space_history', true
   );

COMMIT;
