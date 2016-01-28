-- Revert postgrest-demo:postgrest/tables/event-space from pg

BEGIN;

SET search_path TO postgrest, public;

DROP TRIGGER tr_event_space_versioning ON event_space;

DROP TABLE event_space;
DROP TABLE event_space_history;

COMMIT;
