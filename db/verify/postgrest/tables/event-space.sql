-- Verify postgrest-demo:postgrest/tables/event-space on pg

BEGIN;

SET search_path TO postgrest, public;

DO $$
DECLARE
   rstr text;
   esid uuid;
   hcount integer;
   ccount integer;
BEGIN
   rstr := register_account('andrew.rademacher@smrxt.com', 'Andrew Rademacher', '12345');
   EXECUTE format('SET ROLE %s;', rstr);

   INSERT INTO event_space
      (name, address, description)
   VALUES
      ('Creepy Abby', '666 13th St.', 'This place gives me the heebie jeebies.')
   RETURNING event_space_id INTO esid;
   IF hcount != 0 THEN
      RAISE EXCEPTION 'Something in history that should not be. Count: %', hcount;
   END IF;
   IF ccount != 1 THEN
      RAISE EXCEPTION 'Value not added to current table. Count: %', ccount;
   END IF;

   ---

   -- UPDATE event_space
   -- SET address = '666 13th St., Kansas City MO 64106'
   -- WHERE event_space_id = esid;

   -- SELECT count(event_space_id) INTO hcount
   -- FROM ONLY event_space_history;
   -- IF hcount != 1 THEN
      -- RAISE EXCEPTION 'Nothing in event space history table. Count: %', hcount;
   -- END IF;

   ---

   DELETE
   FROM event_space
   WHERE event_space_id = esid;

   SELECT count(event_space_id) INTO hcount
   FROM ONLY event_space_history;
   IF hcount != 1 THEN 
      RAISE EXCEPTION 'Nothing in event space history table. Count: %', hcount;
   END IF;
END$$;

ROLLBACK;
