-- Revert postgrest-demo:public/extensions/uuid-ossp from pg

BEGIN;

DROP EXTENSION "uuid-ossp";

COMMIT;
