-- Deploy postgrest-demo:public/extensions/uuid-ossp to pg

BEGIN;

CREATE EXTENSION "uuid-ossp";

COMMIT;
