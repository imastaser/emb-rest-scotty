{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module DB.SchemeQuery where

import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types(Query)

bootstrapQ :: Query
bootstrapQ = [sql|
CREATE TABLE IF NOT EXISTS schema_migration (
    id              serial      NOT NULL,
    migration       int         NOT NULL,
    time            timestamptz NOT NULL DEFAULT now(),

    PRIMARY KEY (id),
    UNIQUE (migration)
);
|]


personQ :: Query
personQ = [sql|
CREATE TABLE IF NOT EXISTS person (
    id              serial      NOT NULL,
    firstName       character varying(32) NOT NULL,
    lastName        character varying(64) NULL,
    phone           character varying(32) NOT NULL,
    phone2          character varying(32) NULL,
    email           character varying(32) NULL,
    note            text        NULL,
    time            timestamptz NOT NULL DEFAULT now(),

    PRIMARY KEY (id)
);
|]

productQ :: Query
productQ = [sql|
CREATE TABLE IF NOT EXISTS product (
    id              serial NOT NULL,
    person_id       serial NOT NULL,
    workType_id     serial NOT NULL,
    name            character varying(64)   NOT NULL,
    price           integer   NULL,
    caxs            integer   NULL,
    stitches        integer   NULL,
    note            text NULL,  
    time            timestamptz NOT NULL DEFAULT now(),

    PRIMARY KEY (id)
);
|] 


workTypeQ :: Query
workTypeQ = [sql|
CREATE TABLE IF NOT EXISTS workType (
    id              serial NOT NULL,
    name            character varying(64) NOT NULL,
    note            text NULL,  
    PRIMARY KEY (id)
);
|] 

-- order is reserved word in postgresql, should use public."order" or "order" in queries
orderCreateQ :: Query
orderCreateQ = [sql|
CREATE TABLE IF NOT EXISTS "order" (
    id              serial  NOT NULL,
    person_id       serial  NOT NULL,
    product_id      serial  NOT NULL,
    price           integer NOT NULL,
    caxs            integer NOT NULL,
    count           integer NOT NULL,
    datetime        text NOT NULL,
    note            text NULL, 
    PRIMARY KEY (id)
);
|] 
-- timestamptz