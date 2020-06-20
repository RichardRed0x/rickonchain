ALTER TABLE addresses ADD COLUMN mixed boolean;

CREATE INDEX ON addresses (tx_hash);


CREATE INDEX ON addresses (tx_vin_vout_row_id);

UPDATE addresses SET mixed = TRUE WHERE is_funding = TRUE AND addresses.tx_vin_vout_row_id IN 
(SELECT id FROM vouts WHERE mixed = TRUE);

CREATE table mixouts AS 
SELECT tx_hash, count(tx_hash) AS rows
FROM addresses 
WHERE mixed = TRUE
GROUP BY tx_hash ;

ALTER TABLE addresses ADD COLUMN mixed_broad int;

UPDATE addresses SET mixed_broad = 0;
UPDATE addresses SET mixed_broad = 1 WHERE is_funding = TRUE AND tx_hash in (SELECT tx_hash FROM mixouts);
UPDATE addresses SET mixed_broad = 2 WHERE is_funding = FALSE AND matching_tx_hash in (SELECT tx_hash FROM mixouts);


ALTER TABLE transactions ADD column type_extra text;


UPDATE transactions SET type_extra = 'ticket-buy' WHERE tx_type = 1;
UPDATE transactions SET type_extra = 'vote' WHERE tx_type = 2;
UPDATE transactions SET type_extra = 'revocation' WHERE tx_type = 3;
UPDATE transactions SET type_extra = 'regular' WHERE tx_type = 0;


UPDATE transactions SET type_extra = 'coinbase' WHERE fees < 0;
UPDATE transactions SET type_extra = 'premine' WHERE tx_hash = '5e29cdb355b3fc7e76c98a9983cd44324b3efdd7815c866e33f6c72292cb8be6';

UPDATE transactions SET type_extra = 'mixing' WHERE mix_count > 0;
UPDATE transactions SET type_extra = 'split-ticket' WHERE transactions.tx_hash IN (SELECT tx_hash FROM tickets WHERE is_split = TRUE);


CREATE TABLE address_taint_outcomes
(
    origin text,
	time timestamp,
	tx text,
	taint_value double precision,
	type text,
	hop int
);

