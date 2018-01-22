ALTER TABLE nis ADD COLUMN nis_key2 BIGINT;
UPDATE nis SET nis_key2=nis_key;
ALTER TABLE nis DROP COLUMN nis_key;
ALTER TABLE nis ADD COLUMN nis_key BIGINT;
UPDATE nis SET nis_key=nis_key2;
ALTER TABLE nis DROP COLUMN nis_key2;