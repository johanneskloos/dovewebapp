CREATE TABLE users (
	username VARCHAR(30) PRIMARY KEY,
	password VARCHAR(100),
	token CHAR(32),
	token_expires DATE,
	alternative_email VARCHAR(100),
	admin TINYINT NOT NULL
);

CREATE TABLE sessions (
sessionid CHAR(32) PRIMARY KEY,
session_expires DATE NOT NULL,
username VARCHAR(30) NOT NULL)

