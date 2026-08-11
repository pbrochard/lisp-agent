# MongoDB Skill

This skill provides a wrapper for interacting with a MongoDB instance using the `mongosh` command-line tool via Common Lisp.

## Functions

### `(mongo-eval expr)`
Executes a raw MongoDB shell expression.

- **Arguments**: `expr` (String) - The MongoDB shell command to execute.
- **Returns**: The output of the command as a string.
- **Use Case**: Use this for administrative commands, inserts, updates, or complex shell scripts.
- **Example**:
  ```lisp
  (mongo-eval "db.adminCommand({ listDatabases: 1 })")
  ```

### `(mongo-query db collection query)`
A high-level helper for reading documents from a specific collection.

- **Arguments**:
  - `db` (String): The name of the target database.
  - `collection` (String): The name of the collection.
  - `query` (String): The MongoDB query filter (JSON string).
- **Returns**: A JSON array string of the matching documents.
- **Use Case**: Quick retrieval of documents from a known database and collection.
- **Example**:
  ```lisp
  ;; Fetch all documents from the 'users' collection in 'prod_db'
  (mongo-query "prod_db" "users" "{}")

  ;; Fetch documents where status is 'active'
  (mongo-query "prod_db" "users" "{ status: 'active' }")
  ```

## Requirements
- `mongosh` must be installed and available in the system PATH.
- The `uiop` library must be available in the Lisp environment.
