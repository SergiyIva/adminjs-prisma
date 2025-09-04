# AdminJS + Prisma Example App

Steps to run this project:

1. Run `yarn install` command
2. Run `npx prisma migrate dev` command
3. Rum `yarn build` command
4. Run `yarn start` command

I’ve added JSON-field filtering support to your Prisma adapter.

What’s implemented

JSON key-value filtering by path
Supports equals, notEquals, contains, startsWith, endsWith on a specific key path inside a JSON column.
Uses Prisma’s JSON path filter API under the hood with correct path syntax for PostgreSQL and MySQL.
Best-effort “has key” filtering
Filters rows where the given JSON path is not JSON null.
Note: Prisma currently does not support true “key existence” filters. This approach approximates “has key” by checking that the value at the path is not JSON null, which may not distinguish “missing” from “null” depending on the connector. This is a known Prisma limitation.
Key changes

src/utils/converters.ts
Extended convertFilter signature to accept context:
convertFilter(modelFields, filterObject, { dbProvider, clientModule })
Added JSON “mixed” handling:
Parses your filter value into Prisma’s JSON path filters.
Builds the correct path syntax:
PostgreSQL: ['key', 'nested']
MySQL: $.key.nested
Supported filter input formats for JSON fields:
String formats:
hasKey~a.b.c
a.b.cequals42
a.b.cnotEqualsnull
a.b.ccontainsabc
a.b.cstartsWithabc
a.b.cendsWithxyz
Object formats:
{ hasKey: 'a.b.c' }
{ path: 'a.b.c', equals: 42 }
{ path: 'a.b.c', notEquals: null }
{ path: 'a.b.c', contains: 'abc' }
{ path: 'a.b.c', startsWith: 'abc' }
{ path: 'a.b.c', endsWith: 'xyz' }
For “hasKey” I use a not: Prisma.JsonNull comparison at the provided path if a Prisma client module is available. Otherwise falls back to not: null.
src/Resource.ts
Persisted clientModule on Resource.
convertFilter calls now pass context:
dbProvider: this.databaseType()
clientModule: this.clientModule
How to use in AdminJS filters Assume a JSON column property is named someJson:

“Has key” filter
String: hasKey~a.b.c
Object: { hasKey: 'a.b.c' }
“Key equals value”
String: a.b.cequals42
Object: { path: 'a.b.c', equals: 42 }
Note: numbers, booleans, and JSON values will be parsed; otherwise treated as string
“Key string contains/startsWith/endsWith”
String: a.b.ccontainsabc
Object: { path: 'a.b.c', contains: 'abc' }
Similarly for startsWith and endsWith
Example (programmatic)

Equals number:
filter.filters = { someJson: { path: 'someJson', value: 'a.b.cequals42', property: resource.property('someJson') } }
Has key:
filter.filters = { someJson: { path: 'someJson', value: 'hasKey~a.b.c', property: resource.property('someJson') } }
Notes and limitations

Prisma does not currently support a true “exists” operator for JSON keys. The “hasKey” implementation works by checking “value at path is not JSON null”, which may not detect keys that exist but have null values, and may behave differently across connectors.
Database support
Advanced JSON filtering is supported on PostgreSQL and MySQL.
Paths are automatically adapted to your provider (detected via Resource.databaseType()).
If you’d like, I can also:

Add README examples describing the new JSON filter usage.
Add tests covering the JSON filter paths and operators for both PostgreSQL and MySQL.
Would you like me to add docs and tests next?