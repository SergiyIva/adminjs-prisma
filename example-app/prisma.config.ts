import dotenv from 'dotenv';
import dotenvExpand from 'dotenv-expand';
import { defineConfig, env } from 'prisma/config';

const result = dotenv.config();
dotenvExpand.expand(result);

export default defineConfig({
  // the main entry for your schema
  schema: 'prisma',
  // where migrations should be generated
  // what script to run for "prisma db seed"
  migrations: {
    path: 'prisma/migrations',
  },
  // The database URL
  datasource: {
    // Type Safe env() helper
    // Does not replace the need for dotenv
    url: env('DATABASE_URL'),
  },
});
