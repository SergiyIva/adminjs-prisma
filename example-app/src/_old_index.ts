/* eslint-disable no-underscore-dangle */
import express from 'express';
import AdminJS from 'adminjs';
import AdminJSExpress from '@adminjs/express';
//@ts-ignore
import { Database, Resource, getModelByName } from '../../lib/index.js';
import { PrismaPg } from '@prisma/adapter-pg';

// eslint-disable-next-line import/no-relative-packages
import { Prisma, PrismaClient } from './client-prisma/client.js';

const PORT = process.env.port || 3000;

const adapter = new PrismaPg({
  connectionString: process.env.DATABASE_URL,
});
const prisma = new PrismaClient({
  adapter,
});

const PrismaModule = { Prisma }

AdminJS.registerAdapter({ Database: Database as any, Resource: Resource as any });

const run = async () => {
  const app = express();

  const admin = new AdminJS({
    resources: [{
      resource: { model: getModelByName('Post', PrismaModule), client: prisma, clientModule: PrismaModule },
      options: {
        properties: {
          someJson: { type: 'mixed', isArray: true },
          'someJson.number': { type: 'number' },
          'someJson.string': { type: 'string' },
          'someJson.boolean': { type: 'boolean' },
          'someJson.date': { type: 'datetime' },
        },
      },
    }, {
      resource: { model: getModelByName('Profile', PrismaModule), client: prisma, clientModule: PrismaModule },
      options: {},
    }, {
      resource: { model: getModelByName('Publisher', PrismaModule), client: prisma, clientModule: PrismaModule },
      options: {},
    }],
  });

  const router = AdminJSExpress.buildRouter(admin);

  app.use(admin.options.rootPath, router);

  app.listen(PORT, () => {
    // eslint-disable-next-line no-console
    console.log(`Example app listening at http://localhost:${PORT}`);
  });
};

run()
  .finally(async () => {
    await prisma.$disconnect();
  });
