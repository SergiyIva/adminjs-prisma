import { PrismaPg } from '@prisma/adapter-pg';

import { getModelByName } from '../../lib/index.js';
import { Prisma, PrismaClient } from './client-prisma/client.js';

const adapter = new PrismaPg({
  connectionString: process.env.DATABASE_URL,
});

export const prisma = new PrismaClient({
  adapter,
  log:
    process.env.NODE_ENV === 'production'
      ? undefined
      : ['query', 'info', 'warn', 'error'],
});

const clientModulePrisma = { Prisma };

export const getDBResource = (modelName: string) => ({
  model: getModelByName(modelName, clientModulePrisma),
  client: prisma,
  clientModule: clientModulePrisma,
});