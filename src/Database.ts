/* eslint-disable class-methods-use-this */
import { PrismaClient } from '@prisma/client';
import type { DMMF } from '@prisma/client/runtime/client';
import { BaseDatabase } from 'adminjs';

import { Resource } from './Resource.js';

export class Database extends BaseDatabase {
  protected client: PrismaClient;

  protected clientModule?: any;

  public constructor(args: { client: PrismaClient, clientModule?: any }) {
    super(args);
    const { client, clientModule } = args;

    this.client = client;
    this.clientModule = clientModule;
  }

  public resources(): Array<Resource> {
    // In Prisma v7, dmmf must be provided via clientModule
    if (!this.clientModule?.Prisma?.dmmf) {
      throw new Error(
        'clientModule with Prisma.dmmf is required for Prisma v7.\n\n'
        + 'Usage:\n'
        + '  import { Prisma, PrismaClient } from "./your-generated-client/client.js";\n'
        + '  const prisma = new PrismaClient();\n'
        + '  const db = new Database({ client: prisma, clientModule: { Prisma } });\n\n'
        + 'See migration guide: https://github.com/SergiyIva/adminjs-prisma/blob/main/PRISMA_V7_MIGRATION.md',
      );
    }
    const dmmf = this.clientModule.Prisma.dmmf.datamodel;

    if (!dmmf?.models) return [];

    return dmmf.models.map((model: DMMF.Model) => {
      const resource = new Resource({ model, client: this.client });
      return resource;
    });
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  public static isAdapterFor(args: { client?: PrismaClient, clientModule?: any }): boolean {
    const { clientModule } = args;

    // In Prisma v7, dmmf must be provided via clientModule
    if (!clientModule?.Prisma?.dmmf) {
      return false;
    }
    const dmmf = clientModule.Prisma.dmmf.datamodel;

    return dmmf?.models?.length > 0;
  }
}
