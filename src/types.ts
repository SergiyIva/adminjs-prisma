import type { DMMF } from '@prisma/client/runtime/client';

export type ModelManager = {
  [action in DMMF.ModelAction]: (...args: any[]) => Promise<any>;
};

export type Enums = { [key: string]: DMMF.DatamodelEnum };
