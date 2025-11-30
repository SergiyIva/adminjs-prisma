import { Enums } from '../types.js';

export const getEnums = (clientModule?: any): Enums => {
  if (!clientModule?.Prisma?.dmmf) {
    throw new Error(
      'clientModule with Prisma.dmmf is required for Prisma v7.\n\n'
      + 'See migration guide: https://github.com/SergiyIva/adminjs-prisma/blob/main/PRISMA_V7_MIGRATION.md',
    );
  }
  const dmmf = clientModule.Prisma.dmmf.datamodel;

  return dmmf.enums.reduce((memo, current) => {
    // eslint-disable-next-line no-param-reassign
    memo[current.name] = current;

    return memo;
  }, {});
}
