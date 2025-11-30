export const getModelByName = (name: string, clientModule?: any) => {
  if (!clientModule?.Prisma?.dmmf) {
    throw new Error(
      'clientModule with Prisma.dmmf is required for Prisma v7.\n\n'
      + 'See migration guide: https://github.com/SergiyIva/adminjs-prisma/blob/main/PRISMA_V7_MIGRATION.md',
    );
  }
  const dmmf = clientModule.Prisma.dmmf.datamodel;

  const model = dmmf.models.find(({ name: modelName }) => modelName === name);

  if (!model) {
    throw new Error(`Could not find model: "${name}" in Prisma's DMMF!`);
  }

  return model;
};
