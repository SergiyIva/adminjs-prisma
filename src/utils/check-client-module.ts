/**
 * Helper function to validate clientModule structure for debugging
 */
export const checkClientModule = (clientModule: any): void => {
  console.log('=== Checking clientModule ===');
  console.log('clientModule exists:', !!clientModule);
  console.log('clientModule.Prisma exists:', !!clientModule?.Prisma);
  console.log('clientModule.Prisma.dmmf exists:', !!clientModule?.Prisma?.dmmf);
  
  if (clientModule?.Prisma) {
    console.log('Prisma keys:', Object.keys(clientModule.Prisma));
    console.log('Prisma.dmmf:', clientModule.Prisma.dmmf);
  }
  
  console.log('=== End Check ===');
};

/**
 * Get DMMF from clientModule with fallback for different Prisma versions
 */
export const getDMMF = (clientModule: any) => {
  // Try direct access (Prisma v7)
  if (clientModule?.Prisma?.dmmf?.datamodel) {
    return clientModule.Prisma.dmmf.datamodel;
  }
  
  // Try alternative access patterns
  if (clientModule?.dmmf?.datamodel) {
    return clientModule.dmmf.datamodel;
  }
  
  // Check if Prisma is a getter or requires initialization
  if (clientModule?.Prisma && typeof clientModule.Prisma === 'object') {
    const prisma = clientModule.Prisma;
    
    // Log available properties for debugging
    console.warn('Prisma namespace found but dmmf not accessible. Available properties:', 
      Object.getOwnPropertyNames(prisma).filter(p => !p.startsWith('_')).slice(0, 10));
  }
  
  return null;
};
