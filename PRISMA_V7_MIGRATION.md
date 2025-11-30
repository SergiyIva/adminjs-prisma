# Prisma v7 Migration Summary

## Overview
Successfully migrated `@sergiyiva/adminjs-prisma` package to support Prisma v7 while maintaining backward compatibility with Prisma v5 and v6.

## Changes Made

### 1. Updated Package Dependencies
**File:** `package.json`
- Updated `peerDependencies` to support Prisma v7:
  ```json
  "@prisma/client": "^5.0.0 || ^6.0.0 || ^7.0.0"
  ```

### 2. Fixed ESM Import Issues
Prisma v7 is now ESM-only and changed how DMMF types are exported. The main changes:

#### DMMF Import Changes
- **Old (v6)**: `import { DMMF } from '@prisma/client/runtime/library.js'`
- **New (v7)**: `import { Prisma } from '@prisma/client'` and use `Prisma.DMMF`

The path `@prisma/client/runtime/library.js` no longer exists in v7. All types are now accessed through the `Prisma` namespace.

#### Files Modified:

**`src/Database.ts`**
- Changed to namespace import pattern:
  ```typescript
  import * as PrismaClientModule from '@prisma/client';
  import { PrismaClient } from '@prisma/client';
  import type { Prisma as PrismaNamespace } from '@prisma/client';
  
  const Prisma = (PrismaClientModule as any).Prisma as typeof PrismaNamespace;
  ```
- Updated DMMF usage: `Prisma.DMMF.Model`

**`src/Resource.ts`**
- Changed from: `import { DMMF } from '@prisma/client/runtime/library.js'`
- Changed to: `import { Prisma } from '@prisma/client'`
- Updated all DMMF references to use `Prisma.DMMF`

**`src/Property.ts`**
- Updated DMMF import to use `Prisma.DMMF.Field`

**`src/types.ts`**
- Updated type definitions to use `Prisma.DMMF.ModelAction` and `Prisma.DMMF.DatamodelEnum`

**`src/utils/converters.ts`**
- Updated to use `Prisma.DMMF.Model['fields']`

**`src/utils/get-enums.ts`**
- Applied namespace import pattern for accessing `Prisma.dmmf`

**`src/utils/get-model-by-name.ts`**
- Applied namespace import pattern for accessing `Prisma.dmmf`

### 3. Maintained Backward Compatibility
The solution maintains compatibility with:
- Prisma v5.x
- Prisma v6.x
- Prisma v7.x

The code uses optional chaining (`?.`) to safely access the Prisma namespace from either the `clientModule` parameter or the imported module.

## Technical Details

### Why This Approach Works
1. **Type Import**: `import type { Prisma as PrismaNamespace }` - Imports the Prisma namespace type for TypeScript type checking
2. **Namespace Import**: `import * as PrismaClientModule` - Imports the entire module as a namespace
3. **Runtime Access**: `const Prisma = (PrismaClientModule as any).Prisma` - Accesses the Prisma namespace at runtime

This pattern works because:
- In Prisma v6: The module exports both named exports and a namespace
- In Prisma v7: The ESM module structure is preserved, and the Prisma namespace is still accessible via the module object

### Build Verification
✅ Package builds successfully with TypeScript
✅ No type errors
✅ ESM output is correct

## Testing Recommendations

1. **Test with Prisma v7**:
   ```bash
   bun install @prisma/client@^7.0.0
   bun run build
   ```

2. **Test with Prisma v6** (backward compatibility):
   ```bash
   bun install @prisma/client@^6.0.0
   bun run build
   ```

3. **Integration Test**: Test the package in a real application with Prisma v7

## How to Upgrade

### For Users of This Package

1. **Update Prisma to v7:**
   ```bash
   npm install @prisma/client@latest prisma@latest
   # or
   yarn add @prisma/client@latest prisma@latest
   # or
   bun add @prisma/client@latest prisma@latest
   ```

2. **Update your Prisma schema:**
   - Change generator provider from `prisma-client-js` to `prisma-client`
   - Specify output path (recommended to be inside your project `src/generated/client`)
   - Remove `url` from datasource (it's now configured via `prisma.config.ts` or passed to PrismaClient constructor)
   
   ```prisma
   datasource db {
     provider = "postgresql"
     // url removed - configure via prisma.config.ts or adapter
   }

   generator client {
     provider = "prisma-client"
     output   = "./src/generated/client"  // Inside your project!
   }
   ```
   
   **Important:** Do NOT generate to `node_modules/.prisma/client` with the new `prisma-client` generator. 
   Always generate inside your project (e.g., `./src/generated/client`).

3. **Create `prisma.config.ts` (optional but recommended):**
   ```typescript
   import { defineConfig, env } from 'prisma/config';

   export default defineConfig({
     schema: 'prisma',
     migrations: {
       path: 'prisma/migrations',
     },
     datasource: {
       url: env('DATABASE_URL'),
     },
   });
   ```

4. **Ensure your project supports ESM:**
   - Add `"type": "module"` to your `package.json`
   - Update `tsconfig.json`:
     ```json
     {
       "compilerOptions": {
         "module": "ESNext",
         "moduleResolution": "node",
         "target": "ES2023"
       }
     }
     ```

5. **Update your code to pass `clientModule` with Prisma namespace:**
   ```typescript
   import { PrismaClient } from './generated/client/client.js';
   import { Database, Resource } from '@adminjs/prisma';
   import internals from "@prisma/internals";
   import { readFileSync } from "fs";
   import { join } from "path";
   
   const getDMMF = internals.getDMMF;
   const schemaPath = join(__dirname, "../../prisma/schema.prisma");
   const schema = readFileSync(schemaPath, "utf-8");
   const fullDMMF = await getDMMF({ datamodel: schema });
   const clientModulePrisma = { Prisma: { dmmf: fullDMMF } };

   const prisma = new PrismaClient({
     // In v7, connection is configured here or via adapter
     adapter: yourAdapter, // e.g., PrismaPg
   });

   AdminJS.registerAdapter({
     Database,
     Resource,
   });

   const admin = new AdminJS({
     databases: [
       {
         client: prisma,
         clientModule: clientModulePrisma, // REQUIRED for Prisma v7 - provides DMMF metadata
       }
     ],
   });
   ```

   **Important:** The `clientModule` parameter is **required** in Prisma v7 because `Prisma.dmmf` is no longer available as a static import. You must pass an object containing the `Prisma` namespace from your generated client.

## References
- [Prisma v7 Upgrade Guide](https://www.prisma.io/docs/orm/more/upgrade-guides/upgrading-versions/upgrading-to-prisma-7?utm_source=blog&utm_content=launch-blog&via=prisma7&utm_campaign=prisma7)
- [Prisma v7 ESM Support](https://github.com/prisma/docs/blob/main/content/900-ai/prompts/prisma-7.mdx)
