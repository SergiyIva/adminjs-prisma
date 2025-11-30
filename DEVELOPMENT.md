# Development Guide

## Working with Prisma v7 in This Library

This is a library/adapter, so the approach to Prisma Client is different from a regular application:

### For Library Development

1. **Prisma Client Location:**
   - For development and testing, we generate Prisma Client to `node_modules/.prisma/client`
   - This is only for TypeScript type-checking during library development
   - The actual Prisma Client used by end-users comes from their own project

2. **Generate Prisma Client:**
   ```bash
   bun run prisma:generate
   ```
   
   This will:
   - Generate Prisma Client from `spec/prisma/schema.prisma`
   - Create necessary files for TypeScript to work with `@prisma/client` imports
   
3. **Build:**
   ```bash
   bun run build
   ```

4. **Lint:**
   ```bash
   bun run lint
   ```

### For End-Users

End-users should:
1. Generate Prisma Client **inside their project** (e.g., `./src/generated/client`)
2. NOT generate to `node_modules/.prisma/client` with Prisma v7's `prisma-client` generator
3. Import from their generated client path
4. Pass `clientModule` with the `Prisma` namespace to this adapter

See [PRISMA_V7_MIGRATION.md](./PRISMA_V7_MIGRATION.md) for user migration instructions.

## Key Points

- **This library**: Uses `@prisma/client` as `peerDependency` and `devDependency`
- **Library code**: Imports types from `@prisma/client/runtime/client` directly
- **End-user apps**: Generate their own Prisma Client and pass it via `clientModule`

## Prisma v7 Generator Note

The new `prisma-client` generator in Prisma v7 generates TypeScript files directly.
For library development in `node_modules`, we need to manually create `default.ts` file (done by `prisma:generate` script).
End-users don't need this workaround when generating to their project directory.
