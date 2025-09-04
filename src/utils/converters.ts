/* eslint-disable no-param-reassign */
import { DMMF } from '@prisma/client/runtime/library.js';
import { Filter } from 'adminjs';

import { Property } from '../Property.js';
import { safeParseJSON, safeParseNumber } from './helpers.js';

const OPERATOR_SEPARATOR = '~';

const MATCHING_PATTERNS = {
  EQ: 'equals',
  NE: 'notEquals',
  CO: 'contains',
  EW: 'endsWith',
  SW: 'startsWith',
};

const OPERATORS = {
  AND: 'and',
  OR: 'or',
};

export const convertParam = (
  property: Property,
  fields: DMMF.Model['fields'],
  value: string | boolean | number | Record<string, any> | null | undefined,
): string | boolean | number | Record<string, any> | null | undefined => {
  const type = property.type();

  if (type === 'mixed') return value;
  if (type === 'number') {
    return safeParseNumber(value);
  }
  if (type === 'reference') {
    const foreignColumn = fields.find((field) => field.name === property.foreignColumnName());
    if (!foreignColumn) return value;
    if (value === undefined || value === null) return value;

    const foreignColumnType = foreignColumn.type;
    if (foreignColumnType === 'String') return String(value);

    return safeParseNumber(value);
  }

  return value;
};

type ConvertFilterContext = {
  dbProvider?: string; // e.g. 'postgresql' | 'mysql' | others
  clientModule?: any; // optional Prisma client module to access NullTypes if needed
};

const buildJsonPath = (rawPath: string, dbProvider?: string): string | string[] => {
  const parts = rawPath
    .replace(/^\$\.?/, '') // strip possible leading JSONPath '$.' if provided
    .split('.')
    .filter(Boolean);
  if (dbProvider === 'mysql') {
    return `$.${parts.join('.')}`;
  }
  // default to PostgreSQL-style array path
  return parts;
};

export const convertFilter = (
  modelFields: DMMF.Model['fields'],
  filterObject?: Filter,
  context: ConvertFilterContext = {},
): Record<string, any> => {
  if (!filterObject) return {};

  const uuidRegex = /^[0-9A-F]{8}-[0-9A-F]{4}-[5|4|3|2|1][0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$/i;
  const { filters = {} } = filterObject;
  const { dbProvider, clientModule } = context;
  return Object.entries(filters).reduce<{[key: string]: any}>((where, [name, filter]) => {
    if (['boolean', 'number', 'float', 'object', 'array'].includes(filter.property.type())) {
      where[name] = safeParseJSON(filter.value as string);
    } else if (['date', 'datetime'].includes(filter.property.type())) {
      if (filter.value === null) {
        where[name] = { equals: null };
      } else if (typeof filter.value !== 'string' && filter.value.from && filter.value.to) {
        where[name] = { gte: new Date(filter.value.from), lte: new Date(filter.value.to) };
      } else if (typeof filter.value !== 'string' && filter.value.from) {
        where[name] = { gte: new Date(filter.value.from) };
      } else if (typeof filter.value !== 'string' && filter.value.to) {
        where[name] = { lte: new Date(filter.value.to) };
      }
    } else if ((filter.property as Property).isEnum()) {
      where[name] = { equals: filter.value };
    } else if (filter.property.type() === 'string' && uuidRegex.test(filter.value.toString())) {
      where[name] = { equals: filter.value };
    } else if (filter.property.type() === 'reference' && (filter.property as Property).foreignColumnName()) {
      where[(filter.property as Property).foreignColumnName() as string] = convertParam(
        filter.property as Property,
        modelFields,
        filter.value,
      );
    } else if (filter.property.type() === 'mixed') {
      // JSON field handling
      const raw = filter.value as any;
      // Supported syntaxes:
      // - string: 'hasKey~a.b.c'
      // - string: 'a.b.c~equals~value'
      // - string: 'a.b.c~contains~value' | 'startsWith' | 'endsWith' | 'notEquals'
      // - object: { hasKey: 'a.b.c' }
      // - object: { path: 'a.b.c', equals: any } | { path: 'a.b.c', string_contains: 'v' } etc.
      const opSep = OPERATOR_SEPARATOR;

      const applyJsonPathFilter = (
        path: string,
        operator: 'equals' | 'not' | 'string_contains' | 'string_starts_with' | 'string_ends_with',
        value: any,
      ) => {
        const prismaPath = buildJsonPath(path, dbProvider);
        where[name] = {
          path: prismaPath,
          [operator]: value,
        } as any;
      };

      const tryParseValue = (val: string) => {
        // Try to parse JSON primitives; fallback to string
        const parsed = safeParseJSON(val);
        return parsed === null && val !== 'null' ? val : parsed;
      };

      if (typeof raw === 'string') {
        if (raw.startsWith(`hasKey${opSep}`)) {
          const path = raw.slice(`hasKey${opSep}`.length);
          const prismaPath = buildJsonPath(path, dbProvider);
          const notValue = clientModule?.Prisma?.JsonNull ?? null;
          where[name] = {
            path: prismaPath,
            not: notValue,
          } as any;
        } else if (raw.includes(opSep)) {
          const [path, op, ...rest] = raw.split(opSep);
          const valueStr = rest.join(opSep);
          const lowerOp = op.toLowerCase();
          if (lowerOp === MATCHING_PATTERNS.EQ.toLowerCase()) {
            applyJsonPathFilter(path, 'equals', tryParseValue(valueStr));
          } else if (lowerOp === MATCHING_PATTERNS.NE.toLowerCase()) {
            applyJsonPathFilter(path, 'not', tryParseValue(valueStr));
          } else if (lowerOp === MATCHING_PATTERNS.CO.toLowerCase()) {
            applyJsonPathFilter(path, 'string_contains', valueStr);
          } else if (lowerOp === MATCHING_PATTERNS.SW.toLowerCase()) {
            applyJsonPathFilter(path, 'string_starts_with', valueStr);
          } else if (lowerOp === MATCHING_PATTERNS.EW.toLowerCase()) {
            applyJsonPathFilter(path, 'string_ends_with', valueStr);
          } else {
            // Fallback: contains on full JSON string representation is not supported.
          }
        } else {
          // If plain string provided for JSON, fallback to equality against entire JSON
          where[name] = safeParseJSON(raw);
        }
      } else if (raw && typeof raw === 'object') {
        if (raw.hasKey) {
          const prismaPath = buildJsonPath(String(raw.hasKey), dbProvider);
          const notValue = clientModule?.Prisma?.JsonNull ?? null;
          where[name] = {
            ...(typeof prismaPath === 'string' ? { path: prismaPath } : { path: prismaPath }),
            not: notValue,
          } as any;
        } else if (raw.path) {
          const prismaPath = buildJsonPath(String(raw.path), dbProvider);
          const opMap: Record<string, string> = {
            equals: 'equals',
            notEquals: 'not',
            contains: 'string_contains',
            startsWith: 'string_starts_with',
            endsWith: 'string_ends_with',
          };
          const foundOp = Object.keys(opMap).find((k) => raw[k] !== undefined);
          if (foundOp) {
            const prismaOp = opMap[foundOp];
            const value = foundOp === 'equals' || foundOp === 'notEquals' ? raw[foundOp] : String(raw[foundOp]);
            where[name] = {
              ...(typeof prismaPath === 'string' ? { path: prismaPath } : { path: prismaPath }),
              [prismaOp]: value,
            } as any;
          }
        } else {
          // direct match on full JSON
          where[name] = raw;
        }
      } else {
        where[name] = raw;
      }
    } else {
      const { value } = filter
      if (typeof value === 'object') {
        if (value[MATCHING_PATTERNS.SW]) {
          where[name] = { startsWith: value[MATCHING_PATTERNS.SW].toString() };
        } else if (value[MATCHING_PATTERNS.EW]) {
          where[name] = { endsWith: value[MATCHING_PATTERNS.EW].toString() };
        } else if (value[MATCHING_PATTERNS.EQ]) {
          where[name] = { equals: value[MATCHING_PATTERNS.EQ].toString() };
        } else if (value[MATCHING_PATTERNS.NE]) {
          where[name] = { not: value[MATCHING_PATTERNS.NE].toString() };
        } else {
          const orPrefix = `${OPERATORS.OR}${OPERATOR_SEPARATOR}`;
          if (value[`${orPrefix}${MATCHING_PATTERNS.SW}`]) {
            where.OR = [
              ...(where.OR || []),
              { [name]: { startsWith: value[`${orPrefix}${MATCHING_PATTERNS.SW}`].toString() } },
            ];
          } else if (value[`${orPrefix}${MATCHING_PATTERNS.EW}`]) {
            where.OR = [
              ...(where.OR || []),
              { [name]: { endsWith: value[`${orPrefix}${MATCHING_PATTERNS.EW}`].toString() } },
            ];
          } else if (value[`${orPrefix}${MATCHING_PATTERNS.EQ}`]) {
            where.OR = [
              ...(where.OR || []),
              { [name]: { equals: value[`${orPrefix}${MATCHING_PATTERNS.EQ}`].toString() } },
            ];
          } else if (value[`${orPrefix}${MATCHING_PATTERNS.NE}`]) {
            where.OR = [
              ...(where.OR || []),
              { [name]: { not: value[`${orPrefix}${MATCHING_PATTERNS.NE}`].toString() } },
            ];
          } else if (value[OPERATORS.OR]) {
            where.OR = [...(where.OR || []), { [name]: { contains: value[OPERATORS.OR].toString() } }];
          }
        }
      } else {
        where[name] = { contains: value.toString() };
      }
    }

    return where;
  }, {});
};
