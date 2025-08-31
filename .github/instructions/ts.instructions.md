---
applyTo: "**/*.ts"
---

## TypeScript instructions

CRITICAL:
Never use TypeScript for application logic! Always prefer Elm code over TypeScript.

Whenever you have to use TypeScript, follow these guidelines:

- prefer Elm over TypeScript!
- never modify `tsconfig.json`!
- favor functional programming style
- prefer Sum types over Product types
- write short, concise and single value returning functions
- use `ts-pattern` to pattern match on messages and handle errors
- avoid using booleans in records for anything other than boolean logic
- always use absolute paths using `@/` prefix
