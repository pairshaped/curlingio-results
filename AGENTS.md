# Repository Guidelines

## Project Structure & Module Organization
- `src/Results` — main widget (entry: `Results.elm`); domain modules live here.
- `src/Results/Reports` — reporting views (e.g., `ScoringAnalysis.elm`, `View.elm`).
- `src/Shared` — shared UI/theme/translation helpers.
- `src/CurrentGames.elm` — alternate entry for the Current Games widget.
- Top‑level examples: `dev.html`, `example.html`, `current-games-dev.html`.
- Build artifacts in repo root: `dev.js`, `prod.js`, `prod.min.js`, `current-games-prod*.js`.
- Build script: `prod.sh`.

## Build, Test, and Development Commands
- Install deps: `npm install`
- Run Results dev server: `npm start`
  - Serves `src/Results.elm`, opens `dev.html`, outputs `dev.js` with Elm debugger.
- Run Current Games dev server: `npm run current`
  - Serves `src/CurrentGames.elm`, opens `current-games-dev.html`, outputs `current-games-dev.js`.
- Production build (both widgets): `./prod.sh`
  - Compiles with `elm make --optimize` and minifies with `uglifyjs` to `prod.min.js` and `current-games-prod.min.js`.

## Coding Style & Naming Conventions
- Elm idioms: one public module per file; expose only what’s needed.
- Naming: functions/values `lowerCamelCase`; types/constructors/modules `UpperCamelCase`.
- Indentation: 4 spaces; align pipelines and record fields for readability.
- Imports: group Elm packages before local modules; avoid exposing `(..)`.
- Place new report views under `src/Results/Reports/`; shared utilities under `src/Shared/`.

### Formatting
- Use `elm-format` on every save. Configure your editor to run it automatically.
- Do not rely on pre-commit formatting; code should already be formatted before staging.
- CLI example: `elm-format --yes src` (handy for bulk normalizing).

### Linting & Review
- Run `elm-review` before committing to catch unused code and API issues.
- Command: `npx elm-review` (uses the repo’s default configuration automatically).
- Examine the output and address findings deliberately. Only apply `--fix` after reviewing changes; do not auto‑apply blindly.

## Testing Guidelines
- No automated test harness is configured. Validate changes by:
  - Running the dev servers and exercising flows in `dev.html` / `current-games-dev.html`.
  - Using real subdomains in flags where possible; verify navigation via URL hash.
- If adding tests, prefer `elm-test` and keep test files under `tests/` mirroring module paths.

## Commit & Pull Request Guidelines
- Commits: short, imperative, and scoped (e.g., "Fix bracket view border rendering").
- Include user-visible changes in messages; avoid bundling minified-only commits unless updating distribution.
- PRs: provide a summary, affected modules, reproduction steps, and before/after screenshots for UI changes; link related issues.

## Security & Configuration Tips
- Do not hardcode API hosts; pass `host` and `subdomain` via flags as shown in examples.
- Avoid committing secrets; this widget consumes public endpoints only.
