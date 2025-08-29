# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Start Development Server
```bash
npm start
```
Launches elm-live for the main Results widget with hot reloading on dev.html

### Start Current Games Development Server
```bash
npm run current
```
Launches elm-live for the CurrentGames widget with hot reloading on current-games-dev.html

### Production Build
```bash
./prod.sh
```
Compiles both Results.elm and CurrentGames.elm to optimized, minified production JavaScript files:
- `prod.js` → `prod.min.js` (main Results widget)
- `current-games-prod.js` → `current-games-prod.min.js` (CurrentGames widget)

### Install Dependencies
```bash
npm install
```

## Architecture Overview

This is an Elm application that creates embeddable JavaScript widgets for displaying curling league/competition data from the Curling I/O API. The project consists of two main applications:

### Main Applications
- **Results.elm**: The primary widget for displaying leagues, competitions, brackets, standings, schedules, and detailed statistics
- **CurrentGames.elm**: A focused widget for displaying current/live games

### Module Structure

#### Results Application (src/Results/)
- `Rest.elm`: API integration and data fetching
- `Types.elm`: Core data types and models
- `Update.elm`: State management and message handling
- `View.elm`: Main UI rendering
- `Helpers.elm`: Utility functions
- `CustomSvg.elm`: Custom SVG components

#### Reports System (src/Results/Reports/)
The application includes a comprehensive reporting system with modules for:
- `Attendance.elm`: Attendance tracking
- `CompetitionMatrix.elm`: Competition matrices
- `ScoringAnalysis.elm`: Detailed scoring analysis
- `ScoringAnalysisByHammer.elm`: Hammer-specific scoring
- `ScoringAndPercentages.elm`: Performance statistics
- `StatisticsByTeam.elm`: Team-based statistics
- `TeamRosters.elm`: Team roster management
- `HogLineViolation.elm`: Rule violation tracking
- `PositionalComparison.elm`: Positional analysis
- `View.elm`: Reports UI components

#### Shared Components (src/Shared/)
- `Theme.elm`: Theming and styling
- `Translation.elm`: Internationalization (English/French)

## Key Technologies
- **Elm 0.19.1**: Functional programming language for the frontend
- **elm-ui**: UI framework for layout and styling
- **RemoteData**: HTTP data management
- **elm-live**: Development server with hot reloading
- **uglifyjs**: JavaScript minification for production

## Widget Configuration
Both widgets are configured via JavaScript flags including:
- `subdomain`: Curling I/O subdomain
- `theme`: Custom color schemes
- `lang`: Language selection (en/fr)
- `section`: Display mode (leagues/competitions/products)
- `eventId`: Specific event filtering
- Various display toggles and customization options

## Development Workflow
1. Edit elm.json for dependency management
2. Modify dev.html or current-games-dev.html to test widget configurations
3. Use `npm start` or `npm run current` for development
4. Build production assets with `./prod.sh` before deployment

## Git Workflow
- **IMPORTANT**: Always run `./prod.sh` when committing changes to ensure production builds are updated