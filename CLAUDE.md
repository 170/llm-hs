# Instructions for Claude

## Code Quality Requirements

When making any code changes, you MUST perform the following steps:

### 1. Build Verification
After modifying code, always run the build command and ensure there are no errors or warnings:
```bash
stack build
```
- Fix ALL compilation errors
- Fix ALL compilation warnings
- Do not proceed until the build is completely clean

### 2. HLint Check
After the build passes cleanly, run hlint and address all suggestions:
```bash
hlint .
```
- Fix ALL hlint warnings and suggestions
- Apply recommended refactorings
- Ensure hlint reports no issues

### 3. Verification
Confirm that both checks pass:
```bash
stack build && hlint .
```

### 4. Documentation Update
After making code changes, you MUST check if README.md needs to be updated:
- Review what functionality was changed or added
- Determine if README.md should be updated to reflect these changes
- If README.md needs updating (new features, changed behavior, API changes, etc.), update it accordingly
- Ensure documentation remains accurate and up-to-date with the code

## Important Notes
- These steps are MANDATORY for every code change
- Do not consider a task complete until both build and hlint are clean
- Always run these checks before committing code
- Always ensure README.md is synchronized with code changes
