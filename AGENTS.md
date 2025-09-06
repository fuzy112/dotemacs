# Emacs Configuration Development Guidelines

This document outlines the development practices and conventions for maintaining this Emacs configuration.

## Git and ChangeLog Conventions

- Each git commit must include a ChangeLog entry
- The ChangeLog file must be committed alongside the changes
- Commit messages should match the ChangeLog entry content
- ChangeLog entries should follow the GNU ChangeLog format with proper attribution
- When adding new files, mention them in both ChangeLog and commit message
- Use `git commit --amend` to update commits if ChangeLog was missed initially

## Directory Structure

- `site-lisp/` - Contains locally developed Emacs Lisp packages and utilities
- `straight/repos/` - Contains package repositories managed by straight.el
- `packages.el` - Defines all package recipes and configurations

## Package Management

- All packages are managed using straight.el
- Package recipes are defined in `packages.el`
- External package repositories are cloned to `straight/repos/`
- Prefer pinned commits for stability in package recipes

## Emacs Lisp Coding Conventions

- All symbols must use a proper prefix (typically the package name)
- Public symbols: `prefix-function-name`
- Private symbols: `prefix--function-name` (note the double dash)
- Use meaningful function and variable names
- Include proper docstrings for all public symbols
- Follow Emacs Lisp indentation and formatting standards

## File Headers

Each Emacs Lisp file should include:
- Copyright notice
- Author information
- License header
- Package version (Only necessary if it makes sense to distribute a standalone version of the package)
- Commentary section
- Code section marker
- Split logical pages with C-l (ascii code 12)

## Additional Guidelines

- Test configurations before committing
- Keep configurations modular and well-organized
- Document complex configurations with comments
- Prefer built-in Emacs functionality when possible
- Maintain backward compatibility where feasible
