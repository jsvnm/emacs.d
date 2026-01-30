# Emacs Refresh Plan

Date: 2026-01-30

## Goals
- Get a clean, modern, maintainable Emacs config that still feels like your old setup.
- Replace abandoned packages with supported, minimal alternatives.
- Move to a reproducible package manager and staged migration path.
- Keep Emacs startup stable while you ramp back up.

## High-level approach
We will migrate in small, reversible stages. Each stage produces a working Emacs.
We will keep your old config intact until the new one can stand on its own.

## Package manager decision
We will use Elpaca and integrate it with use-package for readable config.

References:
- https://github.com/progfolio/elpaca
- https://www.gnu.org/software/emacs/manual/html_mono/use-package.html

## Stage 0: Snapshot + guardrails
- Freeze the current config (no edits) and add a new entry point for the refreshed config.
- Add a safety switch so you can choose old vs new at startup.
- Keep `custom.el` separate and loaded late.

Deliverables:
- `init.el` updated to support a new `init-modern.el` (or similar).
- `early-init.el` added if missing.

## Stage 1: Bootstrap Elpaca
- Add Elpaca bootstrap code at the very top of the new init.
- Disable `package.el` auto-init so it does not conflict.
- Configure `use-package` defaults and ensure Elpaca handles installs.

Why:
- Elpaca replaces `el-get` and installs from source asynchronously.

References:
- https://github.com/progfolio/elpaca
- https://www.gnu.org/software/emacs/manual/html_mono/use-package.html

## Stage 2: Completion & navigation stack (replace Anything/Ido/Smex)
Replace the old `anything` + `ido` + `smex` stack with:
- Vertico (minibuffer completion UI)
- Orderless (fuzzy-ish matching style)
- Marginalia (annotations)
- Consult (commands like consult-line, consult-buffer)
- Embark (actions on candidates)

This is the modern, modular completion system that maps best to your old workflow.

References:
- https://github.com/minad/vertico
- https://github.com/oantolin/orderless
- https://elpa.gnu.org/packages/marginalia.html
- https://github.com/minad/consult
- https://github.com/oantolin/embark

## Stage 3: In-buffer completion (replace auto-complete)
Replace `auto-complete` with:
- Corfu (completion popup in buffers)
- Optional: Cape for extra completion-at-point backends

References:
- https://github.com/minad/corfu

## Stage 4: Org mode cleanup
- Use built-in Org (no external org-mode package).
- Port your org settings directly, removing obsolete forms like `org-install`.
- Re-validate your agenda and capture templates.

References:
- https://orgmode.org/manual/

## Stage 5: Language tooling (defer until core is stable)
- Keep this minimal at first. Modern Emacs typically uses:
  - `eglot` (built-in LSP client)
  - `flymake` (built-in diagnostics)
- We will only add language tooling once the core UI is stable.

References:
- https://www.gnu.org/software/emacs/manual/html_mono/eglot.html

## Stage 6: Prune old code
Remove or archive:
- `el-get` setup
- `anything`, `auto-complete`, `smex`
- old `cedet` autoloads (only keep if you actively use it)
- deprecated advice (`defadvice`, `flet`) and obsolete functions

## Replacement map (old -> new)
- el-get -> elpaca
- anything -> consult + vertico
- ido -> vertico + orderless
- smex -> built-in M-x + vertico + marginalia
- auto-complete -> corfu (+cape optional)
- org-mode from el-get -> built-in org

## Quick summary of changes
- New package manager (Elpaca) + use-package.
- Modern completion UX (Vertico/Consult/Orderless/Marginalia/Embark).
- Modern in-buffer completion (Corfu).
- Org config cleaned up and kept on built-in Org.
- Old/abandoned packages retired.

## Open questions
- Do you want to keep the black/white theme and minimal UI, or refresh visuals too?
- Do you rely on CEDET or any of the old language-specific packages?
- Should we keep your hyper-key bindings exactly, or modernize them?

