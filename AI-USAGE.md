# Use of AI tools in this repository

This file records what was produced with an AI tool and what human
verification each piece has had. It is kept from the first commit because
journals and software reviews ask for this statement.

**Summary, 2026-11-13: everything in the first commits of this repository was
written by an AI agent. No human has reviewed any of it yet.**

## Tool

- Claude (Anthropic), model "Claude Fable 5.2", run as an autonomous coding
  agent (Claude Code) inside a development container, on 2026-11-13.
- The agent worked from design notes kept outside this repository. The design
  decisions recorded there (three packages, their names and scope, shared code
  as copied single-source files, MIT licence, English, per-cell seeds, the
  survey estimate of child mortality supplied by the user, age groups under
  1 year) were taken by the repository owner. The agent took no design
  decision that those notes left open without listing it below.

## What the AI did, piece by piece

| Piece | AI contribution | Human verification so far |
|---|---|---|
| Repository layout, `DESCRIPTION`, `README`, `NEWS`, licence files | written entirely | none |
| Shared code in `standalone/` (draws container, per-cell seeds, age groups, validators) | designed in detail and written entirely | none |
| Hash behind `cell_seed()` (FNV-1a, 32 bits, plus the final mix of MurmurHash3) | chosen and written by the AI; outputs compared by the AI with an independent implementation in another language and with the published FNV-1a test vectors; the resulting seeds are frozen in a test | none; a human should confirm the choice (32-bit seeds can collide between cells, as the help page says) |
| Simulated country (`simulate_country()`) | model, parameters and code invented by the AI; nothing comes from real data | none; a demographer should judge whether the invented schedules are plausible enough for the tests they will support |
| Skeletons of the public functions (signatures, argument names, defaults) | drafted by the AI from a table of planned functions; argument names and defaults are provisional | none |
| Help pages: sections "GBD equivalent" and "Method" | written by the AI. Section and page numbers of the GBD 2023 demographics appendix were carried over from the design notes and **not re-read** by the AI in this session. Three pages say explicitly that the passage still has to be cited | none; every statement about what the GBD study does must be checked against the appendix before release |
| References and DOIs | copied from a reference list whose DOIs had been resolved during the design phase; not re-resolved here. References without DOI are cited from that list | none |
| Implemented package functions (`ddl_cap()`, `ddl_combine()`, `ddl_simulate()`, `prl_simulate()`, `evl_summarise()`) | written entirely | none |
| Tests (`tests/testthat/`, `standalone/tests/`, `tools/tests/`) | written entirely; expected values were written from definitions or planted truth before running the code, except the frozen seeds (taken from the independent implementation) and the absence of seed collisions on a 5,000-cell grid (observed). Three expectations that were themselves wrong were corrected after the first run (a date fraction, a cell-by-cell ratio that only holds for areas, and fixtures lacking a column). One test passed for the wrong reason (it looked for the print methods from inside the package, where they are always visible) and hid a real defect, found only by installing the three packages and loading them together; code and test were then fixed | none |
| Controls in `tools/` | written entirely | none |
| `R CMD check` runs, test runs | executed by the AI; results reported as produced | none |

## Decisions the AI took that a human should confirm

1. `ddl_cap()` implements exactly "completeness at or above 0.95 becomes 1"
   and leaves lower values unchanged. Whether a transition just below the
   threshold belongs in the package is open.
2. `ddl_combine()` returns a missing value when fewer than three estimates
   are available, instead of falling back to another rule.
3. The shared functions are exported by each of the three packages under the
   same names. Attaching two of the packages therefore prints masking
   messages; the objects are identical. The print methods of the shared
   classes are registered by whichever package is loaded first. The
   alternative is a small base package.
4. Condition classes: `<prefix>_error_input`, `<prefix>_error_not_implemented`
   and the family-wide `demolite_error_*`.
5. Names and scales of several skeleton arguments (`age_trim`,
   `e_open_factor`, `level_error`, `age_error`, the layout of `cells`) are
   provisional.
6. `simulate_country()` uses exogenous births (no fertility rates) and does
   not yet simulate completeness that changes over time.
7. The deny-list of the clean-package control (`tools/clean-package-patterns.txt`)
   was assembled by the AI; a human should extend it with anything else that
   must never appear.

## Human verification still needed

- [ ] Code review of `standalone/` by a person who did not write it.
- [ ] Review of every "GBD equivalent" and "Method" section against the
      source it cites.
- [ ] Review of the simulated country by a demographer.
- [ ] Decision on authors, copyright holder, maintainer and public
      repository.
- [ ] Review of the items under "Decisions the AI took".
- [ ] Confirmation that nothing from the originating study is present
      (`Rscript tools/check-clean.R --history`), including a read by a person:
      the control only knows the words in its deny-list.

Update this file whenever an AI tool contributes again or a human
verification takes place: add the date, the tool, the piece and who verified
what.
