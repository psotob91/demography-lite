# Use of AI tools in this repository

This file records what was produced with an AI tool and what human
verification each piece has had. It is kept from the first commit because
journals and software reviews ask for this statement.

**Summary, 2026-11-13: everything in the first commits of this repository was
written by an AI agent. No human has reviewed any of it yet.**

**Update, 14 November 2026: the repository owner has taken three decisions
(see "Decisions the AI took" below: items 1 and 3 are settled, and the width
of the per-cell seed was settled with them) and an AI agent carried them out.
The code that implements them was written by the agent and has not been
reviewed by a person either.**

## Tool

- Claude (Anthropic), model "Claude Fable 5.2", run as an autonomous coding
  agent (Claude Code) inside a development container, on 2026-11-13.
- Claude (Anthropic), model "Claude Opus 5", run the same way on 2026-11-14,
  to carry out the three decisions of the owner listed below. It took no
  design decision of its own except the ones recorded in this file under
  14 November 2026.
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
| Hash behind `cell_seed()` (FNV-1a, 32 bits, plus the final mix of MurmurHash3) | chosen and written by the AI; outputs compared by the AI with an independent implementation in another language and with the published FNV-1a test vectors; the hash itself is unchanged and still frozen in a test | the owner decided on 14 November 2026 that the seed had to live in a wider space; the hash was kept |
| Width of the random state of a cell (14 November 2026) | the AI derives six 32-bit words from the label of the cell, one per word of the state of the L'Ecuyer-CMRG generator, with one salt per word, reduces the first three modulo 4294967087 and the last three modulo 4294944443, avoids a trio of zeros and the one word that an R integer cannot hold, and writes the words into `.Random.seed`. The reduction modulo the two moduli, the handling of the degenerate states and the salts are the AI's choices; the owner asked for the generator and for the state to stay derived from the labels. Collisions measured: none on a simulated grid of 200,000 cells; the states are frozen in a test | none |
| Simulated country (`simulate_country()`) | model, parameters and code invented by the AI; nothing comes from real data | none; a demographer should judge whether the invented schedules are plausible enough for the tests they will support |
| Skeletons of the public functions (signatures, argument names, defaults) | drafted by the AI from a table of planned functions; argument names and defaults are provisional | none |
| Help pages: sections "GBD equivalent" and "Method" | written by the AI. Section and page numbers of the GBD 2023 demographics appendix were carried over from the design notes and **not re-read** by the AI in this session. Three pages say explicitly that the passage still has to be cited | none; every statement about what the GBD study does must be checked against the appendix before release |
| References and DOIs | copied from a reference list whose DOIs had been resolved during the design phase; not re-resolved here. References without DOI are cited from that list | none |
| Implemented package functions (`ddl_cap()`, `ddl_combine()`, `ddl_simulate()`) | written entirely | none |
| Public wrappers of the shared code (`<prefix>_draws_new()`, `<prefix>_draws_long()`, `<prefix>_draws_wide()`, `<prefix>_summarise()`, `<prefix>_draws_generate()`, `<prefix>_age_groups()`, `<prefix>_country()`), 14 November 2026 | the owner decided that the shared code stops being exported; the AI chose which wrappers are the smallest set that keeps each package usable on its own, their names, the single template they come from and the substitution done by `tools/sync-standalone.R` | none; a human should confirm the set and the names |
| Registration of the print methods of the shared classes as S3 methods, 14 November 2026 | written by the AI, including the step that gives the copies of each method the base environment as their enclosure so that the three packages register the same object and no overwrite is reported | none |
| Tests (`tests/testthat/`, `standalone/tests/`, `tools/tests/`) | written entirely; expected values were written from definitions or planted truth before running the code, except the frozen hashes and states and the absence of collisions on a grid (observed). Three expectations that were themselves wrong were corrected after the first run (a date fraction, a cell-by-cell ratio that only holds for areas, and fixtures lacking a column). One test passed for the wrong reason (it looked for the print methods from inside the package, where they are always visible) and hid a real defect, found only by installing the three packages and loading them together; code and test were then fixed. On 14 November 2026 that test was rewritten again to require the registered method and the method of the package to be the same object; the frozen seeds were replaced by frozen six-word states, and the absence of collisions is checked on a 5,000-cell grid in the tests and was measured once on a grid of 200,000 cells outside them, because the hash in base R takes about half a minute at that size | none |
| Controls in `tools/` | written entirely | none |
| `R CMD check` runs, test runs | executed by the AI; results reported as produced | none |

## Decisions the AI took that a human should confirm

1. **Settled by the repository owner on 14 November 2026.** `ddl_cap()` offers
   only the published rule ("completeness at or above 0.95 becomes 1") and no
   transition of its own, so that what the function does is exactly what can
   be cited. Its help page now says that the rule is a jump and how big the
   jump is (registered deaths multiplied by 1.053 at a completeness of 0.9499
   and left untouched at 0.9501), and the function warns, with a condition of
   class `<prefix>_warning_threshold`, when some input value falls in
   [0.90, 0.95), where the choice of threshold decides the result. What it
   returns did not change.
2. `ddl_combine()` returns a missing value when fewer than three estimates
   are available, instead of falling back to another rule.
3. **Settled by the repository owner on 14 November 2026.** The shared
   functions are no longer exported. Each package exports instead a thin
   wrapper of each one under its own prefix, from a single template, and the
   print methods of the shared classes are registered as S3 methods through
   `NAMESPACE`. Installing the three in one library and attaching them
   together now prints nothing, in any order, and no longer depends on which
   package is loaded first. A small base package is still the alternative if
   the copies ever have to diverge.
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
- [ ] Review of the items under "Decisions the AI took" that are still open
      (2, 4, 5, 6 and 7; 1 and 3 were settled on 14 November 2026).
- [ ] Review of the code written on 14 November 2026 for those two decisions
      and for the width of the random state of a cell.
- [ ] Confirmation that nothing from the originating study is present
      (`Rscript tools/check-clean.R --history`), including a read by a person:
      the control only knows the words in its deny-list.

Update this file whenever an AI tool contributes again or a human
verification takes place: add the date, the tool, the piece and who verified
what.
