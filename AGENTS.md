# Working conventions for RDBEScore

## Project context

- Read `README.md` and any applicable project instructions before substantive work. 
- Treat project files and the current working tree as the source of truth. Inspect relevant Git changes and preserve unrelated work.
- Keep work focused on the requested issue. Do not automatically commit, tag, push, publish or prepare a release.
- Use the existing R package structure: functions in `R/`, tests in `tests/testthat/`, packaged inputs in `data/`, data preparation in `data-raw/`, user guides in `vignettes/` and additional documentation in `WGRDBES-EST/`.
- Keep agent working preferences in this file. Retain contributor-facing guidance in `README.md`.

## Code and data

- Match the surrounding R style and use camelCase for new function names. The repository style guide is `WGRDBES-EST/newbies_guide/StyleGuide_WKRDB-EST_Annex8.pdf`.
- Prefer base R for ordinary manipulation and use data.table where the project already depends on it. Do not introduce dplyr, tidyr or reshape2 without an explicit request.
- Copy data.table inputs with `data.table::copy()` before modifying them. Avoid changing caller-owned tables by reference.
- Inspect actual data and documentation before making claims about variables, sampling designs or expected results.
- Preserve source datasets. Do not silently filter, recode or replace observations to make code pass.
- Develop with public or synthetic data only. Do not search or access the user computer for data outside the current directory.

## Function examples

- Prefer concise, runnable examples that demonstrate the function's purpose using complete packaged datasets.
- In exported-function examples, identify packaged inputs explicitly, for example `H1DataObj <- H1Example` or `H5DataObj <- H5Example`.
- In internal-function examples, use the packaged data directly. Do not introduce aliases such as `H8DataObj <- H8ExampleEE1`. Necessary preparation or copying before mutation is still appropriate.
- Do not filter datasets solely to reduce runtime. Consider a smaller complete dataset, checking that it still demonstrates the intended behaviour and sampling design. Filtering examples should demonstrate filtering itself.
- Keep `strict = TRUE` as the default. Ordinary examples should use strict validation.
- Show relevant warnings and explain briefly what they mean. Do not suppress warnings or disable validation merely to make an example pass.
- Allow `\dontrun{}` around examples deliberately demonstrating errors under `strict = TRUE`. Explain the expected error and retain the strict call.
- A separate example may explicitly demonstrate `strict = FALSE` when explaining permissive behaviour. Label it clearly and leave its warning visible.

## Documentation and review

- Edit roxygen source and regenerate help files with `devtools::document()`. Do not manually edit generated `.Rd` files.
- Follow the README's AI-involvement and human-review requirements for new or substantially modified functions and tests.
- Record only genuine human approvals. Preserve existing review records and leave new approvals pending until a person has checked and accepted the work.
- Leave `Notes/scope` empty when there is no specific information to record. Avoid generic notes such as "Examples revised for #272. Implementation unchanged."
- Document concrete limitations or verification scope when useful. Do not imply that a passing software check establishes scientific correctness.

## Verification

- Run relevant examples and checks for the change. Add meaningful regression tests for correctness fixes, using independently established expectations.
- Before integrating changes into `dev`, regenerate documentation, run `devtools::test()` and run `devtools::check()` as required by the README.
- Fix problems introduced by the change and report remaining failures or pre-existing warnings accurately.
- Do not repeat completed tests without a new change, failure or unresolved concern that justifies them.
