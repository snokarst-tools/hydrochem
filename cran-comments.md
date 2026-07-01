## R CMD check results

Duration: 42.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## Test environments

* Local: macOS, R 4.4.x
* R-hub / GitHub Actions (R-devel):
  - linux (ubuntu-latest) — OK
  - macos (macos-13) — OK
  - windows (windows-latest) — OK

## Comments

This is a resubmission addressing CRAN reviewer comments from the prior submission:

* Added references describing the methods implemented in the package 
  to the Description field of DESCRIPTION, in the requested 
  `authors (year) <doi:...>` format.

* Replaced remaining \dontrun{} with \donttest{} in examples that are 
  executable but take a small amount of time to run; examples that 
  execute in under 5 seconds have been unwrapped entirely.

* Removed the default/non-tempdir file path in save_plot()'s examples; 
  all examples, vignettes, and tests now write only to tempdir(), and 
  no exported function has a default path that writes to the user's 
  home filespace.

The package was prepared following the checklist from 
https://github.com/ThinkR-open/prepare-for-cran to ensure 
compliance with CRAN policies.

## Reverse dependencies

Not applicable, this is a new package with no reverse dependencies.