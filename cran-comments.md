## R CMD check results
0 errors | 0 warnings | 0 notes

## Test environments
* Windows 11, R 4.5.1 (local)
* win-builder R-release (R 4.5.3)
* win-builder R-devel

## win-builder notes addressed
* Unicode characters (U+2265, U+2013, U+2014, U+00D7) replaced with ASCII equivalents
* 404 GitHub URLs removed pending public repository launch
* Acronyms (NDI, CFI, VCI, RMI) expanded on first use in DESCRIPTION
* magrittr dependency removed; reverted to utils::globalVariables() for .data

## Downstream dependencies
None.
