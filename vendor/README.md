# Vendored packages

Small shims for packages that are unmaintained upstream. Each directory
holds a patched copy; provenance and reason for vendoring are noted below.

## know-your-http-well

- Upstream: https://github.com/for-GET/know-your-http-well (dead)
- Vendored from el-get checkout commit `c916e82` ("add references. fix #57")
- Only the two data files its sole consumer (`company-restclient`) uses
  are kept: `http-headers.el`, `http-methods.el`. `http-relations.el`
  and `http-status-codes.el` are dropped — unused, and the latter
  carries a GPLv2+ header (Copyright 2011 Ruslan Spivak) that
  contradicts upstream's Unlicense, so keeping it would mean GPL
  redistribution homework for a dead file.
- Reason: the `.el` files lack `lexical-binding` cookies, which Emacs 30+
  warns about on every load (the files load at startup via the
  ob-restclient → restclient → company-restclient chain). Cookies are the
  only change; the data is untouched.
- The el-get package is replaced by this copy:
  `el-get-user/recipes/company-restclient.rcp` overrides the standard
  recipe without the `know-your-http-well` dependency, so `el-get-cleanup`
  removes the old install automatically.
