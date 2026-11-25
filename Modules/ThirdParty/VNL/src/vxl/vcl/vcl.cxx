// vcl is a header-only compatibility layer. This translation unit exists only
// to give the backward-compatibility "vcl" static archive a linker language and
// a public symbol, so downstream link lines that still reference -lvcl resolve.
// Compiled only when ITK_FUTURE_LEGACY_REMOVE is OFF; see vcl/CMakeLists.txt.
extern int vcl_legacy_compat_symbol;
int vcl_legacy_compat_symbol = 1;
