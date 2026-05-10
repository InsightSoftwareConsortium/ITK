// vcl is now a header-only compatibility layer. This empty translation
// unit gives the static library a linker language and avoids
// "no public symbols found" warnings on platforms that dislike empty
// archives.
extern int vcl_dummy_var;
int vcl_dummy_var = 12345678;
