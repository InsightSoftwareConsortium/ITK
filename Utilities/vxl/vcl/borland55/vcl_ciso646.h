#ifndef vcl_borland55_ciso646_h_
#define vcl_borland55_ciso646_h_

// Borland C++ 5.5 does not provide iso646.h at all.
// We implement it here.  - Brad King

#define and    &&
#define and_eq &=
#define bitand &
#define bitor  |
#define compl  ~
#define not    !
#define not_eq !=
#define or     ||
#define or_eq  |=
#define xor    ^
#define xor_eq ^=

#endif // vcl_borland55_ciso646_h_
