#ifndef vcl_emulation_rel_ops_h_
#define vcl_emulation_rel_ops_h_

template <class T>
inline bool operator!=(const T& x, const T& y) { return !(x == y); }

template <class T>
inline bool operator> (const T& x, const T& y) { return  (y <  x); }

template <class T>
inline bool operator<=(const T& x, const T& y) { return !(y <  x); }

template <class T>
inline bool operator>=(const T& x, const T& y) { return !(x <  y); }

#endif // vcl_emulation_rel_ops_h_
