#include <vcl_vector.txx>

// The sunpro CC compiler does not have an operator>>(signed char) only
// char, and it tries to use vnl_matrix>> or vnl_vector>> instead??
//"vcl_vector.cc", line 164: Error: Overloading ambiguity between "operator>>(istream&, vnl_matrix<signed char>&)" and "operator>>(istream&, vnl_vector<signed char>&)".
// "vcl_vector.cc", line 173: Error: Overloading ambiguity between "operator>>(istream&, vnl_matrix<signed char>&)" and "operator>>(istream&, vnl_vector<signed char>&)".
// however on the sun signed char is char.

VCL_VECTOR_INSTANTIATE(signed char);
