// This is core/vnl/vnl_tag.h
#ifndef vnl_tag_h_
#define vnl_tag_h_
//:
// \file
// \author fsm
//
// These tags are used as argument to some private vnl_matrix and vnl_vector
// constructors to take advantage of the C++ return value optimization.
// \relatesalso vnl_matrix
// \relatesalso vnl_vector

struct vnl_tag_add { };
struct vnl_tag_sub { };
struct vnl_tag_mul { };
struct vnl_tag_div { };
struct vnl_tag_grab { };

#endif // vnl_tag_h_
