#ifndef vnl_unary_function_h_
#define vnl_unary_function_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_unary_function - Abstract 1D map
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_unary_function.h
// .FILE	vnl_unary_function.cxx
//
// .SECTION Description
//    vnl_unary_function is an abstract map between two types (read spaces).
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 28 Nov 98
//
// .SECTION Modifications
//     981128 AWF Initial version.
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_numeric_limits.h>

template <class Return, class Argument>
class vnl_unary_function {
public:
  typedef vnl_numeric_limits<Return> limits;

  // -- Apply the function.  The name is "f" rather than operator(), as the
  // function will generally be called through a pointer.  Note that the
  // function is NOT const when you subclass.
  virtual Return f(Argument const& i) = 0;

  // -- Return bounding cube of domain (inputs)
  // removed by awf simply to avoid implementing vnl_numeric_limits<vnl_vector<T> >.
  //virtual Argument get_domain_min() const { return vnl_numeric_limits<Argument>::min(); }
  //virtual Argument get_domain_max() const { return vnl_numeric_limits<Argument>::max(); }

  // -- Return bounding cube of range (outputs)
  virtual Return get_range_min() const { return vnl_numeric_limits<Return >::min(); }
  virtual Return get_range_max() const { return limits::max(); }

  // -- Copy should allocate a copy of this on the heap and return it.  If
  // Subclasses do not implement this function, it will return null, but many
  // applications will never call it, so this may not be a problem for you.
  virtual vnl_unary_function<Return,Argument>* Copy() const { return 0; }

  virtual ~vnl_unary_function() {}
};

#endif // vnl_unary_function_h_
