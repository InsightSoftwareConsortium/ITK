#ifndef vcl_sunpro_vector_h_
#define vcl_sunpro_vector_h_
/*
  fsm@robots.ox.ac.uk
*/

#include <vector>

// this avoids the VCL_SUNPRO_ALLOCATOR_HACK

template <typename T>
struct vcl_vector_sunpro_50 : public std::vector<T, std::allocator<T> >
{
  typedef std::vector<T, std::allocator<T> > base;

  //explicit vcl_vector(Allocator const & = Allocator());

  explicit vcl_vector_sunpro_50() : base() { }

  explicit vcl_vector_sunpro_50(base::size_type n, T const &value = T()) : base(n, value) { }

  //template <typename InputIterator>
  //vcl_vector_sunpro_50(InputIterator first, InputIterator last) : base(first, last) { }
  explicit vcl_vector_sunpro_50(base::const_iterator first, base::const_iterator last) : base(first, last) { }
  
  explicit vcl_vector_sunpro_50(base const &that) : base(that) { }
};

#endif // vcl_sunpro_vector_h_
