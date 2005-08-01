// This is core/vnl/io/vnl_io_nonlinear_minimizer.h
#ifndef vnl_io_nonlinear_minimizer_h
#define vnl_io_nonlinear_minimizer_h
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \author dac
// \date 21-Mar-2001

#include <vsl/vsl_binary_io.h>
#include <vnl/vnl_nonlinear_minimizer.h>

//: Base for objects which provide IO
//  for classes derived from vnl_nonlinear_minimizer
class vnl_io_nonlinear_minimizer
{
 public:
  //: Constructor
  vnl_io_nonlinear_minimizer() {}

  //: Destructor
  virtual ~vnl_io_nonlinear_minimizer() {}

  //: Create new object of type vnl_nonlinear_minimizer on heap
  virtual vnl_nonlinear_minimizer* new_object() const;

  //: Write derived class to os using vnl_nonlinear_minimizer reference
  virtual void b_write_by_base(vsl_b_ostream& os,
                               const vnl_nonlinear_minimizer& base) const;

  //: Write derived class to os using vnl_nonlinear_minimizer reference
  virtual void b_read_by_base(vsl_b_istream& is,
                              vnl_nonlinear_minimizer& base) const;

  //: Print summary of derived class to os
  //  using vnl_nonlinear_minimizer reference
  virtual void print_summary_by_base(vcl_ostream& os,
                                     const vnl_nonlinear_minimizer& base) const;

  //: Copy this object onto the heap and return a pointer
  virtual vnl_io_nonlinear_minimizer* clone() const;

  //: Return name of class for which this object provides IO
  virtual vcl_string target_classname() const { return "vnl_nonlinear_minimizer"; }

  //: Return true if b is of class target_classname()
  //  Typically this will just be "return b.is_a()==target_classname()"
  //  However, third party libraries may use a different system
  virtual bool is_io_for(const vnl_nonlinear_minimizer& b) const
  { return b.is_a()==target_classname(); }
};

//: Add example object to list of those that can be loaded
//  The vsl_binary_loader must see an example of each derived class
//  before it knows how to deal with them.
//  A clone is taken of b
void vsl_add_to_binary_loader(const vnl_io_nonlinear_minimizer& b);

//: Binary save to stream by vnl_nonlinear_minimizer pointer
void vsl_b_write(vsl_b_ostream &os, const vnl_nonlinear_minimizer * b);

//: Binary read from stream by vnl_nonlinear_minimizer pointer
void vsl_b_read(vsl_b_istream &is, vnl_nonlinear_minimizer* &b);

//: Print summary to stream by vnl_nonlinear_minimizer pointer
void vsl_print_summary(vcl_ostream &os, const vnl_nonlinear_minimizer * b);

//: Binary save vnl_real_polynomial to stream.
void vsl_b_write(vsl_b_ostream &os, const vnl_nonlinear_minimizer & v);

//: Binary load vnl_real_polynomial from stream.
void vsl_b_read(vsl_b_istream &is, vnl_nonlinear_minimizer & v);

//: Print human readable summary of object to a stream
void vsl_print_summary(vcl_ostream& os,const vnl_nonlinear_minimizer & b);

#endif // vnl_io_nonlinear_minimizer_h
