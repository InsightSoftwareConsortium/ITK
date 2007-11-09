// This is core/vnl/io/vnl_io_nonlinear_minimizer.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file

#include "vnl_io_nonlinear_minimizer.h"
#include <vsl/vsl_binary_io.h>
#include <vsl/vsl_clipon_binary_loader.txx>

//: Create new object of type vnl_nonlinear_minimizer on heap
vnl_nonlinear_minimizer* vnl_io_nonlinear_minimizer::new_object() const
{
  return new vnl_nonlinear_minimizer;
}

//: Write derived class to os using vnl_nonlinear_minimizer reference
void vnl_io_nonlinear_minimizer::b_write_by_base(vsl_b_ostream& os,
                                                 const vnl_nonlinear_minimizer& base) const
{
  vsl_b_write(os,base);
}

//: Write derived class to os using vnl_nonlinear_minimizer reference
void vnl_io_nonlinear_minimizer::b_read_by_base(vsl_b_istream& is,
                                                vnl_nonlinear_minimizer& base) const
{
  vsl_b_read(is,base);
}

//: Print summary of derived class to os using vnl_nonlinear_minimizer reference
void vnl_io_nonlinear_minimizer::print_summary_by_base(vcl_ostream& os,
                                                       const vnl_nonlinear_minimizer& base) const
{
  vsl_print_summary(os,base);
}

//: Copy this object onto the heap and return a pointer
vnl_io_nonlinear_minimizer* vnl_io_nonlinear_minimizer::clone() const
{
  return new vnl_io_nonlinear_minimizer(*this);
}

//==============================================================================
//: Binary save self to stream.
void vsl_b_write(vsl_b_ostream & os, const vnl_nonlinear_minimizer & p)
{
  const short io_version_no = 1;
  vsl_b_write(os, io_version_no);
  vsl_b_write(os, p.get_f_tolerance());
  vsl_b_write(os, p.get_x_tolerance());
  vsl_b_write(os, p.get_g_tolerance());
  vsl_b_write(os, p.get_max_function_evals());
  vsl_b_write(os, p.get_epsilon_function());
  vsl_b_write(os, p.get_trace());
  vsl_b_write(os, p.get_verbose());
  vsl_b_write(os, p.get_check_derivatives());
}

//==============================================================================
//: Binary load self from stream.
void vsl_b_read(vsl_b_istream &is, vnl_nonlinear_minimizer & p)
{
  if (!is) return;

  short ver;
  // Load & save variables
  double ftol;    // Termination tolerance on F (sum of squared residuals)
  double xtol;    // Termination tolerance on X (solution vector)
  double gtol;    // Termination tolerance on Grad(F)' * F = 0
  int    maxfev;  // Termination maximum number of iterations.
  double epsfcn;  // Step length for FD Jacobian
  bool trace;
  bool verbose;
  int check_derivatives;

  vsl_b_read(is, ver);
  switch (ver)
  {
   case 1:
    vsl_b_read(is, ftol);
    p.set_f_tolerance(ftol);
    vsl_b_read(is, xtol);
    p.set_x_tolerance(xtol);
    vsl_b_read(is, gtol);
    p.set_g_tolerance(gtol);
    vsl_b_read(is, maxfev);
    p.set_max_function_evals(maxfev);
    vsl_b_read(is, epsfcn);
    p.set_epsilon_function(epsfcn);
    vsl_b_read(is, trace);
    p.set_trace(trace);
    vsl_b_read(is, verbose);
    p.set_verbose(verbose);
    vsl_b_read(is, check_derivatives);
    p.set_check_derivatives(check_derivatives);
    break;

   default:
    vcl_cerr << "I/O ERROR: vsl_b_read(vsl_b_istream&, vnl_nonlinear_minimizer&)\n"
             << "           Unknown version number "<< ver << '\n';
    is.is().clear(vcl_ios::badbit); // Set an unrecoverable IO error on stream
    return;
  }
}

//==============================================================================
//: Output a human readable summary to the stream
void vsl_print_summary(vcl_ostream & os,const vnl_nonlinear_minimizer & p)
{
  os<<"Tolerance of {F, X, G}: {"<<p.get_f_tolerance() << ", "
    << p.get_x_tolerance()<<", "<<p.get_g_tolerance() << "}\n"
    <<"Max Function Evals:"<<p.get_max_function_evals()<<"    Epsilon function:"
    <<p.get_epsilon_function()<<"       Trace:"<<p.get_trace()<<'\n'
    <<"Verbose:"<<p.get_verbose()<<"     Check Derivatives:"
    <<p.get_check_derivatives()<<'\n';
}

//: Add example object to list of those that can be loaded
//  The vsl_binary_loader must see an example of each derived class
//  before it knows how to deal with them.
//  A clone is taken of b
void vsl_add_to_binary_loader(const vnl_io_nonlinear_minimizer& b)
{
    vsl_clipon_binary_loader<vnl_nonlinear_minimizer,
                             vnl_io_nonlinear_minimizer>::instance().add(b);
}


//: Binary save to stream by vnl_nonlinear_minimizer pointer
void vsl_b_write(vsl_b_ostream &os, const vnl_nonlinear_minimizer * b)
{
    vsl_clipon_binary_loader<vnl_nonlinear_minimizer,
                             vnl_io_nonlinear_minimizer>::instance().write_object(os,b);
}

//: Binary read from stream by vnl_nonlinear_minimizer pointer
void vsl_b_read(vsl_b_istream &is, vnl_nonlinear_minimizer* &b)
{
    vsl_clipon_binary_loader<vnl_nonlinear_minimizer,
                             vnl_io_nonlinear_minimizer>::instance().read_object(is,b);
}

//: Print summary to stream by vnl_nonlinear_minimizer pointer
void vsl_print_summary(vcl_ostream &os, const vnl_nonlinear_minimizer * b)
{
    vsl_clipon_binary_loader<vnl_nonlinear_minimizer,
                             vnl_io_nonlinear_minimizer>::instance().print_object_summary(os,b);
}

// Explicitly instantiate loader
VSL_CLIPON_BINARY_LOADER_INSTANTIATE(vnl_nonlinear_minimizer, \
                                     vnl_io_nonlinear_minimizer);
