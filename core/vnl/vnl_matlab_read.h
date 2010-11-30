// This is core/vnl/vnl_matlab_read.h
#ifndef vnl_matlab_read_h_
#define vnl_matlab_read_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Read from MATLAB files
// \author fsm
//
// \verbatim
//  Modifications
//   LSB (Manchester) 23 Mar 2001 documentation tidied
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
//   21 Apr 2009 Kent Williams - Taking care of the byte ordering of the MAT file
// \endverbatim

#include <vcl_iosfwd.h>
#include <vcl_complex.h>
#include <vnl/vnl_matlab_header.h>

// ------------------------------ easy ------------------------------

template <class T> class vnl_vector;
template <class T> class vnl_matrix;

//: Attempt to read vector or matrix.
// If the MATLAB header cannot be read, return false.
// Else, if a name is given, and it doesn't match what's in the file, abort().
// If the data in the file cannot reasonably be read into the destination, abort().
//
// The vector/matrix will be resized if necessary.
template <class T> bool vnl_matlab_read_or_die(vcl_istream &, vnl_vector<T> &, char const *name =0);
template <class T> bool vnl_matlab_read_or_die(vcl_istream &, vnl_matrix<T> &, char const *name =0);

// ------------------------------ less easy ------------------------------

//: MATLAB stores its data as a real block followed by an imaginary block.
// This function will read both blocks and interleave them into the area
// pointed to by ptr. For real T, it is equivalent to s.read(ptr, sizeof(T)*n);
template <class T> void vnl_matlab_read_data(vcl_istream &s, T *ptr, unsigned n);

class vnl_matlab_readhdr
{
  VCL_SAFE_BOOL_DEFINE;
 public:
  vnl_matlab_readhdr(vcl_istream &);
  ~vnl_matlab_readhdr();

  operator safe_bool () const;
  bool operator!() const;
  void read_next(); // skip to next header in file

  bool is_single() const;
  bool is_rowwise() const;
  bool is_bigendian() const; // don't use this
  long rows() const { return hdr.rows; }
  long cols() const { return hdr.cols; }
  bool is_complex() const { return hdr.imag != 0; }
  char const *name() const { return varname; }

  // bah! no member templates
  //template <class T> bool read_data(T &); // scalar
  //template <class T> bool read_data(T *); // vector
  //template <class T> bool read_data(T * const *); // 2D array
#define fsm_declare_methods(T) \
 private: \
  bool type_chck(T &); \
 public: \
  bool read_data(T &); \
  bool read_data(T *); \
  bool read_data(T * const *) // no ; here, please. SunPro 5.0 barfs.
fsm_declare_methods(float);
fsm_declare_methods(double);
fsm_declare_methods(vcl_complex<float>);
fsm_declare_methods(vcl_complex<double>);
#undef fsm_declare_methods

 private:
  vcl_istream &s;
  vnl_matlab_header hdr;
  char *varname;
  bool data_read;
  bool need_swap;
  void read_hdr(); // internal work routine
};

#endif // vnl_matlab_read_h_
