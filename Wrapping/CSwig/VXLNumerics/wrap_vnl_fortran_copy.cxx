/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_vnl_fortran_copy.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "vnl/vnl_fortran_copy.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_VXLNumerics.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(vnl_fortran_copy);
  typedef vcl_complex<double> double_complex;
  typedef vcl_complex<float> float_complex;
  namespace wrappers
  {
    typedef vnl_fortran_copy<double>         vnl_fortran_copy_double;
    typedef vnl_fortran_copy<double_complex> vnl_fortran_copy_double_complex;
    typedef vnl_fortran_copy<float>          vnl_fortran_copy_float;
    typedef vnl_fortran_copy<float_complex>  vnl_fortran_copy_float_complex;
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers;
  sizeof(vnl_fortran_copy_double);
  sizeof(vnl_fortran_copy_double_complex);
  sizeof(vnl_fortran_copy_float);
  sizeof(vnl_fortran_copy_float_complex);
}

#endif
