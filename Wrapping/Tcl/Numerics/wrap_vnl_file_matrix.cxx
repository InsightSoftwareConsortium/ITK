/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_vnl_file_matrix.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "vnl/vnl_file_matrix.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_VXLNumerics.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(vnl_file_matrix);
  namespace wrappers
  {
    typedef vnl_file_matrix<double> vnl_file_matrix_double;
    typedef vnl_file_matrix<float>  vnl_file_matrix_float;
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers;
  sizeof(vnl_file_matrix_double);
  sizeof(vnl_file_matrix_float);
}

#endif
