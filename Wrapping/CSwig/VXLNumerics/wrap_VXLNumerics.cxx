/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_VXLNumerics.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifdef CABLE_CONFIGURATION
#include "wrap_VXLNumerics.h"
namespace _cable_
{
  const char* const package = ITK_WRAP_PACKAGE_NAME(ITK_WRAP_PACKAGE);
  const char* const groups[] =
  {
    ITK_WRAP_GROUP(vnl_matrix),
    ITK_WRAP_GROUP(vnl_vector),
    ITK_WRAP_GROUP(vnl_c_vector),
    ITK_WRAP_GROUP(vnl_diag_matrix),
    ITK_WRAP_GROUP(vnl_file_matrix),
    ITK_WRAP_GROUP(vnl_file_vector),
    ITK_WRAP_GROUP(vnl_fortran_copy),
    ITK_WRAP_GROUP(vnl_matrix_fixed),
    ITK_WRAP_GROUP(vnl_matrix_fixed_ref),
    ITK_WRAP_GROUP(vnl_matrix_ref),
    ITK_WRAP_GROUP(vnl_vector_ref)
  };
}
#endif
