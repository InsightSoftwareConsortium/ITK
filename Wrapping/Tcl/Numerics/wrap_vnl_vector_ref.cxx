/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_vnl_vector_ref.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "vnl/vnl_vector_ref.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_VXLNumerics.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(vnl_vector_ref);
  namespace wrappers
  {
    typedef vnl_vector_ref<double> vnl_vector_ref_double;
    typedef vnl_vector_ref<float>  vnl_vector_ref_float;
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers;
  sizeof(vnl_vector_ref_double);
  sizeof(vnl_vector_ref_float);
}

#endif
