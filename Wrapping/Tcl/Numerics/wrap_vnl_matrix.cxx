/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_vnl_matrix.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "vcl_complex.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_VXLNumerics.h"
ITK_WRAP_VNL(vnl_matrix);
#endif
