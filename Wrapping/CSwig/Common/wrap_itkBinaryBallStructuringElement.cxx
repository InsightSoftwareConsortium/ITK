/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkBinaryBallStructuringElement.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkBinaryBallStructuringElement.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkBinaryBallStructuringElement);
  namespace wrappers
  {
    typedef itk::BinaryBallStructuringElement<float, 2 >::Self itkBinaryBallStructuringElementF2;
    typedef itk::BinaryBallStructuringElement<float, 3 >::Self itkBinaryBallStructuringElementF3;
    typedef itk::BinaryBallStructuringElement<unsigned char, 2 >::Self itkBinaryBallStructuringElementUC2;
    typedef itk::BinaryBallStructuringElement<unsigned char, 3 >::Self itkBinaryBallStructuringElementUC3;
  }
}
#endif
