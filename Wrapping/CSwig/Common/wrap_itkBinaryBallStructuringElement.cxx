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
#include "itkCSwigBinaryBallStructuringElement.h"
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkBinaryBallStructuringElement);
  namespace wrappers
  {
    typedef structuringElement::F2     itkBinaryBallStructuringElementF2;
    typedef structuringElement::F3     itkBinaryBallStructuringElementF3;
    typedef structuringElement::UC2    itkBinaryBallStructuringElementUC2;
    typedef structuringElement::UC3    itkBinaryBallStructuringElementUC3;
    typedef structuringElement::US2    itkBinaryBallStructuringElementUS2;
    typedef structuringElement::US3    itkBinaryBallStructuringElementUS3;
  }
}
#endif
