/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSample.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkListSample_txx
#define __itkListSample_txx

#include "itkListSample.h"

namespace itk{ 
  namespace Statistics{

template< class TMeasurement, unsigned int VMeasurementVectorSize >
ListSample< TMeasurement, VMeasurementVectorSize >
::ListSample()
{
  // all measurement vectors are sored
  SetSortedFlag(false) ;
  // supports GetFrequency method
  SetSupportingFrequencyFlag(false) ;
  // no dupliates
  SetAllowingDuplicatesFlag(true) ;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize >
void 
ListSample< TMeasurement, VMeasurementVectorSize >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}
  } // end of namespace Statistics
} // end of namespace itk 

#endif
