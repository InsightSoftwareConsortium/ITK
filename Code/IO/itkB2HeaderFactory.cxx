/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkB2HeaderFactory.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkB2HeaderFactory.h"
#include "itkB2IPLHeaderInfo.h"
#include "itkB2MaskHeaderInfo.h"
//#include "itkROIHeaderInfo.h"
//#include "itkB2TalairachParametersHeaderInfo.h"

namespace itk
{
B2HeaderBase::Pointer
B2HeaderFactory::CreateB2HeaderReader(const std::string & TypeID)
{
  if( TypeID == B2IPLHeaderInfo().GetHeaderBeginTag() )
    {
    return new B2IPLHeaderInfo;
    }
  else if( TypeID == B2MaskHeaderInfo().GetHeaderBeginTag() )
    {
    return new B2MaskHeaderInfo;
    }
#if 0 //_itkROIHeaderInfo_h
  else if( TypeID == ROIHeaderInfo().GetHeaderBeginTag() )
    {
    return new ROIHeaderInfo;
    }
#endif
#if 0
  else if( TypeID == B2TalairachParametersHeaderInfo().GetHeaderBeginTag() )
    {
    return new B2TalairachParametersHeaderInfo;
    }
  assert(0==1);
#endif
  return NULL;
}

B2HeaderBase::Pointer
B2HeaderFactory::CreateB2HeaderWriter(const std::string & TypeID)
{
  return B2HeaderFactory::CreateB2HeaderReader(TypeID);
}

B2HeaderFactory::B2HeaderFactory()
{
  //Do nothing
}
B2HeaderFactory::~B2HeaderFactory()
{
  //Do nothing
}
} // end namespace itk
