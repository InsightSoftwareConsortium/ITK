/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkBrains2HeaderFactory.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkBrains2HeaderFactory.h"
#include "itkBrains2IPLHeaderInfo.h"
#include "itkBrains2MaskHeaderInfo.h"
//#include "itkROIHeaderInfo.h"
//#include "itkBrains2TalairachParametersHeaderInfo.h"

namespace itk
{
Brains2HeaderBase::Pointer
Brains2HeaderFactory::CreateBrains2HeaderReader(const std::string & TypeID)
{
  if( TypeID == Brains2IPLHeaderInfo().GetHeaderBeginTag() )
    {
    return new Brains2IPLHeaderInfo;
    }
  else if( TypeID == Brains2MaskHeaderInfo().GetHeaderBeginTag() )
    {
    return new Brains2MaskHeaderInfo;
    }
#if 0 //_itkROIHeaderInfo_h
  else if( TypeID == ROIHeaderInfo().GetHeaderBeginTag() )
    {
    return new ROIHeaderInfo;
    }
#endif
#if 0
  else if( TypeID == Brains2TalairachParametersHeaderInfo().GetHeaderBeginTag() )
    {
    return new Brains2TalairachParametersHeaderInfo;
    }
  assert(0==1);
#endif
  return NULL;
}

Brains2HeaderBase::Pointer
Brains2HeaderFactory::CreateBrains2HeaderWriter(const std::string & TypeID)
{
  return Brains2HeaderFactory::CreateBrains2HeaderReader(TypeID);
}

Brains2HeaderFactory::Brains2HeaderFactory()
{
  //Do nothing
}
Brains2HeaderFactory::~Brains2HeaderFactory()
{
  //Do nothing
}
} // end namespace itk
