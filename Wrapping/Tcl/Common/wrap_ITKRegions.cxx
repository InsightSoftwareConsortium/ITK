/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKRegions.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkRegion.h"
#include "itkImageRegion.h"
#include "itkMeshRegion.h"
#include "itkSize.h"
#include "itkIndex.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(ITKRegions);
  namespace wrappers
  {
    namespace itk
    {
      typedef ::itk::Region         Region;
      typedef ::itk::MeshRegion     MeshRegion;
      typedef ::itk::ImageRegion<2> ImageRegion2;
      typedef ::itk::ImageRegion<3> ImageRegion3;
      typedef ::itk::Index<2>       Index2;
      typedef ::itk::Index<3>       Index3;
      typedef ::itk::Size<2>        Size2;
      typedef ::itk::Size<3>        Size3;
    }
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers::itk;
  sizeof(Region);
  sizeof(MeshRegion);
  sizeof(ImageRegion2);
  sizeof(ImageRegion3);
  sizeof(Index2);
  sizeof(Index3);
  sizeof(Size2);
  sizeof(Size3);
}

#endif
