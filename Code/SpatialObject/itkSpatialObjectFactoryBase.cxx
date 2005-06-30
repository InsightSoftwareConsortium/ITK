/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSpatialObjectFactoryBase.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkSpatialObjectFactoryBase.h"
#include "itkSpatialObjectFactory.h"
#include "itkVersion.h"

#include "itkGroupSpatialObject.h"
#include "itkEllipseSpatialObject.h"
#include "itkDTITubeSpatialObject.h"

namespace itk
{
  SpatialObjectFactoryBase* SpatialObjectFactoryBase::m_Factory = 0;

SpatialObjectFactoryBase::SpatialObjectFactoryBase()
{
}

SpatialObjectFactoryBase::~SpatialObjectFactoryBase()
{
}

void SpatialObjectFactoryBase::RegisterDefaultSpatialObjects()
{
  if ( !m_Factory )
    {
    // 3D Objects
    typedef EllipseSpatialObject<3> EllipseType3D;
    typedef GroupSpatialObject<3> GroupType3D;
    typedef DTITubeSpatialObject<3> DTITubeType3D;
    SpatialObjectFactory<EllipseType3D>::RegisterSpatialObject();
    SpatialObjectFactory<GroupType3D>::RegisterSpatialObject();
    SpatialObjectFactory<DTITubeType3D>::RegisterSpatialObject();
    }
}

const char*
SpatialObjectFactoryBase::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char*
SpatialObjectFactoryBase::GetDescription() const
{
  return "SpatialObject FactoryBase";
}

} // end namespace itk
