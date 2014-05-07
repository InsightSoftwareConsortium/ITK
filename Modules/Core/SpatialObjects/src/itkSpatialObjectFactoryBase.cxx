/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkSpatialObjectFactory.h"
#include "itkVersion.h"

#include "itkGroupSpatialObject.h"
#include "itkEllipseSpatialObject.h"
#include "itkDTITubeSpatialObject.h"

namespace itk
{
SpatialObjectFactoryBase *SpatialObjectFactoryBase:: m_Factory = ITK_NULLPTR;

SpatialObjectFactoryBase::SpatialObjectFactoryBase()
{}

SpatialObjectFactoryBase::~SpatialObjectFactoryBase()
{}

void SpatialObjectFactoryBase::RegisterDefaultSpatialObjects()
{
  if ( !m_Factory )
    {
    // 3D Objects
    typedef EllipseSpatialObject< 3 > EllipseType3D;
    typedef GroupSpatialObject< 3 >   GroupType3D;
    typedef DTITubeSpatialObject< 3 > DTITubeType3D;
    SpatialObjectFactory< EllipseType3D >::RegisterSpatialObject();
    SpatialObjectFactory< GroupType3D >::RegisterSpatialObject();
    SpatialObjectFactory< DTITubeType3D >::RegisterSpatialObject();
    }
}

const char *
SpatialObjectFactoryBase::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
SpatialObjectFactoryBase::GetDescription() const
{
  return "SpatialObject FactoryBase";
}
} // end namespace itk
