/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
SpatialObjectFactoryBase * SpatialObjectFactoryBase::m_Factory = nullptr;

SpatialObjectFactoryBase::SpatialObjectFactoryBase() = default;

SpatialObjectFactoryBase::~SpatialObjectFactoryBase() = default;

void
SpatialObjectFactoryBase::RegisterDefaultSpatialObjects()
{
  if (!m_Factory)
  {
    // 3D Objects
    using EllipseType3D = EllipseSpatialObject<3>;
    using GroupType3D = GroupSpatialObject<3>;
    using DTITubeType3D = DTITubeSpatialObject<3>;
    SpatialObjectFactory<EllipseType3D>::RegisterSpatialObject();
    SpatialObjectFactory<GroupType3D>::RegisterSpatialObject();
    SpatialObjectFactory<DTITubeType3D>::RegisterSpatialObject();
  }
}

const char *
SpatialObjectFactoryBase::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
SpatialObjectFactoryBase::GetDescription() const
{
  return "SpatialObject FactoryBase";
}
} // end namespace itk
