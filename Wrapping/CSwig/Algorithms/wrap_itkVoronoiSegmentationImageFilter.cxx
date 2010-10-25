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
#include "itkImage.h"
#include "itkVoronoiSegmentationImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace itktraits
{
  typedef itk::DefaultDynamicMeshTraits<double,2,2,double,float,double>
  dynamicMeshTraitDouble;
}
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkVoronoiSegmentationImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT3(PointSet,double,2,itktraits::dynamicMeshTraitDouble,itkPointSetDouble);
    ITK_WRAP_OBJECT3(Mesh,double,2,itktraits::dynamicMeshTraitDouble,itkMeshDouble);
    ITK_WRAP_OBJECT3(VoronoiSegmentationImageFilterBase,
                     image::UC2, image::UC2, image::UC2,
                     itkVoronoiSegmentationImageFilterBaseUC2UC2UC2);
    ITK_WRAP_OBJECT3(VoronoiSegmentationImageFilter, image::UC2, image::UC2, image::UC2,
                     itkVoronoiSegmentationImageFilterUC2UC2UC2);
  }
}

#endif
