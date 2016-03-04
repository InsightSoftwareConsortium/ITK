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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkQuaternionOrientationAdapter_h
#define itkQuaternionOrientationAdapter_h
#if !defined( ITK_LEGACY_REMOVE )
#include "itkOrientationAdapterBase.h"
#include "itkQuaternionRigidTransform.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class QuaternionOrientationAdapter
 *  \brief converts QuaternionOrientation flags to/from direction cosines
 * \deprecated
 * \ingroup ITKDeprecated
 */
namespace QuaternionOrientationAdapterClasses
{
typedef QuaternionRigidTransform< double > TransformType;
typedef TransformType::Pointer             TransformPointerType;
}
template< int VDimension >
class QuaternionOrientationAdapter:
  public OrientationAdapterBase< QuaternionOrientationAdapterClasses::TransformPointerType, VDimension >
{
public:
  /** typedef for superclass */
  typedef QuaternionOrientationAdapter Self;

  typedef OrientationAdapterBase< QuaternionOrientationAdapterClasses::TransformPointerType, VDimension > Superclass;
  typedef QuaternionRigidTransform< double >
  OrientationRootType;
  typedef QuaternionOrientationAdapterClasses::TransformPointerType
  OrientationType;

  /** The dimension of the input image must be 3. */
  itkConceptMacro( DimensionShouldBe3,
                   ( Concept::SameDimension< VDimension, 3 > ) );

  /** typedef for direction cosines */
  typedef typename Superclass::DirectionType DirectionType;

  /** convert from direction cosines. */
  virtual OrientationType FromDirectionCosines(const DirectionType & Dir)
  {
    OrientationType q = OrientationRootType::New();

    q->SetMatrix(Dir);
    return q;
  }

  /** convert to direction cosines. */
  virtual DirectionType ToDirectionCosines(const OrientationType & Or)
  {
    return Or->GetMatrix();
  }
};
} // namespace itk

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif // itkQuaternionOrientationAdapter_h
