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
#ifndef itkOrientationAdapterBase_h
#define itkOrientationAdapterBase_h

#if !defined(ITK_LEGACY_REMOVE)
#  include "itkImageBase.h"

namespace itk
{
/** \class OrientationAdapterBase
 *  \brief base class that converts Orientation representations to direction cosines.
 *
 * OrientationAdapterBase is a pure virtual base class that defines the
 * member function signatures for any subclass that concretely defines the
 * conversion relation between a method of representing orientation, and the
 * direction cosines managed in itk::ImageBase.
 * \ingroup ITKCommon
 */
template <typename OrientationType, unsigned int Dimension = 3>
class ITK_TEMPLATE_EXPORT [[deprecated("Since ITK 5.3 use SpatialOrientationAdapter.")]] OrientationAdapterBase
{
public:
  /** type alias for matching ImageBase */
  using ImageType = ImageBase<Dimension>;

  /** type alias for matching Direction Cosines type */
  using DirectionType = typename ImageType::DirectionType;

  /** Convert direction cosines to the Orientation type */
  virtual OrientationType
  FromDirectionCosines(const DirectionType & Dir) = 0;

  /** Convert Orientation type direction cosines */
  virtual DirectionType
  ToDirectionCosines(const OrientationType & Orient) = 0;

protected:
  /** destructor, to silence "virtual class has non-virtual destructor()"
    warnings */
  virtual ~OrientationAdapterBase() = default;
};
} // namespace itk
#else // ITK_LEGACY_REMOVE
#  error itkOrientationAdapterBase.h is a legacy file since ITK 5.3 and will be removed in the future.
#endif // ITK_LEGACY_REMOVE

#endif // itkOrientationAdapterBase_h
