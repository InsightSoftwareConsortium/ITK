/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkImageVectorOptimizerParametersHelper_h
#define itkImageVectorOptimizerParametersHelper_h

#include "itkOptimizerParametersHelper.h"
#include "itkImage.h"

namespace itk
{
/** \class ImageVectorOptimizerParametersHelper
 *  \brief Class to hold and manage parameters of type
 *          Image<Vector<...>,...>, used in Transforms, etc.
 *
 *  \sa OptimizerParametersHelper
 *  \ingroup ITKCommon
 */

/* Can we template of Image type instead, but require that Image be of type
 * Image< Vector< TValue, NVectorDimension >, VImageDimension > ? */
template <typename TValue, unsigned int NVectorDimension, unsigned int VImageDimension>
class ITK_TEMPLATE_EXPORT ImageVectorOptimizerParametersHelper : public OptimizerParametersHelper<TValue>
{
public:
  /** The element type stored at each location in the Array. */
  using ValueType = TValue;
  using Self = ImageVectorOptimizerParametersHelper;
  using Superclass = OptimizerParametersHelper<TValue>;

  /** Image type that this class expects. */
  using ParameterImageType = Image<Vector<TValue, NVectorDimension>, VImageDimension>;
  using ParameterImagePointer = typename ParameterImageType::Pointer;

  /** Type of the common data object used in OptimizerParameters */
  using CommonContainerType = typename Superclass::CommonContainerType;

  /** Default constructor. */
  ImageVectorOptimizerParametersHelper();

  /** Set a new data pointer for *both* the Array and parameter image,
   * pointing both to a different memory block.
   * The size of the new memroy block must be the same as current size of
   * Array and the parameter image's buffer, in elements of TValue.
   * Memory must be managed by caller afterwards. */
  void
  MoveDataPointer(CommonContainerType * container, TValue * pointer) override;

  /** Set an image that holds the parameter data. \c container is a pointer
   * of type itkArray to the object to which this helper is assigned.
   *\c container will be pointed to the image data buffer, and set not to
   * manage memory, so the image still manages its memory.
   * A dynamic cast is performed on \c object to make sure its of proper type.
   * Generally this will be called from
   * OptimizerParameters::SetParameterObject. */
  void
  SetParametersObject(CommonContainerType * container, LightObject *) override;

  ~ImageVectorOptimizerParametersHelper() override = default;

private:
  /** The parameter image used by the class */
  ParameterImagePointer m_ParameterImage;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageVectorOptimizerParametersHelper.hxx"
#endif

#endif
