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
#ifndef __itkImageVectorTransformParametersHelper_h
#define __itkImageVectorTransformParametersHelper_h

#include "itkTransformParametersHelper.h"
#include "itkImage.h"

namespace itk
{
/** \class ImageVectorTransformParametersHelper
 *  \brief Class to hold and manage parameters of type
 *          Image<Vector<...>,...>, used in Transforms.
 *
 *  \sa TransformParametersHelper
 *  \ingroup ITKTransform
 */

/* Can we template of Image type instead, but require that Image be of type
 * Image< Vector< TValueType, NVectorDimension >, VImageDimension > ? */
template< typename TValueType,
          unsigned int NVectorDimension,
          unsigned int VImageDimension >
class ImageVectorTransformParametersHelper
  : public TransformParametersHelper< TValueType >
{
public:

  /** The element type stored at each location in the Array. */
  typedef TValueType                                ValueType;
  typedef ImageVectorTransformParametersHelper      Self;
  typedef TransformParametersHelper< TValueType >   Superclass;

  /** Image type that this class expects. */
  typedef Image< Vector<TValueType, NVectorDimension>,
                 VImageDimension >
                                                ParameterImageType;
  typedef typename ParameterImageType::Pointer  ParameterImagePointer;

  /** Type of the common data object used in TransformParameters */
  typedef typename Superclass::CommonContainerType CommonContainerType;

  /** Default constructor. */
  ImageVectorTransformParametersHelper();

  /** Set a new data pointer for *both* the Array and parameter image,
   * pointing both to a different memory block.
   * The size of the new memroy block must be the same as current size of
   * Array and the parameter image's buffer, in elements of TValueType.
   * Memory must be managed by caller afterwards. */
  virtual void MoveDataPointer(CommonContainerType* container,
                               TValueType * pointer );

  /** Set an image that holds the parameter data. \c container is a pointer
   * of type itkArray to the object to which this helper is assigned.
   *\c container will be pointed to the image data buffer, and set not to
   * manage memory, so the image still manages its memory.
   * A dynamic cast is performed on \c object to make sure its of proper type.
   * Generally this will be called from
   * TransformParameters::SetParameterObject. */
  virtual void SetParametersObject(CommonContainerType * container,
                                   LightObject * );

  ~ImageVectorTransformParametersHelper(){}

private:
  /** The parameter image used by the class */
  ParameterImagePointer           m_ParameterImage;

};

}//namespace itk

#if ITK_TEMPLATE_TXX
#include "itkImageVectorTransformParametersHelper.hxx"
#endif

#endif
