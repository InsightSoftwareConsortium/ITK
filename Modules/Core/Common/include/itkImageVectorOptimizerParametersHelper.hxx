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

#ifndef itkImageVectorOptimizerParametersHelper_hxx
#define itkImageVectorOptimizerParametersHelper_hxx

#include "itkImageVectorOptimizerParametersHelper.h"

namespace itk
{
/** Default contstructor */
template< typename TValue,
          unsigned int NVectorDimension,
          unsigned int VImageDimension >
ImageVectorOptimizerParametersHelper< TValue, NVectorDimension, VImageDimension >
::ImageVectorOptimizerParametersHelper()
{
  m_ParameterImage = ITK_NULLPTR;
}

/** Move the data pointer */
template< typename TValue,
          unsigned int NVectorDimension,
          unsigned int VImageDimension >
void
ImageVectorOptimizerParametersHelper< TValue, NVectorDimension, VImageDimension >
::MoveDataPointer( CommonContainerType* container, TValue * pointer )
{
  if( m_ParameterImage.IsNull() )
    {
    itkGenericExceptionMacro("ImageVectorOptimizerParametersHelper::"
      "MoveDataPointer: m_ParameterImage must be defined.");
    }
  // The buffer for Image<Vector> points to Vector type, not TValue, so
  // have to cast.
  typedef typename ParameterImageType::PixelContainer::Element vectorElement;
  vectorElement* vectorPointer = reinterpret_cast<vectorElement *>(pointer);
  // We're expecting the new memory buffer t be of same size.
  unsigned int sizeInVectors = m_ParameterImage->GetPixelContainer()->Size();
  // After this call, PixelContainer will *not* manage its memory.
  this->m_ParameterImage->GetPixelContainer()->SetImportPointer( vectorPointer,
                                                              sizeInVectors );
  Superclass::MoveDataPointer( container, pointer );
}

/** Set parameter image */
template< typename TValue,
          unsigned int NVectorDimension,
          unsigned int VImageDimension >
void
ImageVectorOptimizerParametersHelper< TValue, NVectorDimension, VImageDimension >
::SetParametersObject(CommonContainerType * container, LightObject * object)
{
  if( object == ITK_NULLPTR )
    {
    m_ParameterImage = ITK_NULLPTR;
    return;
    }
  else
    {
    ParameterImageType* image =
      dynamic_cast<ParameterImageType *>( object );
    if( image == ITK_NULLPTR )
      {
      itkGenericExceptionMacro(
        "ImageVectorOptimizerParametersHelper::SetParametersObject: object is "
        "not of proper image type. Expected VectorImage, received "
        << object->GetNameOfClass() )
      }
    m_ParameterImage = image;
    //The PixelContainer for Image<Vector> points to type Vector, so we have
    // to determine the number of raw elements of type TValue in the buffer
    // and cast a pointer to it for assignment to the Array data pointer.
    typename CommonContainerType::SizeValueType sz = image->GetPixelContainer()->Size() * NVectorDimension;
    TValue* valuePointer = reinterpret_cast<TValue *>
                              ( image->GetPixelContainer()->GetBufferPointer() );
    //Set the Array's pointer to the image data buffer. By default it will
    // not manage the memory.
    container->SetData( valuePointer, sz, false );
    }
}

}//namespace itk
#endif
