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
#ifndef itkDefaultVectorPixelAccessorFunctor_h
#define itkDefaultVectorPixelAccessorFunctor_h

#include "itkMacro.h"

namespace itk
{
/** \class DefaultVectorPixelAccessorFunctor
 * \brief This class provides a common API for pixel accessors for Image and
 * VectorImage. (between the DefaultVectorPixelAccessor and DefaultPixelAccessor).
 *
 * The pixel accessor is set with the SetPixelAccessor method. This accessor is
 * meant to be used only for VectorImage and not for Image. Prior to use, the
 * start of the VectorImage buffer must also be set with the SetBegin method.
 *
 * \sa DefaultVectorPixelAccessor
 * \sa DefaultPixelAccessor
 * \sa DefaultPixelAccessorFunctor
 *
 * \note
 * This work is part of the National Alliance for Medical Image Computing
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKCommon
 */
template< typename TImageType >
class DefaultVectorPixelAccessorFunctor
{
public:

  typedef TImageType                            ImageType;
  typedef typename ImageType::InternalPixelType InternalPixelType;
  typedef typename ImageType::PixelType         ExternalPixelType;
  typedef typename ImageType::AccessorType      PixelAccessorType;
  typedef unsigned int                          VectorLengthType;


  /**
   * example usage:
   *  todo
   *
   */
  template <typename UImageType>
  struct Rebind
    {
      typedef DefaultVectorPixelAccessorFunctor<UImageType>  Type;
    };


  static void SetVectorLength(ImageType *image, VectorLengthType length)
  {
    image->SetVectorLength(length);
  }

  static VectorLengthType GetVectorLength(const ImageType *image)
  {
    return image->GetVectorLength();
  }


  DefaultVectorPixelAccessorFunctor () : m_Begin(ITK_NULLPTR) {}

  /** Set the PixelAccessor. This is set at construction time by the image iterators.
   * The type PixelAccessorType is obtained from the ImageType over which the iterators
   * are templated.
   * */
  inline void SetPixelAccessor(PixelAccessorType & accessor)
  {
    m_PixelAccessor = accessor;
  }

  /** Set the pointer index to the start of the buffer. */
  inline void SetBegin(const InternalPixelType *begin)
  { this->m_Begin = const_cast< InternalPixelType * >( begin ); }

  /** Set output using the value in input */
  inline void Set(InternalPixelType & output, const ExternalPixelType & input) const
  {
    m_PixelAccessor.Set(output, input, ( &output ) - m_Begin);
  }

  /** Get the value from input */
  inline ExternalPixelType Get(const InternalPixelType & input) const
  {
    return m_PixelAccessor.Get(input, &input  - m_Begin);
  }

private:
  PixelAccessorType  m_PixelAccessor;    // The pixel accessor
  InternalPixelType *m_Begin;            // Begin of the buffer
};
}

#endif
