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
#ifndef itkDefaultPixelAccessorFunctor_h
#define itkDefaultPixelAccessorFunctor_h

#include "itkMacro.h"

namespace itk
{
/** \class DefaultPixelAccessorFunctor
 * \brief Provides a common API for pixel accessors for Image and VectorImage.
 *
 * This class makes the interface to DefaultVectorPixelAccessor and
 * DefaultPixelAccessor appear the same.
 *
 * The pixel accessor is set with the SetPixelAccessor method. This accessor is
 * meant to be used for Image and not for VectorImage.
 *
 * \note
 * This work is part of the National Alliance for Medical Image Computing
 * (NAMIC), funded by the National Institutes of Health through the NIH
 * Roadmap for Medical Research, Grant U54 EB005149.
 *
 * \ingroup ImageAdaptors
 *
 * \sa DefaultVectorPixelAccessor
 * \sa DefaultPixelAccessor
 * \sa DefaultVectorPixelAccessorFunctor
 * \ingroup ITKCommon
 */
template< typename TImageType >
class DefaultPixelAccessorFunctor
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
      typedef DefaultPixelAccessorFunctor<UImageType>  Type;
    };


  static void SetVectorLength(ImageType *, VectorLengthType)
  {}

  static VectorLengthType GetVectorLength(const ImageType *)
  {
    return 1;
  }

  /** Set the PixelAccessor. This is set at construction time by the image iterators.
   * The type PixelAccessorType is obtained from the ImageType over which the iterators
   * are templated.
   * */
  inline void SetPixelAccessor(PixelAccessorType & accessor)
  {
    m_PixelAccessor = accessor;
  }

  /** Set the pointer index to the start of the buffer.
   * The method exists to maintain consistency in the API of the
   * DefaultPixelAccessorFunctor and the DefaultVectorPixelAccessorFunctor. */
  inline void SetBegin( const InternalPixelType *itkNotUsed(begin) ) {}

  /** Set output using the value in input */
  inline void Set(InternalPixelType & output, const ExternalPixelType & input) const
  {
    m_PixelAccessor.Set(output, input);
  }

  /** Get the value from input */
  inline ExternalPixelType Get(InternalPixelType & input) const
  {
    return m_PixelAccessor.Get(input);
  }

  /** Get a const reference to the pixel. */
  inline const ExternalPixelType Get(const InternalPixelType & input) const
  {
    return m_PixelAccessor.Get(input);
  }

private:
  PixelAccessorType m_PixelAccessor; // The pixel accessor
};
}

#endif
