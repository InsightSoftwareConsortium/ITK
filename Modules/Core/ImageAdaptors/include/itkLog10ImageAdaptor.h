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
#ifndef itkLog10ImageAdaptor_h
#define itkLog10ImageAdaptor_h

#include "itkImageAdaptor.h"
#include "itkMath.h"

namespace itk
{
namespace Accessor
{
/** \class Log10PixelAccessor
 * \brief Give access to the std::log10() function of a value
 *
 * Log10PixelAccessor is templated over an internal type and an
 * external type representation. This class cast the input
 * applies the function to it and cast the result according
 * to the types defined as template parameters
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 */

template< typename TInternalType, typename TExternalType >
class Log10PixelAccessor
{
public:
  /** External typedef. It defines the external aspect
   * that this class will exhibit. */
  typedef TExternalType ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data. */
  typedef TInternalType InternalType;

  static inline void Set(TInternalType & output, const TExternalType & input)
  { output = (TInternalType)std::log10( (double)input ); }

  static inline TExternalType Get(const TInternalType & input)
  { return (TExternalType)std::log10( (double)input ); }
};
} // end namespace Accessor

/** \class Log10ImageAdaptor
 * \brief Presents an image as being composed of the std::log10() of its pixels
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules.
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 */
template< typename TImage, typename TOutputPixelType >
class Log10ImageAdaptor:public
  ImageAdaptor< TImage, Accessor::Log10PixelAccessor<
                  typename TImage::PixelType,
                  TOutputPixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef Log10ImageAdaptor Self;
  typedef ImageAdaptor< TImage, Accessor::Log10PixelAccessor<
                          typename TImage::PixelType,
                          TOutputPixelType > > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Log10ImageAdaptor, ImageAdaptor);

protected:
  Log10ImageAdaptor() {}
  virtual ~Log10ImageAdaptor() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Log10ImageAdaptor);
};
} // end namespace itk

#endif
