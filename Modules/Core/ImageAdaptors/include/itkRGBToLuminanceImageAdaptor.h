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
#ifndef itkRGBToLuminanceImageAdaptor_h
#define itkRGBToLuminanceImageAdaptor_h

#include "itkImageAdaptor.h"
#include "itkMath.h"

namespace itk
{
namespace Accessor
{
/** \class RGBToLuminancePixelAccessor
 * \brief Give access to Luminance of a color pixel type.
 *
 * RGBToLuminancePixelAccessor is templated over an internal type and an
 * external type representation. This class cast the input applies the function
 * to it and cast the result according to the types defined as template
 * parameters. The input pixel type must support the GetLuminance() method.
 * This is the case of the RGBPixel class for example.
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 */
template< typename TInternalType, typename TExternalType >
class RGBToLuminancePixelAccessor
{
public:
  /** External typedef. It defines the external aspect
   * that this class will exhibit. */
  typedef TExternalType ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data. */
  typedef TInternalType InternalType;

  static inline void Set(TInternalType & output, const TExternalType & input)
  { output = static_cast< TInternalType >( input.GetLuminance() ); }

  static inline TExternalType Get(const TInternalType & input)
  { return static_cast< TExternalType >( input.GetLuminance() ); }
};
} // end namespace Accessor

/** \class RGBToLuminanceImageAdaptor
 * \brief Presents a color image as being composed of the Luminance of its pixels.
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules. The input color pixel type must
 * provide a GetLuminance() method.
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 */
template< typename TImage, typename TOutputPixelType >
class RGBToLuminanceImageAdaptor:public
  ImageAdaptor< TImage,
                Accessor::RGBToLuminancePixelAccessor<
                  typename TImage::PixelType,
                  TOutputPixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef RGBToLuminanceImageAdaptor Self;
  typedef ImageAdaptor< TImage, Accessor::RGBToLuminancePixelAccessor<
                          typename TImage::PixelType,
                          TOutputPixelType > >  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RGBToLuminanceImageAdaptor, ImageAdaptor);

protected:
  RGBToLuminanceImageAdaptor() {}
  virtual ~RGBToLuminanceImageAdaptor() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RGBToLuminanceImageAdaptor);
};
} // end namespace itk

#endif
