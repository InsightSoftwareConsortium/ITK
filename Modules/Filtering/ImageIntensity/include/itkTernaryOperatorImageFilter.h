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
#ifndef itkTernaryOperatorImageFilter_h
#define itkTernaryOperatorImageFilter_h

#include "itkTernaryFunctorImageFilter.h"
#include "itkLogicOpsFunctors.h"

namespace itk
{

/** \class TernaryOperatorImageFilter
 * \brief Return the value of input 2 if input 1 is false, and that of input 3 otherwise.
 *
 * \author Davis Vigneault
 *
 * This class is templated over the mask type and image type.  Inputs two and three
 * and the output image are all templated over the same type.
 * If the value of Input1 evaluates to true, the value of Input2 is taken;
 * otherwise, the value of Input3 is taken.  This is equivalent to applying
 * the ternary operator pixelwise across the three images:
 *
 * Output = Input1 ? Input2 : Input3
 *
 * As an example of where this filter could be useful, consider the case where
 * you have the following images:
 *
 * - Foreground: An image containing an object of interest.
 * - Background: An image containing a background for the object.
 * - Mask: A boolean image where pixels corresponding to the object are true.
 *
 * A new image with the foreground object placed on the background can be obtained
 * as follows:
 *
 * \code
 *
 * typedef itk::TernaryOperatorImageFilter< MaskType, ImageType > TernaryType;
 * TernaryType::Pointer ternaryFilter = TernaryType::New();
 *
 * ternaryFilter->SetInput1( maskImage );
 * ternaryFilter->SetInput2( foregroundImage );
 * ternaryFilter->SetInput3( backgroundImage );
 *
 * ImageType::Pointer objectOnBackground = ternaryFilter->GetOutput();
 *
 * \endcode
 *
 * \ingroup ITKImageIntensity
 */
template< typename TMask, typename TImage >
class TernaryOperatorImageFilter:
  public
  TernaryFunctorImageFilter< TMask, TImage, TImage, TImage,
                             Functor::TernaryOperator< typename TMask::PixelType,
                                                       typename TImage::PixelType,
                                                       typename TImage::PixelType,
                                                       typename TImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef TernaryOperatorImageFilter Self;
  typedef TernaryFunctorImageFilter<
    TMask, TImage, TImage, TImage,
    Functor::TernaryOperator< typename TMask::PixelType,
                              typename TImage::PixelType,
                              typename TImage::PixelType,
                              typename TImage::PixelType > > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(TernaryOperatorImageFilter,
               TernaryFunctorImageFilter);

protected:
  TernaryOperatorImageFilter() {}
  virtual ~TernaryOperatorImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TernaryOperatorImageFilter);
};
} // end namespace itk

#endif
