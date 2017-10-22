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
#ifndef itkVanHerkGilWermanDilateImageFilter_h
#define itkVanHerkGilWermanDilateImageFilter_h

#include "itkVanHerkGilWermanErodeDilateImageFilter.h"
#include "itkMath.h"

namespace itk
{
template< typename TPixel >
class MaxFunctor
{
public:
  MaxFunctor(){}
  ~MaxFunctor(){}
  inline TPixel operator()(const TPixel & A, const TPixel & B) const
  {
    return std::max(A, B);
  }
};

template< typename TImage, typename TKernel >
class VanHerkGilWermanDilateImageFilter:
  public VanHerkGilWermanErodeDilateImageFilter< TImage, TKernel, MaxFunctor< typename TImage::PixelType > >
{
public:
  typedef VanHerkGilWermanDilateImageFilter Self;
  typedef VanHerkGilWermanErodeDilateImageFilter< TImage, TKernel,
                                                  MaxFunctor< typename TImage::PixelType > > Superclass;

  /** Runtime information support. */
  itkTypeMacro(VanHerkGilWermanDilateImageFilter,
               VanHerkGilWermanErodeDilateImageFilter);

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef typename TImage::PixelType PixelType;


  /** Method for creation through the object factory. */
  itkNewMacro(Self);

protected:

  VanHerkGilWermanDilateImageFilter()
  {
    this->m_Boundary = NumericTraits< PixelType >::NonpositiveMin();
  }
  virtual ~VanHerkGilWermanDilateImageFilter() ITK_OVERRIDE {}

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(VanHerkGilWermanDilateImageFilter);
};
} // namespace itk

#endif
