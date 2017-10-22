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
#ifndef itkVanHerkGilWermanErodeImageFilter_h
#define itkVanHerkGilWermanErodeImageFilter_h

// Intentionally include the Dilate filter to get the base class to avoid issues
// with itkFlatStructuringElement.hxx
#include "itkVanHerkGilWermanDilateImageFilter.h"

namespace itk
{
template< typename TPixel >
class MinFunctor
{
public:
  MinFunctor(){}
  ~MinFunctor(){}
  inline TPixel operator()(const TPixel & A, const TPixel & B) const
  {
    return std::min(A, B);
  }
};

template< typename TImage, typename TKernel >
class VanHerkGilWermanErodeImageFilter:
  public VanHerkGilWermanErodeDilateImageFilter< TImage, TKernel, MinFunctor< typename TImage::PixelType > >

{
public:
  typedef VanHerkGilWermanErodeImageFilter Self;
  typedef VanHerkGilWermanErodeDilateImageFilter< TImage, TKernel,
                                                  MinFunctor< typename TImage::PixelType > > Superclass;

  /** Runtime information support. */
  itkTypeMacro(VanHerkGilWermanErodeImageFilter,
               VanHerkGilWermanErodeDilateImageFilter);

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef typename TImage::PixelType PixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

protected:

  VanHerkGilWermanErodeImageFilter()
  {
    this->m_Boundary = NumericTraits< PixelType >::max();
  }
  virtual ~VanHerkGilWermanErodeImageFilter() ITK_OVERRIDE {}

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(VanHerkGilWermanErodeImageFilter);
};
} // namespace itk

#endif
