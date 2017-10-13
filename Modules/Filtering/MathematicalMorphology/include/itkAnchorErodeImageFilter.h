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
#ifndef itkAnchorErodeImageFilter_h
#define itkAnchorErodeImageFilter_h

#include "itkAnchorErodeDilateImageFilter.h"

namespace itk
{
template< typename TImage, typename TKernel >
class AnchorErodeImageFilter:
  public AnchorErodeDilateImageFilter< TImage, TKernel, std::less< typename TImage::PixelType > >

{
public:
  typedef AnchorErodeImageFilter Self;
  typedef AnchorErodeDilateImageFilter< TImage, TKernel, std::less< typename TImage::PixelType > >
                                 Superclass;

  /** Runtime information support. */
  itkTypeMacro(AnchorErodeImageFilter,
               AnchorErodeDilateImageFilter);

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename TImage::PixelType PixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  protected:

  AnchorErodeImageFilter()
  {
    this->m_Boundary = NumericTraits< PixelType >::max();
  }
  virtual ~AnchorErodeImageFilter() ITK_OVERRIDE {}

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(AnchorErodeImageFilter);
};
} // namespace itk

#endif
