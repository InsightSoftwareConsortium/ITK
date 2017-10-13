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
#ifndef itkAnchorDilateImageFilter_h
#define itkAnchorDilateImageFilter_h

#include "itkAnchorErodeDilateImageFilter.h"
#include <functional>

namespace itk
{
template< typename TImage, typename TKernel >
class AnchorDilateImageFilter:
  public AnchorErodeDilateImageFilter< TImage, TKernel, std::greater< typename TImage::PixelType > >

{
public:
  typedef AnchorDilateImageFilter Self;
  typedef AnchorErodeDilateImageFilter< TImage, TKernel, std::less< typename TImage::PixelType > >
                                  Superclass;

  /** Runtime information support. */
  itkTypeMacro(AnchorDilateImageFilter,
               AnchorErodeDilateImageFilter);

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename TImage::PixelType PixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

protected:

  AnchorDilateImageFilter()
  {
    this->m_Boundary = NumericTraits< PixelType >::NonpositiveMin();
  }
  virtual ~AnchorDilateImageFilter() ITK_OVERRIDE {}

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(AnchorDilateImageFilter);
};
} // namespace itk

#endif
