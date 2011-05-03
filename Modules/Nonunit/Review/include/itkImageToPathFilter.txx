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
#ifndef __itkImageToPathFilter_txx
#define __itkImageToPathFilter_txx

#include "itkImageToPathFilter.h"

/*
 * This code was contributed in the Insight Journal paper:
 * "ContourExtractor2DImageFilter: A subpixel-precision image isocontour extraction filter."
 * by Pincus Z.
 * http://hdl.handle.net/1926/165
 * http://www.insight-journal.org/browse/publication/72
 *
 */

namespace itk
{
/**
 *
 */
template< class TInputImage, class TOutputPath >
ImageToPathFilter< TInputImage, TOutputPath >
::ImageToPathFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);
}

/**
 *
 */
template< class TInputImage, class TOutputPath >
ImageToPathFilter< TInputImage, TOutputPath >
::~ImageToPathFilter()
{}

/**
 *
 */
template< class TInputImage, class TOutputPath >
void
ImageToPathFilter< TInputImage, TOutputPath >
::SetInput(const InputImageType *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< InputImageType * >( input ) );
}

/**
 * Connect one of the operands for pixel-wise addition
 */
template< class TInputImage, class TOutputPath >
void
ImageToPathFilter< TInputImage, TOutputPath >
::SetInput(unsigned int index, const TInputImage *image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( index,
                                    const_cast< TInputImage * >( image ) );
}

/**
 *
 */
template< class TInputImage, class TOutputPath >
const typename ImageToPathFilter< TInputImage, TOutputPath >::InputImageType *
ImageToPathFilter< TInputImage, TOutputPath >
::GetInput(void)
{
  if ( this->GetNumberOfInputs() < 1 )
    {
    return 0;
    }

  return static_cast< const TInputImage * >
         ( this->ProcessObject::GetInput(0) );
}

/**
 *
 */
template< class TInputImage, class TOutputPath >
const typename ImageToPathFilter< TInputImage, TOutputPath >::InputImageType *
ImageToPathFilter< TInputImage, TOutputPath >
::GetInput(unsigned int idx)
{
  return static_cast< const TInputImage * >
         ( this->ProcessObject::GetInput(idx) );
}

template< class TInputImage, class TOutputPath >
void
ImageToPathFilter< TInputImage, TOutputPath >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
