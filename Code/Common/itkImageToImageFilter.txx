/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToImageFilter_txx
#define _itkImageToImageFilter_txx
#include "itkImageToImageFilter.h"


namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
ImageToImageFilter<TInputImage,TOutputImage>
::ImageToImageFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
ImageToImageFilter<TInputImage,TOutputImage>
::~ImageToImageFilter()
{
}
  

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ImageToImageFilter<TInputImage,TOutputImage>
::SetInput(const InputImageType *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0, 
                                   const_cast< InputImageType * >( input ) );
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage, class TOutputImage>
void
ImageToImageFilter<TInputImage,TOutputImage>
::SetInput( unsigned int index, const TInputImage * image ) 
{
  if( index+1 > this->GetNumberOfInputs() )
    {
    this->SetNumberOfRequiredInputs( index + 1 );
    }
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(index, 
                                   const_cast< TInputImage *>( image ) );
}



/**
 *
 */
template <class TInputImage, class TOutputImage>
const typename ImageToImageFilter<TInputImage,TOutputImage>::InputImageType *
ImageToImageFilter<TInputImage,TOutputImage>
::GetInput(void) 
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<const TInputImage * >
    (this->ProcessObject::GetInput(0) );
}
  
/**
 *
 */
template <class TInputImage, class TOutputImage>
const typename ImageToImageFilter<TInputImage,TOutputImage>::InputImageType *
ImageToImageFilter<TInputImage,TOutputImage>
::GetInput(unsigned int idx)
{
  return static_cast< const TInputImage * >
    (this->ProcessObject::GetInput(idx));
}



//-----------------------------------------------------------------------
//
template<class TInputImage, class TOutputImage>
void 
ImageToImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  for (unsigned int idx = 0; idx < this->GetNumberOfInputs(); ++idx)
    {
    if (this->GetInput(idx))
      {
      InputImagePointer input =
        const_cast< TInputImage * > ( this->GetInput(idx) );

      // Use the function object RegionCopier to copy the output region
      // to the input.  The default region copier has default implementations
      // to handle the cases where the input and output are the same
      // dimension, the input a higher dimension than the output, and the
      // input a lower dimension than the output.
      InputImageRegionType inputRegion;
      this->CallCopyOutputRegionToInputRegion(inputRegion, this->GetOutput()->GetRequestedRegion());
      input->SetRequestedRegion( inputRegion );
      }
    }  
}

template<class TInputImage, class TOutputImage>
void 
ImageToImageFilter<TInputImage,TOutputImage>
::CallCopyOutputRegionToInputRegion(InputImageRegionType &destRegion,
                                    const OutputImageRegionType &srcRegion)
{
  OutputToInputRegionCopierType regionCopier;
  regionCopier(destRegion, srcRegion);
}


template<class TInputImage, class TOutputImage>
void 
ImageToImageFilter<TInputImage,TOutputImage>
::CallCopyInputRegionToOutputRegion(OutputImageRegionType &destRegion,
                                    const InputImageRegionType &srcRegion)
{
  InputToOutputRegionCopierType regionCopier;
  regionCopier(destRegion, srcRegion);
}


template<class TInputImage, class TOutputImage>
void 
ImageToImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}



} // end namespace itk

#endif
