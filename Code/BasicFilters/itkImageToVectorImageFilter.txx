/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToVectorImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToVectorImageFilter_txx
#define __itkImageToVectorImageFilter_txx

#include "itkImageToVectorImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"

namespace itk {

//----------------------------------------------------------------------------
template< class TInputImage >
ImageToVectorImageFilter< TInputImage >
::ImageToVectorImageFilter()
{
  // At least 1 inputs is necessary for a vector image.
  this->SetNumberOfRequiredInputs( 1 ); 
}

//----------------------------------------------------------------------------
template< class TInputImage > 
void 
ImageToVectorImageFilter< TInputImage >
::SetNthInput(unsigned int idx, const InputImageType *image)
{
  this->ProcessObject::SetNthInput(idx, const_cast< InputImageType* >(image));
  this->Modified();
}

//----------------------------------------------------------------------------
template< class TInputImage > 
void 
ImageToVectorImageFilter< TInputImage >
::AllocateOutputs()
{
  // Override the method in itkImageSource, so we can set the vector length of
  // the output itk::VectorImage

  OutputImageType * output = this->GetOutput();
  output->SetVectorLength(this->GetNumberOfInputs());
  this->Superclass::AllocateOutputs();
}

//----------------------------------------------------------------------------
template< class TInputImage > 
void 
ImageToVectorImageFilter< TInputImage >
::BeforeThreadedGenerateData()
{
  // Check to verify all inputs are specified and have the same metadata,
  // spacing etc...
  const unsigned int numberOfInputs = this->GetNumberOfInputs();
  RegionType region;
  for (unsigned int i=0; i<numberOfInputs; i++)
    {
    InputImageType *input = static_cast< InputImageType * >(
                            this->ProcessObject::GetInput(i));
    if (!input)
      {
      itkExceptionMacro(<< "Input " << i << " not set!");
      }
    if (i==0)
      {
      region = input->GetLargestPossibleRegion();
      }
    else if (input->GetLargestPossibleRegion() != region) 
      {
      itkExceptionMacro(<< "All Inputs must have the same dimensions.");
      }
    }
}

//----------------------------------------------------------------------------
template< class TInputImage > 
void
ImageToVectorImageFilter< TInputImage >
::ThreadedGenerateData(const RegionType& outputRegionForThread,
                       int ) 
{
  typename OutputImageType::Pointer outputImage = 
            static_cast< OutputImageType * >(this->ProcessObject::GetOutput(0));
  
  ImageRegionIterator< OutputImageType > oit(outputImage, outputRegionForThread);
  oit.GoToBegin();
    
  typedef ImageRegionConstIterator< InputImageType > InputIteratorType;
  std::vector< InputIteratorType * > inputItContainer;
    
  for( unsigned int i = 0; i< this->GetNumberOfInputs(); i++ )
    {
    typename InputImageType::Pointer inputImagePointer = 
        static_cast< InputImageType * >(this->ProcessObject::GetInput(i));
      
    InputIteratorType *iit = new InputIteratorType( 
                          inputImagePointer, outputRegionForThread );
    iit->GoToBegin();
    inputItContainer.push_back(iit);
    }
    
  typename OutputImageType::PixelType pix(this->GetNumberOfInputs());
  while( !oit.IsAtEnd() )
    {
    for( unsigned int i = 0; i< this->GetNumberOfInputs(); i++ )
      {
      pix[i] = inputItContainer[i]->Get();
      ++(*inputItContainer[i]);  
      }
    oit.Set(pix);
    ++oit;
    }

  for( unsigned int i = 0; i< this->GetNumberOfInputs(); i++ )
    {
    delete inputItContainer[i];
    }
}

//----------------------------------------------------------------------------
template< class TInputImage > 
void
ImageToVectorImageFilter< TInputImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk

#endif
