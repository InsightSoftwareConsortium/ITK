/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientImageToBloxBoundaryPointImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGradientImageToBloxBoundaryPointImageFilter_txx
#define __itkGradientImageToBloxBoundaryPointImageFilter_txx

#include "itkGradientImageToBloxBoundaryPointImageFilter.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{

template< typename TInputImage >
GradientImageToBloxBoundaryPointImageFilter< TInputImage >
::GradientImageToBloxBoundaryPointImageFilter()
{
  itkDebugMacro(<< "GradientImageToBloxBoundaryPointImageFilter::GradientImageToBloxBoundaryPointImageFilter() called");

  // The default threshold level is 128 (for no particular reason)
  m_Threshold = 128;
}

template< typename TInputImage >
void
GradientImageToBloxBoundaryPointImageFilter< TInputImage >
::GenerateInputRequestedRegion()
{
  itkDebugMacro(<< "GradientImageToBloxBoundaryPointImageFilter::GenerateInputRequestedRegion() called");
  
  Superclass::GenerateInputRequestedRegion();
}


template< typename TInputImage >
void
GradientImageToBloxBoundaryPointImageFilter< TInputImage >
::GenerateData()
{
  itkDebugMacro(<< "GradientImageToBloxBoundaryPointImageFilter::GenerateData() called");

  // Get the input and output pointers
  InputImageConstPointer  inputPtr  = this->GetInput(0);
  OutputImagePointer      outputPtr = this->GetOutput(0);

  // Allocate the output
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Position to figure out pixel location
  TPositionType inputPosition;

  // Create an iterator to walk the input image
  typedef ImageRegionConstIterator<TInputImage> TInputIterator;

  TInputIterator inputIt = TInputIterator(inputPtr,
                                          inputPtr->GetRequestedRegion() );

  // Keep track of how many boundary points we found (for debugging)
  unsigned long int numBP = 0;
  unsigned long int numBPadded = 0;

  // Get the index of the pixel
  typename TInputImage::IndexType inputIndex;
  IndexType bloxIndex;
  
  for ( inputIt.GoToBegin(); !inputIt.IsAtEnd(); ++inputIt)
    {
    // Figure out the magnitude of the gradient
    double mag = 0;

    for(int i = 0; i < NDimensions; i++)
      {
      mag += inputIt.Get()[i] * inputIt.Get()[i];
      }

    mag = sqrt(mag);

    // If the pixel meets threshold requirements, add it to the image
    if( mag >= m_Threshold)
      {
      numBP++;

      // Get the index of the boundary pixel
      inputIndex = inputIt.GetIndex();

      // Convert the index of the input pixel to the physical location of the
      // boundary point in the input image
      inputPtr->TransformIndexToPhysicalPoint(inputIndex, inputPosition);

      // Transform the physical location to a blox index
      outputPtr->TransformPhysicalPointToIndex(inputPosition, bloxIndex);

      // Create a new boundary point item and set its parameters
      BloxBoundaryPointItem<NDimensions>* pItem = new BloxBoundaryPointItem<NDimensions>;
      pItem->SetPhysicalPosition(inputPosition);
      pItem->SetGradient( inputIt.Get() );

      outputPtr->GetPixel(bloxIndex).push_back(pItem);
      numBPadded++;
      }
    }
 
  itkDebugMacro(<< "Finished looking for boundary points\n"
                << "I found " << numBP << " points\n"
                << "I added " << numBPadded << " points\n");
}

template< typename TInputImage >
void
GradientImageToBloxBoundaryPointImageFilter< TInputImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Threshold level: " << m_Threshold << std::endl;
}

} // end namespace

#endif
