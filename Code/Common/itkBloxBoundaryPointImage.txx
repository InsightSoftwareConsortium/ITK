/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryPointImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxBoundaryPointImage_txx
#define __itkBloxBoundaryPointImage_txx

#include <iostream>
#include "itkImageRegionIterator.h"
#include "itkBloxBoundaryPointImage.h"

namespace itk
{

template<class TSourceImage, class TImageTraits>
BloxBoundaryPointImage<TSourceImage, TImageTraits>
::BloxBoundaryPointImage()
{
  m_Threshold = 0;
}

template<class TSourceImage, class TImageTraits>
BloxBoundaryPointImage<TSourceImage, TImageTraits>
::~BloxBoundaryPointImage()
{

}

template<class TSourceImage, class TImageTraits>
void
BloxBoundaryPointImage<TSourceImage, TImageTraits>
::UpdateSourceParameters()
{
  m_SourceOrigin = m_SourceImage->GetOrigin();
  m_SourceSpacing = m_SourceImage->GetSpacing();
}

template<class TSourceImage, class TImageTraits>
void
BloxBoundaryPointImage<TSourceImage, TImageTraits>
::FindBoundaryPoints()
{
  itkDebugMacro(<< "BloxBoundaryPointImage::FindBoundaryPoints() called");

  // Update origin and spacing of the source image
  this->UpdateSourceParameters();

  // Make sure we're getting everything
  m_SourceImage->SetRequestedRegionToLargestPossibleRegion();

  // Position to figure out pixel location
  TPositionType sourcePosition;

  // Create an iterator to walk the source image
  typedef ImageRegionIterator<TSourceImage> sourceIterator;

  sourceIterator sourceIt = sourceIterator(m_SourceImage,
                                           m_SourceImage->GetRequestedRegion() );

  // Keep track of how many boundary points we found (for debugging)
  unsigned long int numBP = 0;
  unsigned long int numBPadded = 0;

  // Get the index of the pixel
  typename TSourceImage::IndexType sourceIndex;
  IndexType bloxIndex;
  
  for ( sourceIt.GoToBegin(); !sourceIt.IsAtEnd(); ++sourceIt)
    {
    // Figure out the magnitude of the gradient
    double mag = 0;

    for(int i = 0; i < NDimensions; i++)
      {
      mag += sourceIt.Get()[i]
        * sourceIt.Get()[i];
      }

    mag = sqrt(mag);

    // If the pixel meets threshold requirements, add it to the image
    if( mag >= m_Threshold)
      {
      numBP++;

      // Get the index of the boundary pixel
      sourceIndex = sourceIt.GetIndex();

      // Convert the index of the source pixel to the physical location of the
      // boundary point in the source image
      m_SourceImage->TransformIndexToPhysicalPoint(sourceIndex, sourcePosition);

      // Transform the physical location to a blox index
      this->TransformPhysicalPointToIndex(sourcePosition, bloxIndex);

      // Create a new boundary point item and set its parameters
      BloxBoundaryPointItem<NDimensions>* pItem = new BloxBoundaryPointItem<NDimensions>;
      pItem->SetPhysicalPosition(sourcePosition);
      pItem->SetGradient( sourceIt.Get() );

      this->GetPixel(bloxIndex).push_back(pItem);
      numBPadded++;
      }
    }
 
  itkDebugMacro(<< "Finished looking for boundary points\n"
                << "I found " << numBP << " points\n"
                << "I added " << numBPadded << " points\n");
}

template<class TSourceImage, class TImageTraits>
void
BloxBoundaryPointImage<TSourceImage, TImageTraits>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Threshold: " << m_Threshold << std::endl;

  unsigned int i;
  os << indent << "Source origin: [";
  for (i=0; i < NDimensions - 1; i++)
    {
    os << m_SourceOrigin[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Source spacing: [";
  for (i=0; i < NDimensions - 1; i++)
    {
    os << m_SourceSpacing[i] << ", ";
    }
  os << "]" << std::endl;
}

} // end namespace itk

#endif
