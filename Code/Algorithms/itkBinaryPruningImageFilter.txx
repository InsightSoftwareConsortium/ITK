/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryPruningImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryPruningImageFilter_txx
#define __itkBinaryPruningImageFilter_txx

#include <iostream>

#include "itkBinaryPruningImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodIterator.h"

namespace itk
{
/**
 *    Constructor
 */
template< class TInputImage, class TOutputImage >
BinaryPruningImageFilter< TInputImage, TOutputImage >
::BinaryPruningImageFilter()
{
  this->SetNumberOfRequiredOutputs(1);

  OutputImagePointer pruneImage = OutputImageType::New();
  this->SetNthOutput( 0, pruneImage.GetPointer() );

  m_Iteration = 3;
}

/**
 *  Return the pruning Image pointer
 */
template< class TInputImage, class TOutputImage >
typename BinaryPruningImageFilter<
  TInputImage, TOutputImage >::OutputImageType *
BinaryPruningImageFilter< TInputImage, TOutputImage >
::GetPruning(void)
{
  return dynamic_cast< OutputImageType * >(
           this->ProcessObject::GetOutput(0) );
}

/**
 *  Prepare data for computation
 */
template< class TInputImage, class TOutputImage >
void
BinaryPruningImageFilter< TInputImage, TOutputImage >
::PrepareData(void)
{
  itkDebugMacro(<< "PrepareData Start");
  OutputImagePointer pruneImage = GetPruning();

  InputImagePointer inputImage  =
    dynamic_cast< const TInputImage  * >( ProcessObject::GetInput(0) );

  pruneImage->SetBufferedRegion( pruneImage->GetRequestedRegion() );
  pruneImage->Allocate();

  typename OutputImageType::RegionType region  = pruneImage->GetRequestedRegion();

  ImageRegionConstIterator< TInputImage > it(inputImage,  region);
  ImageRegionIterator< TOutputImage >     ot(pruneImage,  region);

  it.GoToBegin();
  ot.GoToBegin();

  itkDebugMacro(<< "PrepareData: Copy input to output");

  while ( !ot.IsAtEnd() )
    {
    ot.Set( static_cast< typename OutputImageType::PixelType >( it.Get() ) );
    ++it;
    ++ot;
    }
  itkDebugMacro(<< "PrepareData End");
}

/**
 *  Post processing for computing thinning
 */
template< class TInputImage, class TOutputImage >
void
BinaryPruningImageFilter< TInputImage, TOutputImage >
::ComputePruneImage()
{
  itkDebugMacro(<< "ComputeThinImage Start");
  OutputImagePointer pruneImage          =  GetPruning();

  typename OutputImageType::RegionType region  = pruneImage->GetRequestedRegion();

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill(1);
  NeighborhoodIteratorType ot(radius, pruneImage, region);

  typename NeighborhoodIteratorType::OffsetType offset1 = { { -1, -1 } };
  typename NeighborhoodIteratorType::OffsetType offset2 = { { -1, 0 } };
  typename NeighborhoodIteratorType::OffsetType offset3 = { { -1, 1 } };
  typename NeighborhoodIteratorType::OffsetType offset4 = { { 0, 1 } };
  typename NeighborhoodIteratorType::OffsetType offset5 = { { 1, 1 } };
  typename NeighborhoodIteratorType::OffsetType offset6 = { { 1, 0 } };
  typename NeighborhoodIteratorType::OffsetType offset7 = { { 1, -1 } };
  typename NeighborhoodIteratorType::OffsetType offset8 = { { 0, -1 } };

  unsigned int count = 0;
  while ( count < m_Iteration )
    {
    ot.GoToBegin();
    while ( !ot.IsAtEnd() )
      {
      if ( ot.GetCenterPixel() )
        {
        PixelType genus;
        genus  = ot.GetPixel(offset1) + ot.GetPixel(offset2);
        genus += ot.GetPixel(offset3) + ot.GetPixel(offset4);
        genus += ot.GetPixel(offset5) + ot.GetPixel(offset6);
        genus += ot.GetPixel(offset7) + ot.GetPixel(offset8);
        if ( genus < 2 )
          {
          genus = 0;
          ot.SetCenterPixel(genus);
          }
        }

      ++ot;
      }
    ++count;
    }
  itkDebugMacro(<< "ComputeThinImage End");
}

/**
 *  Generate PruneImage
 */
template< class TInputImage, class TOutputImage >
void
BinaryPruningImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  this->PrepareData();

  itkDebugMacro(<< "GenerateData: Computing Thinning Image");
  this->ComputePruneImage();
} // end GenerateData()

/**
 *  Print Self
 */
template< class TInputImage, class TOutputImage >
void
BinaryPruningImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Pruning image: " << std::endl;
  os << indent << "Iteration: " << m_Iteration << std::endl;
}
} // end namespace itk

#endif
