/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedComponentImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkConnectedComponentImageFilter_txx
#define _itkConnectedComponentImageFilter_txx

#include "itkConnectedComponentImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkEquivalencyTable.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkConstantBoundaryCondition.h"



namespace itk
{
template< class TInputImage, class TOutputImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}



template <class TInputImage, class TOutputImage>
void 
ConnectedComponentImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template< class TInputImage, class TOutputImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // create an equivalency table
  EquivalencyTable::Pointer eqTable = EquivalencyTable::New();

  OutputPixelType    label, originalLabel, neighborLabel;
  OutputPixelType    maxLabel = NumericTraits<OutputPixelType>::Zero;
  const OutputPixelType maxPossibleLabel=NumericTraits<OutputPixelType>::max();

  typename TOutputImage::Pointer output = this->GetOutput();
  typename TInputImage::ConstPointer input = this->GetInput();

  // Allocate the output and initialize to zeros
  this->AllocateOutputs();
  output->FillBuffer( NumericTraits<OutputPixelType>::Zero );
  
  // Set up the boundary condition to be zero padded (used on output image)
  ConstantBoundaryCondition<TOutputImage> BC;
  BC.SetConstant(NumericTraits<OutputPixelType>::Zero);

  // Neighborhood iterator.  Let's use a shaped neighborhood so we can
  // restrict the access to face connected neighbors. This iterator
  // will be applied to the output image
  typedef ConstShapedNeighborhoodIterator<TOutputImage> NeighborhoodIteratorType;
  SizeType kernelRadius;
  kernelRadius.Fill(1);
  NeighborhoodIteratorType nit
    = NeighborhoodIteratorType(kernelRadius, output,
                               output->GetRequestedRegion());
  nit.OverrideBoundaryCondition(&BC); // assign the boundary condition

  // only activate the indices that are "previous" to the current
  // pixel and face connected (exclude the center pixel from the
  // neighborhood) 
  unsigned int d;
  typename NeighborhoodIteratorType::OffsetType offset;

  offset.Fill(0);
  for (d=0; d < InputImageType::ImageDimension; ++d)
    {
    offset[d] = -1;
    nit.ActivateOffset(offset);
    offset[d] = 0;
    }

  // along with a neighborhood iterator on the input, use a standard
  // iterator on the input and output
  ImageRegionConstIterator<InputImageType> it;
  ImageRegionIterator<OutputImageType> oit;
  it = ImageRegionConstIterator<InputImageType>(input,
                                                output->GetRequestedRegion());
  oit = ImageRegionIterator<OutputImageType>(output,
                                             output->GetRequestedRegion());
  

  // Setup a progress reporter.  We have 3 stages to the algorithm so
  // pretend we have 3 times the number of pixels
  ProgressReporter progress(this, 0,
                           3*output->GetRequestedRegion().GetNumberOfPixels());

  // Mark the output image as either background or unlabeled
  it.GoToBegin();
  oit.GoToBegin();
  while (!it.IsAtEnd())
    {
    if (it.Get() != NumericTraits<InputPixelType>::Zero)
      {
      // mark pixel as unlabeled
      oit.Set(maxPossibleLabel);
      }
    
    ++it;
    ++oit;
    progress.CompletedPixel();
    }

  // iterate over the image, labeling the objects and defining
  // equivalence classes.  Use the neighborhood iterator to access the
  // "previous" neighbor pixels and an output iterator to access the
  // current pixel
  nit.GoToBegin();
  oit.GoToBegin();
  while ( !oit.IsAtEnd() )
    {
    // Get the current pixel label
    label = oit.Get();
    originalLabel = label;

    // If the pixel is not background
    if (label != NumericTraits<OutputPixelType>::Zero)
      {
      // loop over the "previous" neighbors to find labels.  this loop
      // may establish one or more new equivalence classes
      typename NeighborhoodIteratorType::ConstIterator sIt;
      for (sIt = nit.Begin(); !sIt.IsAtEnd(); ++sIt)
        {
        // get the label of the pixel previous to this one along a
        // particular dimension (neighbors activated in neighborhood iterator)
        neighborLabel = sIt.Get();

        // if the previous pixel has a label, verify equivalence or
        // establish a new equivalence
        if (neighborLabel != NumericTraits<OutputPixelType>::Zero)
          {
          // if current pixel is unlabeled, copy the label from neighbor
          if (label == maxPossibleLabel)
            {
            // copy the label from the previous pixel
            label = neighborLabel;
            }
          // else if current pixel has a label that is not already
          // equivalent to the label of the previous pixel, then setup
          // a new equivalence.  note the use of Lookup() and not
          // RecursiveLookup(). this is possible since we keep the
          // equivalence table flat.
          else if ((label != neighborLabel)
                && (eqTable->Lookup(label) != eqTable->Lookup(neighborLabel))) 
            {
            eqTable->Add(label, neighborLabel);

            // if we keep the equivalency table up to date, then we
            // can use straight calls to Lookup() instead of
            // RecursiveLookUp().  This works out to be 3X faster.
            eqTable->Flatten();
            }
          }
        }

      // if none of the "previous" neighbors were set, then make a new label
      if (originalLabel == label)
        {
        // create a new entry label
        if (maxLabel == maxPossibleLabel)
          {
          itkWarningMacro(<< "ConnectedComponentImageFilter::GenerateData: Number of labels exceeds number of available labels for the output type." );
          }
        else
          {
          ++maxLabel;
          }

        // assign the new label
        label = maxLabel;
        }

      // Finally, set the output pixel to whatever label we have
      if (label != originalLabel)
        {
        oit.Set( label );
        }
      }

    // move the iterators
    ++nit;
    ++oit;
    progress.CompletedPixel();
    }

  // Flatten the equavalency table
  eqTable->Flatten();

  // remap the labels
  oit.GoToBegin();
  while ( !oit.IsAtEnd() )
    {
    label = oit.Get();
    // if pixel has a label, write out the final equivalence
    if (label != NumericTraits<OutputPixelType>::Zero)
      {
      oit.Set( eqTable->Lookup( label ) );
      }
    ++oit;
    progress.CompletedPixel();
    }
}

} // end namespace itk

#endif
