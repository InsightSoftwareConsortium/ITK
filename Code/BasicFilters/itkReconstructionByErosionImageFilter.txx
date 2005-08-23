/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReconstructionByErosionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

    This software is distributed WITHOUT ANY WARRANTY; without even 
    the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
    PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkReconstructionByErosionImageFilter_txx
#define __itkReconstructionByErosionImageFilter_txx

#include <algorithm>
#include <set>
#include <list>
#include "itkReconstructionByErosionImageFilter.h"
#include "itkProgressReporter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkConstantBoundaryCondition.h"

namespace itk {

template <class TInputImage, class TOutputImage>
ReconstructionByErosionImageFilter<TInputImage, TOutputImage>
::ReconstructionByErosionImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  m_FullyConnected = false;
}


template <class TInputImage, class TOutputImage>
void 
ReconstructionByErosionImageFilter<TInputImage, TOutputImage>
::SetMarkerImage(const MarkerImageType* markerImage)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput(0, const_cast<MarkerImageType *>( markerImage ));
}


template <class TInputImage, class TOutputImage>
const typename ReconstructionByErosionImageFilter<TInputImage, TOutputImage>::MarkerImageType *
ReconstructionByErosionImageFilter<TInputImage, TOutputImage>
::GetMarkerImage()
{
  return this->GetInput(0);
}


template <class TInputImage, class TOutputImage>
void 
ReconstructionByErosionImageFilter<TInputImage, TOutputImage>
::SetMaskImage(const MaskImageType* maskImage)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput(1, const_cast<MaskImageType *>( maskImage ));
}


template <class TInputImage, class TOutputImage>
const typename ReconstructionByErosionImageFilter<TInputImage, TOutputImage>::MaskImageType *
ReconstructionByErosionImageFilter<TInputImage, TOutputImage>
::GetMaskImage()
{
  return this->GetInput(1);
}


template <class TInputImage, class TOutputImage>
void 
ReconstructionByErosionImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the inputs
  MarkerImagePointer  markerPtr =
    const_cast< MarkerImageType * >( this->GetInput(0) );

  MaskImagePointer  maskPtr = 
    const_cast< MaskImageType * >( this->GetInput(1) );
  
  if ( !markerPtr || !maskPtr )
    {
    return;
    }

  // We need to
  // configure the inputs such that all the data is available.
  //
  markerPtr->SetRequestedRegion(markerPtr->GetLargestPossibleRegion());
  maskPtr->SetRequestedRegion(maskPtr->GetLargestPossibleRegion());
}


template <class TInputImage, class TOutputImage>
void 
ReconstructionByErosionImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage, class TOutputImage>
void
ReconstructionByErosionImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  this->AllocateOutputs();
  // Set up the progress reporter for init step of the algorithm (about 2/3 of total computation time)
  ProgressReporter progress(this, 0, this->GetMarkerImage()->GetRequestedRegion().GetNumberOfPixels(), 67, 0, 0.67);
  
  // mask and marker must have the same size
  if ( this->GetMarkerImage()->GetRequestedRegion().GetSize() != this->GetMaskImage()->GetRequestedRegion().GetSize() )
    {
    itkExceptionMacro( << "Marker and mask must have the same size." );
    }
  
  // iterator for the marker image
  typedef ImageRegionConstIteratorWithIndex<MarkerImageType> MarkerIteratorType;
  typedef typename MarkerIteratorType::IndexType IndexType;
  MarkerIteratorType markerIt(this->GetMarkerImage(), this->GetMarkerImage()->GetRequestedRegion());
  
  // iterator for the mask image
  typedef ImageRegionConstIterator<MaskImageType> MaskSimpleIteratorType;
  MaskSimpleIteratorType maskIt(this->GetMaskImage(), this->GetMaskImage()->GetRequestedRegion());
  
  // iterator to copy marker to output image
  typedef ImageRegionIterator< TOutputImage > CopyOutputIterator;
  CopyOutputIterator cpOutIt( this->GetOutput(), this->GetOutput()->GetRequestedRegion() );
  
  // create map to store     pixel value -> [pos1, pos2 .. posn]
  typedef std::set<IndexType, ITK_TYPENAME IndexType::LexicographicCompare> SetType;
  typedef typename std::map <MarkerImagePixelType, SetType> PixelMapType;
  PixelMapType pixelMap;
  
  for ( markerIt.GoToBegin(), maskIt.GoToBegin(), cpOutIt.GoToBegin(); !markerIt.IsAtEnd(); ++markerIt, ++maskIt, ++cpOutIt ) 
    {
    // store index of current pixel value
    IndexType idx = markerIt.GetIndex();
    pixelMap[markerIt.Get()].insert(idx);
    // copy marker to output
    cpOutIt.Set( static_cast<OutputImagePixelType>( markerIt.Get() ) );
    // be sure that mask values are above marker values
    if ( markerIt.Get() < maskIt.Get() )
      {
      itkExceptionMacro( << "Marker pixels must be >= mask pixels." );
      }
    // update progress
    progress.CompletedPixel();
    }
  // end of first part of the algorithm
  
  // Set up the progress reporter for second step of the algorithm (about 1/3 of total computation time)
  // we can't found the exact number of pixel to process, so use the maximum number possible.
  ProgressReporter progress2(this, 0, this->GetMarkerImage()->GetRequestedRegion().GetNumberOfPixels(), 33, 0.67, 0.33);
  
  // biggest value will never propagate, so we can remove it from map
  // and save lots of cycle (in binary images for example)
  SetType* biggestPixels = &(pixelMap[ pixelMap.rbegin()->first ]);
  biggestPixels->clear();

  // create neighbor iterator over output image
  typedef ShapedNeighborhoodIterator<TOutputImage> OutputIteratorType;
  typename OutputIteratorType::RadiusType radius;
  radius.Fill(1);
  OutputIteratorType oIt(radius, this->GetOutput(), this->GetMaskImage()->GetRequestedRegion());

  // create neighbor iterator over mask image
  typedef ConstShapedNeighborhoodIterator<MaskImageType> MaskIteratorType;
  MaskIteratorType mIt(radius, this->GetMaskImage(), this->GetMaskImage()->GetRequestedRegion());
  // set boundary condition to maximum possible value, so we will
  // never try to set pixel value outside output image
  ConstantBoundaryCondition<MaskImageType> cbc;
  cbc.SetConstant(NumericTraits<MaskImagePixelType>::max());
  mIt.OverrideBoundaryCondition(&cbc);

  // activate neighbors 
  typename OutputIteratorType::OffsetType offset;
  unsigned int d;
  typename OutputIteratorType::OffsetValueType i;
  offset.Fill(0);
  
  if (!m_FullyConnected)
    {
    for (d=0; d < TInputImage::ImageDimension; ++d)
      {
      for (i=-1; i<=1; i+=2)
        {
        offset[d] = i;
        oIt.ActivateOffset(offset); // a neighbor pixel in dimension d
        mIt.ActivateOffset(offset);
        }
      offset[d] = 0;
      }
    }
  else
    {
    // activate all pixels excepted center pixel
    for (d=0; d < oIt.GetCenterNeighborhoodIndex()*2+1; ++d)
      {
        oIt.ActivateOffset(oIt.GetOffset(d));
        mIt.ActivateOffset(oIt.GetOffset(d));
      }
    offset.Fill(0);
    oIt.DeactivateOffset(offset);
    mIt.DeactivateOffset(offset);
    }
  //oIt.Print(std::cout);
  //mIt.Print(std::cout);
  oIt.GoToBegin();
  mIt.GoToBegin();
    
  // iterate over pixel values, from low to high
  for ( typename PixelMapType::iterator pixelMapIt=pixelMap.begin(); pixelMapIt!=pixelMap.end(); ++pixelMapIt )
    {
    MarkerImagePixelType pixelValue = pixelMapIt->first;
    SetType* indexes = &(pixelMapIt->second);
    
    // create list to store new indexes. We store new indexes in a list instead of the set where first 
    // indexes are stored because indexes can be added before actual position in iterator
    typedef std::list<IndexType> ListType;
    ListType newIndexes;
    
    // iterate over pixel indexes
    for ( typename SetType::iterator idxIt = indexes->begin(); idxIt != indexes->end(); ++idxIt )
      {
      // shift output and mask iterators to new location
      oIt += *idxIt - oIt.GetIndex();
      mIt += *idxIt - mIt.GetIndex();
      // and now, iterate over neighbors
      typename MaskIteratorType::ConstIterator nmIt;
      typename OutputIteratorType::Iterator noIt;
      for ( noIt = oIt.Begin(),  nmIt= mIt.Begin(); noIt != oIt.End() /*&& nmIt != mIt.End()*/; noIt++, nmIt++)
        {
        // get value of current neighbor
         OutputImagePixelType nValue = noIt.Get();
        // value in constrained by the mask so get the smallest value of mask and current pixel
        OutputImagePixelType contrainedValue = vnl_math_max( static_cast<OutputImagePixelType>( pixelValue ), static_cast<OutputImagePixelType>( nmIt.Get() ) );
        if ( nValue > contrainedValue )
          {
          // get index of current neighbor
          IndexType nIdx = oIt.GetIndex() + noIt.GetNeighborhoodOffset();
         // set new neighbor value, and move his index at the good place
          noIt.Set( contrainedValue );
          if ( contrainedValue == pixelValue )
            {
            // new value is the one we are using, so the index must be added in newIndexes
            newIndexes.push_back( nIdx );
            }
          else
            {
            pixelMap[nValue].erase( nIdx );
            pixelMap[contrainedValue].insert( nIdx );
            }
          }
        }
      progress2.CompletedPixel();
      }
   
    // do the same for new indexes  !
    for ( typename ListType::iterator idxIt = newIndexes.begin(); idxIt != newIndexes.end(); ++idxIt )
      {
      // shift output and mask iterators to new location
      oIt += *idxIt - oIt.GetIndex();
      mIt += *idxIt - mIt.GetIndex();
      // and now, iterate over neighbors
      typename MaskIteratorType::ConstIterator nmIt;
      typename OutputIteratorType::Iterator noIt;
      for ( noIt = oIt.Begin(),  nmIt= mIt.Begin(); noIt != oIt.End() /*&& nmIt != mIt.End()*/; noIt++, nmIt++)
        {
        OutputImagePixelType nValue = noIt.Get();
        OutputImagePixelType contrainedValue = vnl_math_max( static_cast<OutputImagePixelType>( pixelValue ), static_cast<OutputImagePixelType>( nmIt.Get() ) );
        if ( nValue > contrainedValue )
          {
          IndexType nIdx = oIt.GetIndex() + noIt.GetNeighborhoodOffset();
          noIt.Set( contrainedValue );
          if ( contrainedValue == pixelValue )
            {
            newIndexes.push_back( nIdx );
            }
          else
            {
            pixelMap[nValue].erase( nIdx );
            pixelMap[contrainedValue].insert( nIdx );
            }
          }
        }
      progress2.CompletedPixel();
      }
    }
  // end of second part of algorithm
}



template<class TInputImage, class TOutputImage>
void
ReconstructionByErosionImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}
  
}// end namespace itk
#endif
