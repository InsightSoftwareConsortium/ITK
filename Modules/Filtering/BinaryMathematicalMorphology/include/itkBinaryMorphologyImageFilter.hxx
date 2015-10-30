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
#ifndef itkBinaryMorphologyImageFilter_hxx
#define itkBinaryMorphologyImageFilter_hxx

#include "itkImageRegionIteratorWithIndex.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionConstIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkConstantBoundaryCondition.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"
#include "itkBinaryMorphologyImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel >
BinaryMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::BinaryMorphologyImageFilter()
{
  m_ForegroundValue = NumericTraits< InputPixelType >::max();
  m_BackgroundValue = NumericTraits< OutputPixelType >::NonpositiveMin();
  //this->SetNumberOfThreads(1);
  this->AnalyzeKernel();
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
BinaryMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::SetKernel(const KernelType & kernel)
{
  Superclass::SetKernel(kernel);
  // Analyse it: the following process depends only on kernel
  this->AnalyzeKernel();
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
BinaryMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::AnalyzeKernel(void)
{
  // Sure clearing
  m_KernelDifferenceSets.clear();
  m_KernelCCVector.clear();

  std::vector< unsigned int > kernelOnElements;

  IndexValueType i, k;

  // **************************
  // Structuring element ( SE ) coding
  // **************************

  // Get symmetrical structuring element in order to satisfy
  // our definition of binary dilation
//   InputSizeValueType kernelSize      = this->GetKernel().Size();
//   InputSizeValueType kernelCenter    = kernelSize / 2;
//
//   for( i = kernelCenter + 1, k = kernelCenter - 1; i < kernelSize; ++i, --k )
//     {
//     typename TKernel::PixelType px     =
// this->GetKernel().GetBufferReference()[i];
//     this->GetKernel().GetBufferReference()[i]  =
// this->GetKernel().GetBufferReference()[k];
//     this->GetKernel().GetBufferReference()[k]  = px;
//     }

  // Store index of SE of ON elements
  // It allows us to have a fastest access to ON elements
  // of SE Kernel
  KernelIteratorType KernelBegin  = this->GetKernel().Begin();
  KernelIteratorType KernelEnd    = this->GetKernel().End();
  KernelIteratorType kernel_it;

  for ( i = 0, kernel_it = KernelBegin; kernel_it != KernelEnd; ++kernel_it, ++i )
    {
    if ( *kernel_it )
      {
      kernelOnElements.push_back(i);
      }
    }

  // Compute the Nd vector ( called index in case of images...do not
  // mistake with index in case of neighbourhood which is only a
  // position in a 1 dimensional buffer...! ) of the center element in
  // the SE neighbourhood
  IndexType centerElementPosition;
  for ( unsigned int d = 0; d < TInputImage::ImageDimension; ++d )
    {
    // position of center in a given direction is the middle of the direction
    centerElementPosition[d] = this->GetKernel().GetSize(d) / 2;
    }

  // We have to detect the connected component of the structuring
  // element and compute the difference sets in each direction ( 26
  // connectivity in 3D for instance )

  // Detect all the connected components of the SE.
  // ----------------------------------------------
  // To do this we convert the SE into a temp image
  typedef Image< bool, TInputImage::ImageDimension > BoolImageType;
  typename BoolImageType::Pointer tmpSEImage = BoolImageType::New();
  tmpSEImage->SetRegions( this->GetKernel().GetSize() );

  // allocation
  tmpSEImage->Allocate();

  // copy
  ImageRegionIterator< BoolImageType > kernelImageIt; // iterator on image
  kernelImageIt = ImageRegionIterator< BoolImageType >( tmpSEImage,
                                                        tmpSEImage->GetRequestedRegion() );

  kernelImageIt.GoToBegin();
  kernel_it = KernelBegin;

  while ( !kernelImageIt.IsAtEnd() )
    {
    kernelImageIt.Set(*kernel_it ? true : false);
    ++kernelImageIt;
    ++kernel_it;
    }

  // boundary conditions
  // Out boundary pixels are set to false
  ConstantBoundaryCondition< BoolImageType > cbc;
  cbc.SetConstant(false);

  // Now look for connected component and record one SE element
  // position for each CC.
  ImageRegionIteratorWithIndex< BoolImageType >
  kernelImageItIndex( tmpSEImage, tmpSEImage->GetRequestedRegion() );

  // Neighborhood iterator on SE element temp image
  InputSizeType padBy;
  padBy.Fill(1);
  NeighborhoodIterator< BoolImageType >
  SEoNeighbIt( padBy, tmpSEImage, tmpSEImage->GetRequestedRegion() );
  SEoNeighbIt.OverrideBoundaryCondition(&cbc);
  SizeValueType neighborhoodSize = SEoNeighbIt.Size();

  // Use a FIFO queue in order to perform the burning process
  // which allows to identify the connected components of SE
  std::queue< IndexType > propagQueue;

  // Clear vector of recorded CCs
  m_KernelCCVector.clear();

  // walk both the "image" of the kernel iterator and the kernel
  // iterator. use the "image" iterator to keep track of
  // components. use the kernel iterator for quick access of offsets
  kernel_it = KernelBegin;
  kernelImageItIndex.GoToBegin();
  while ( !kernelImageItIndex.IsAtEnd() )
    {
    // If a ON element is found track the CC
    if ( kernelImageItIndex.Get() )
      {
      // Mark current element
      kernelImageItIndex.Set(false);

      // add it to queue
      propagQueue.push( kernelImageItIndex.GetIndex() );

      // We know also that we start a new CC, so we store the position of this
      // element relatively to center of kernel ( i.e a vector ).
      OffsetType offset = this->GetKernel().GetOffset(kernel_it - KernelBegin);
      m_KernelCCVector.push_back(offset);

      // Process while FIFO queue is not empty
      while ( !propagQueue.empty() )
        {
        // Extract pixel index from queue
        IndexType currentIndex = propagQueue.front();
        propagQueue.pop();

        // Now look for neighbours that are also ON pixels
        SEoNeighbIt.GoToBegin();
        SEoNeighbIt.SetLocation(currentIndex);

        for ( SizeValueType ii = 0; ii < neighborhoodSize; ++ii )
          {
          // If current neighb pixel is ON, mark it and push it into queue
          if ( SEoNeighbIt.GetPixel(ii) )
            {
            // Mark it
            bool bIsBounds;
            SEoNeighbIt.SetPixel(ii, false, bIsBounds);

            // Push
            propagQueue.push( SEoNeighbIt.GetIndex(ii) );
            }
          }
        } // while ( !propagQueue.empty() )
      }   // if( kernelImageItIndex.Get() )

    ++kernelImageItIndex;
    ++kernel_it;
    }

  // Free memory of tmp image
  tmpSEImage->Initialize();

  // Now look for difference sets
  // ----------------------------
  // Create a neighbourhood of radius m_Radius This neighbourhood is
  // called adj neighbourhood and is used in order to get the offset
  // in each direction.
  Neighborhood< InputPixelType, InputImageDimension > adjNeigh;
  adjNeigh.SetRadius(padBy);

  // now we look for the difference sets in each directions: If you
  // take a structuring element (SE) and you translate it in one of
  // the direction of the adjacency ( (-1,0,0), (-1,1,0), etc. ) you
  // get SE(dir). Now the difference set is SE(dir) - SE. So when you
  // want to paint SE union SE(dir) you just need to paint SE and the
  // difference set.

  // Allocate difference sets container
  m_KernelDifferenceSets.resize( adjNeigh.Size() );

  // For each direction of the connectivity, look for difference set
  // in this direction
  for ( SizeValueType ii = 0; ii < adjNeigh.Size(); ++ii )
    {
    m_KernelDifferenceSets[ii].clear();
    // For each element of the kernel which index is k, see if they
    // belong to this difference set treat only "ON" elements of SE
    std::vector< unsigned int >::const_iterator kernelOnElementsIt;
    for ( kernelOnElementsIt = kernelOnElements.begin();
          kernelOnElementsIt != kernelOnElements.end(); ++kernelOnElementsIt )
      {
      // Get the index in the SE neighb
      k = *kernelOnElementsIt;

      // Get the Nd position of current SE element. In order to do
      // that, we have not a "GetIndex" function.  So first we get the
      // offset relatively to the center SE element and add it to the
      // index of this center SE element:
      OffsetType currentOffset = this->GetKernel().GetOffset(k);
      IndexType  currentShiftedPosition = centerElementPosition + currentOffset;

      // Add to current element position the offset corresponding the
      // current adj direction
      currentShiftedPosition += adjNeigh.GetOffset(ii);

      // now currentShiftedPosition is the position of the current
      // "pixel" ( SE element ) shifted in the direction ii. Check if it
      // is outside the structuring element. If it is the case, we
      // know that the current SE element is in the difference set of
      // the current direction ii (this works only for boundary pixels!).
      bool bIsOutside   = false;
      for ( unsigned int dimCount = 0;
            dimCount < TInputImage::ImageDimension; ++dimCount )
        {
        if ( currentShiftedPosition[dimCount] < 0
             || currentShiftedPosition[dimCount] >=
             (int)this->GetKernel().GetSize(dimCount) )
          {
          bIsOutside = true;
          break;
          }
        }

      if ( bIsOutside )
        {
        // The current SE element, which index is hence k, belongs to
        // the difference set in the direction ii.  Add it to
        // difference set in dir ii
        m_KernelDifferenceSets[ii].push_back(currentOffset);
        }
      else
        {
        // The current shifted SE element doesn't belong to SE
        // boundaries. In order to see if it belongs to difference set
        // in direction ii, the value of kernel at the position of the
        // current SE element SHIFTED in direction ii must be OFF ( i.e
        // = 0 ) In order to access to shifted value we must compute a
        // neighbourhood index

        // retrieve the index offset relatively to the current NOT
        // shifted SE element
        unsigned int currentRelativeIndexOffset =
          this->GetKernel().GetNeighborhoodIndex( adjNeigh.GetOffset(ii) )
          - this->GetKernel().GetCenterNeighborhoodIndex();

        // Now thanks to this relative offset, we can get the absolute
        // neigh index of the current shifted SE element.
        unsigned int currentShiftedIndex =
          k /*NOT shifted position*/ +  currentRelativeIndexOffset;

        // Test if shifted element is OFF: in fact diff(dir) = all the
        // elements of SE + dir where elements of SE is ON and
        // elements of SE + dir is OFF.
        if ( !this->GetKernel()[currentShiftedIndex] )
          {
          // Add it to difference set in dir ii
          m_KernelDifferenceSets[ii].push_back(currentOffset);
          }
        }
      } // for( kernelOnElementsIt = kernelOnElements.begin(); ...
    }   // for( ii = 0; ii < adjNeigh.Size(); ++ii )

  // For the particular case of the m_KernelDifferenceSets at the
  // center of the kernel ( the difference set is theoretically empty
  // in this case because there is no shift ) we put the kernel set
  // itself, useful for the rest of the process.
  unsigned int centerKernelIndex  = adjNeigh.Size() / 2;
  for ( k = 0, kernel_it = KernelBegin; kernel_it != KernelEnd; ++kernel_it, ++k )
    {
    if ( *kernel_it )
      {
      OffsetType currentOffset = this->GetKernel().GetOffset(k);
      m_KernelDifferenceSets[centerKernelIndex].push_back(currentOffset);
      }
    }
}

/**
 * Standard "PrintSelf" method
 */
template< typename TInputImage, typename TOutput, typename TKernel >
void
BinaryMorphologyImageFilter< TInputImage, TOutput, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Foreground Value: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_ForegroundValue ) << std::endl;
  os << indent << "Background Value: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_BackgroundValue ) << std::endl;
  os << indent << "BoundaryToForeground: " << m_BoundaryToForeground << std::endl;
}
} // end namespace itk

#endif
