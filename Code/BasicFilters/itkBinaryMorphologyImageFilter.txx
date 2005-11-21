/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMorphologyImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBinaryMorphologyImageFilter_txx
#define _itkBinaryMorphologyImageFilter_txx

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkConstantBoundaryCondition.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"
#include "itkBinaryMorphologyImageFilter.h"

namespace itk
{
        
template <class TInputImage, class TOutputImage, class TKernel>
BinaryMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::BinaryMorphologyImageFilter()
  : m_Kernel()            
{
  m_Radius.Fill(1);
  m_ForegroundValue = NumericTraits<InputPixelType>::max();
  m_BackgroundValue = NumericTraits<OutputPixelType>::NonpositiveMin();
  this->SetNumberOfThreads(1);
}

template <class TInputImage, class TOutputImage, class TKernel>
void 
BinaryMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::GenerateInputRequestedRegion() throw (InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  typename Superclass::InputImagePointer inputPtr = 
    const_cast< TInputImage * >( this->GetInput() );
  
  if ( !inputPtr )
    {
    return;
    }

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // The input image needs to be large enough to support:
  //   1. The size of the structuring element
  //   2. The size of the connectivity element (typically one)
  InputSizeType padBy = m_Radius;
  for (unsigned int i=0; i < KernelDimension; ++i)
    {
    padBy[i] =
      (padBy[i]>m_Kernel.GetRadius(i) ? padBy[i] : m_Kernel.GetRadius(i));
    }
  inputRequestedRegion.PadByRadius( padBy );

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion()) )
    {
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    
    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    OStringStream msg;
    msg << static_cast<const char *>(this->GetNameOfClass())
        << "::GenerateInputRequestedRegion()";
    e.SetLocation(msg.str().c_str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

        
template< class TInputImage, class TOutputImage, class TKernel>
void
BinaryMorphologyImageFilter< TInputImage, TOutputImage, TKernel>
::SetKernel( const KernelType& kernel )
{
  // Set Kernel
  m_Kernel = kernel;

  // Analyse it: the following process depends only on kernel
  this->AnalyzeKernel();
}


template< class TInputImage, class TOutputImage, class TKernel>
void
BinaryMorphologyImageFilter< TInputImage, TOutputImage, TKernel>
::AnalyzeKernel( void )
{
  // Sure clearing
  m_KernelDifferenceSets.clear();
  m_KernelCCVector.clear();

  std::vector<unsigned int> kernelOnElements;
                
  unsigned long i,k;

  // **************************
  // Structuring element ( SE ) coding
  // **************************

  // Get symmetrical structuring element in order to satisfy
  // our definition of binary dilation
  unsigned long kernelSize      = m_Kernel.Size();
  unsigned long kernelCenter    = kernelSize / 2;

  for( i = kernelCenter + 1, k = kernelCenter - 1; i < kernelSize; ++i, --k )
    {
    typename TKernel::PixelType px     = m_Kernel.GetBufferReference()[i];
    m_Kernel.GetBufferReference()[i]  = m_Kernel.GetBufferReference()[k];
    m_Kernel.GetBufferReference()[k]  = px;
    }

  // Store index of SE of ON elements
  // It allows us to have a fastest access to ON elements
  // of SE Kernel
  KernelIteratorType KernelBegin  = m_Kernel.Begin();
  KernelIteratorType KernelEnd    = m_Kernel.End();
  KernelIteratorType kernel_it;
                
  for ( i=0, kernel_it=KernelBegin; kernel_it != KernelEnd; ++kernel_it, ++i)
    {
    if ((*kernel_it) > 0)
      {
      kernelOnElements.push_back(i);
      }
    }

  // Compute the Nd vector ( called index in case of images...do not
  // mistake with index in case of neighbourhood which is only a
  // position in a 1 dimensional buffer...! ) of the center element in
  // the SE neighbourhood
  IndexType centerElementPosition;
  for( i = 0; i < TInputImage::ImageDimension; ++i )
    {
    // position of center in a given direction is the middle of the direction
    centerElementPosition[i] = m_Kernel.GetSize(i) / 2;
    }

  // We have to detect the connected component of the structuring
  // element and compute the difference sets in each direction ( 26
  // connectivity in 3D for instance )

  // Detect all the connected components of the SE.
  // ----------------------------------------------
  // To do this we convert the SE into a temp image
  typedef Image< bool, TInputImage::ImageDimension > BoolImageType;
  typename BoolImageType::Pointer tmpSEImage = BoolImageType::New();
  tmpSEImage->SetRegions( m_Kernel.GetSize() );
                
  // allocation
  tmpSEImage->Allocate();

  // copy
  ImageRegionIterator<BoolImageType> kernelImageIt;// iterator on image
  kernelImageIt = ImageRegionIterator<BoolImageType>(tmpSEImage,
                                 tmpSEImage->GetRequestedRegion());

  kernelImageIt.GoToBegin();
  kernel_it = KernelBegin;

  while( !kernelImageIt.IsAtEnd() )
    {
    kernelImageIt.Set( *kernel_it > 0 );
    ++kernelImageIt;
    ++kernel_it;
    }

        
  // boundary conditions
  // Out boundary pixels are set to false
  ConstantBoundaryCondition<BoolImageType> cbc;
  cbc.SetConstant( false );

  // Now look for connected component and record one SE element
  // position for each CC.
  ImageRegionIteratorWithIndex<BoolImageType> kernelImageItIndex;
  kernelImageItIndex
    = ImageRegionIteratorWithIndex<BoolImageType>(tmpSEImage,
                                           tmpSEImage->GetRequestedRegion());

  // Neighborhood iterator on SE element temp image
  NeighborhoodIterator<BoolImageType> SEoNeighbIt;
  SEoNeighbIt = NeighborhoodIterator<BoolImageType>( m_Radius, tmpSEImage,
                                            tmpSEImage->GetRequestedRegion());
  SEoNeighbIt.OverrideBoundaryCondition(&cbc);
  unsigned int neighborhoodSize = SEoNeighbIt.Size();

  // Use a FIFO queue in order to perform the burning process
  // which allows to identify the connected components of SE
  std::queue<IndexType> propagQueue;  
                
  // Clear vector of recorded CCs
  m_KernelCCVector.clear();

  // walk both the "image" of the kernel iterator and the kernel
  // iterator. use the "image" iterator to keep track of
  // components. use the kernel iterator for quick access of offsets
  kernel_it = KernelBegin;
  kernelImageItIndex.GoToBegin();
  while( !kernelImageItIndex.IsAtEnd() )
    {
    // If a ON element is found track the CC
    if( kernelImageItIndex.Get() )
      {
      // Mark current element
      kernelImageItIndex.Set( false );
                                
      // add it to queue
      propagQueue.push( kernelImageItIndex.GetIndex() );
                                
      // We know also that we start a new CC, so we store the position of this
      // element relatively to center of kernel ( i.e a vector ).
      OffsetType offset = m_Kernel.GetOffset(kernel_it - KernelBegin);
      m_KernelCCVector.push_back( offset );
                                
      // Process while FIFO queue is not empty
      while ( !propagQueue.empty() )
        {
        // Extract pixel index from queue
        IndexType currentIndex = propagQueue.front();
        propagQueue.pop();
                                        
        // Now look for neighbours that are also ON pixels
        SEoNeighbIt.GoToBegin();
        SEoNeighbIt.SetLocation( currentIndex );
                                        
        for (i = 0; i < neighborhoodSize; ++i)
          {
          // If current neighb pixel is ON, mark it and push it into queue
          if( SEoNeighbIt.GetPixel(i) )
            {
            // Mark it
            bool bIsBounds;
            SEoNeighbIt.SetPixel(i, false, bIsBounds);

            // Push
            propagQueue.push( SEoNeighbIt.GetIndex(i) );
            }
          }                  
        } // while ( !propagQueue.empty() )

      } // if( kernelImageItIndex.Get() )

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
  Neighborhood<InputPixelType, InputImageDimension> adjNeigh;
  adjNeigh.SetRadius(m_Radius);

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
  for( i = 0; i < adjNeigh.Size(); ++i )
    {
    m_KernelDifferenceSets[i].clear();
    // For each element of the kernel wich index is k, see if they
    // belong to this difference set treat only "ON" elements of SE
    std::vector<unsigned int>::const_iterator kernelOnElementsIt;
    for( kernelOnElementsIt = kernelOnElements.begin();
         kernelOnElementsIt != kernelOnElements.end(); ++kernelOnElementsIt )
      {
      // Get the index in the SE neighb
      k = *kernelOnElementsIt;
                                
      // Get the Nd position of current SE element. In order to do
      // that, we have not a "GetIndex" function.  So first we get the
      // offset relatively to the center SE element and add it to the
      // index of this center SE element:
      OffsetType currentOffset = m_Kernel.GetOffset(k);
      IndexType currentShiftedPosition = centerElementPosition + currentOffset;
                                
      // Add to current element position the offset corresponding the
      // current adj direction
      currentShiftedPosition += adjNeigh.GetOffset(i);
                                
      // now currentShiftedPosition is the position of the current
      // "pixel" ( SE element ) shifted in the direction i. Check if it
      // is outside the structuring element. If it is the case, we
      // know that the current SE element is in the difference set of
      // the current direction i (this works only for boundary pixels!).
      bool bIsOutside   = false;
      for( unsigned int dimCount = 0;
           dimCount < TInputImage::ImageDimension; ++dimCount )
        {
        if( currentShiftedPosition[dimCount] < 0
            || currentShiftedPosition[dimCount] >=
               (int)m_Kernel.GetSize(dimCount) )
          {
          bIsOutside = true;
          break;
          }
        }
                                
      if( bIsOutside )
        {
        // The current SE element, which index is hence k, belongs to
        // the difference set in the direction i.  Add it to
        // difference set in dir i
        m_KernelDifferenceSets[i].push_back(k);
        }
      else
        {
        // The current shifted SE element doesn't belong to SE
        // boundaries. In order to see if it belongs to difference set
        // in direction i, the value of kernel at the position of the
        // current SE element SHIFTED in direction i must be OFF ( i.e
        // = 0 ) In order to access to shifted value we must compute a
        // neighbourhood index
                                        
        // retrieve the index offset relatively to the current NOT
        // shifted SE element
        unsigned int currentRelativeIndexOffset
          = m_Kernel.GetNeighborhoodIndex( adjNeigh.GetOffset(i) )
          - m_Kernel.GetCenterNeighborhoodIndex();
                                        
        // Now thanks to this relative offset, we can get the absolute
        // neigh index of the current shifted SE element.
        unsigned int currentShiftedIndex
          = k /*NOT shifted position*/ +  currentRelativeIndexOffset;
                                        
        // Test if shifted element is OFF: in fact diff(dir) = all the
        // elements of SE + dir where elements of SE is ON and
        // elements of SE + dir is OFF.
        if( m_Kernel[currentShiftedIndex] <= 0 )
          {
          // Add it to difference set in dir i
          m_KernelDifferenceSets[i].push_back(k);
          }
        }                                                                       
                                
      } // for( kernelOnElementsIt = kernelOnElements.begin(); ...
    } // for( i = 0; i < adjNeigh.Size(); ++i )

  // For the particular case of the m_KernelDifferenceSets at the
  // center of the kernel ( the difference set is theoretically empty
  // in this case because there is no shift ) we put the kernel set
  // itself, useful for the rest of the process.
  unsigned int centerKernelIndex  = adjNeigh.Size() / 2;
  for ( k=0, kernel_it=KernelBegin; kernel_it != KernelEnd; ++kernel_it, ++k)
    {
    if ((*kernel_it) > 0)
      {
      m_KernelDifferenceSets[centerKernelIndex].push_back(k);
      }
    }
}


/**
 * Standard "PrintSelf" method
 */
template <class TInputImage, class TOutput, class TKernel>
void
BinaryMorphologyImageFilter<TInputImage, TOutput, TKernel>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Radius which defines connectivity neighborhood: " << m_Radius << std::endl;
  os << indent << "Kernel which defines structuring element: " << m_Kernel << std::endl;
  os << indent << "Foreground Value: " << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_ForegroundValue) << std::endl;
  os << indent << "Background Value: " << static_cast<typename NumericTraits<OutputPixelType>::PrintType>(m_BackgroundValue) << std::endl;
}

} // end namespace itk

#endif
