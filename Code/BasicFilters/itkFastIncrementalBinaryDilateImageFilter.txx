/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastIncrementalBinaryDilateImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFastIncrementalBinaryDilateImageFilter_txx
#define _itkFastIncrementalBinaryDilateImageFilter_txx

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
#include "itkFastIncrementalBinaryDilateImageFilter.h"

namespace itk
{
        
template <class TInputImage, class TOutputImage, class TKernel>
FastIncrementalBinaryDilateImageFilter<TInputImage, TOutputImage, TKernel>
::FastIncrementalBinaryDilateImageFilter()
  : m_Kernel()            
{
  m_Radius.Fill(1);
  m_DilateValue = NumericTraits<InputPixelType>::max();
  m_BackgroundValue = NumericTraits<OutputPixelType>::NonpositiveMin();
}

template <class TInputImage, class TOutputImage, class TKernel>
void 
FastIncrementalBinaryDilateImageFilter<TInputImage, TOutputImage, TKernel>
::GenerateInputRequestedRegion() throw (InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  typename Superclass::InputImagePointer inputPtr = 
    const_cast< TInputImage * >( this->GetInput() );
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();
  
  if ( !inputPtr || !outputPtr )
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
FastIncrementalBinaryDilateImageFilter< TInputImage, TOutputImage, TKernel>
::SetKernel( const KernelType& kernel )
{
  // Set Kernel
  m_Kernel = kernel;

  // Analyse it: the following process depends only on kernel
  this->AnalyzeKernel();
}


template< class TInputImage, class TOutputImage, class TKernel>
void
FastIncrementalBinaryDilateImageFilter< TInputImage, TOutputImage, TKernel>
::AnalyzeKernel( void )
{
  // Sure clearing
  m_KernelDifferenceSets.clear();
  m_KernelCCVectors.clear();
  m_KernelOnElements.clear();
                
                
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
    typename TKernel::PixelType px    = m_Kernel.GetBufferReference()[i];
    m_Kernel.GetBufferReference()[i]  = m_Kernel.GetBufferReference()[k];
    m_Kernel.GetBufferReference()[k]  = px;
    }

  // Store index of SE of ON elements
  // It allows us to have a fastest access to ON elements
  // of SE Kernel
  KernelIteratorType KernelBegin  = m_Kernel.Begin();
  KernelIteratorType KernelEnd    = m_Kernel.End();
  KernelIteratorType kernel_it;
  m_KernelOnElements.clear();
                
  for ( i=0, kernel_it=KernelBegin; kernel_it != KernelEnd; ++kernel_it, ++i)
    {
    if ((*kernel_it) > 0)
      {
      m_KernelOnElements.push_back(i);
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
  m_KernelCCVectors.clear();

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
      m_KernelCCVectors.push_back( offset );
                                
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
  typedef typename TInputImage::PixelType InputImagePixelType;
  Neighborhood< InputImagePixelType, TInputImage::ImageDimension> adjNeigh;
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
    for( kernelOnElementsIt = m_KernelOnElements.begin();
         kernelOnElementsIt != m_KernelOnElements.end(); ++kernelOnElementsIt )
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
            || currentShiftedPosition[dimCount] >= static_cast<typename IndexType::IndexValueType>(m_Kernel.GetSize(dimCount)) )
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
                                
      } // for( kernelOnElementsIt = m_KernelOnElements.begin(); ...
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

template< class TInputImage, class TOutputImage, class TKernel>
void
FastIncrementalBinaryDilateImageFilter< TInputImage, TOutputImage, TKernel>
::GenerateData( void )
{
  unsigned int i,j;
                                
  // Retrieve input and output pointers
  typename OutputImageType::Pointer output = this->GetOutput();
  typename InputImageType::ConstPointer input  = this->GetInput();
                
  // Create a temp image for surface encoding
  // The temp image size is equal to the output requested region
  // padded by connectivity neighborhood radius.
  typedef Image< unsigned char, TInputImage::ImageDimension >
    TempImageType;
  typename TempImageType::Pointer tmpImage = TempImageType::New();

  // Retrieve output requested region
  typename OutputImageType::RegionType outputRequestedRegion
    = output->GetRequestedRegion();
  typename TempImageType::RegionType tmpRequestedRegion = outputRequestedRegion;
                
  // Pad the tmp requested region by the neighborhood of connectivity
  // We do not have to do tests in order to check region validity.  In
  // fact if there were some problems, the default
  // GenerateInputRequestedRegion() function would have checked them.
  //
  tmpRequestedRegion.PadByRadius( m_Radius );
  tmpRequestedRegion.Crop(input->GetBufferedRegion());

  // Define regions of temp image
  tmpImage->SetRegions( tmpRequestedRegion );
                
  // Allocation.
  // Pay attention to the fact that here, the output is still not
  // allocated (so no extra memory needed for tmp image, if you
  // consider that you reserve som memory space for output)
  tmpImage->Allocate();
                
  // Iterators on input and tmp image
  ImageRegionConstIterator<TInputImage> iRegIt;   // iterator on input
  ImageRegionIterator<TempImageType> tmpRegIt;    // iterator on tmp image
  iRegIt = ImageRegionConstIterator<InputImageType>(input, tmpRequestedRegion);
  tmpRegIt = ImageRegionIterator<TempImageType>(tmpImage, tmpRequestedRegion);

  // Copy the input image to the tmp image.
  iRegIt.GoToBegin();
  tmpRegIt.GoToBegin();

  // Support progress methods/callbacks
  // Setup a progress reporter.  We have 4 stages to the algorithm so
  // pretend we have 4 times the number of pixels
  ProgressReporter progress(this, 0,
                            3 * outputRequestedRegion.GetNumberOfPixels()
                            + tmpRequestedRegion.GetNumberOfPixels() );

  // First Stage
  // Tag the tmp Image.
  //     zero means background
  //     one means pixel on but not treated
  //     two means border pixel
  //     three means inner pixel
  static const unsigned char backgroundTag  = 0;
  static const unsigned char onTag          = 1;
  static const unsigned char borderTag      = 2;                            
  static const unsigned char innerTag       = 3;
  
  while(!tmpRegIt.IsAtEnd())
    {
    OutputPixelType pxl = iRegIt.Get();
    if( pxl == m_DilateValue )
      {
      tmpRegIt.Set( onTag );    
      }
    else
      {
      // by default if it is not foreground, consider
      // it as background
      tmpRegIt.Set( backgroundTag ); 
      }
    ++tmpRegIt;
    ++iRegIt;
    progress.CompletedPixel();
    }


  // Second stage
  // Border tracking and encoding

  // Need to record index, use an iterator with index
  ImageRegionIteratorWithIndex<TempImageType> tmpRegIndexIt;
  
  // Define iterators that will traverse the OUTPUT requested region
  // and not the padded one. The tmp image has been padded because in
  // that way we will take care carefully at boundary pixels of output
  // requested region.  Take care means that we will check if a
  // boundary pixel is or not a border pixel.
  tmpRegIndexIt
    = ImageRegionIteratorWithIndex<TempImageType>( tmpImage,
                                                   outputRequestedRegion );
  tmpRegIndexIt.GoToBegin();

  ConstNeighborhoodIterator<TempImageType> oNeighbIt;
  oNeighbIt = ConstNeighborhoodIterator<TempImageType>( m_Radius, tmpImage,
                                                        outputRequestedRegion);
  oNeighbIt.GoToBegin();
  
  // Define boundaries conditions
  ConstantBoundaryCondition<TempImageType> cbc;
  cbc.SetConstant( backgroundTag );
  oNeighbIt.OverrideBoundaryCondition(&cbc);
  
  unsigned int neighborhoodSize       = oNeighbIt.Size();
  unsigned int centerPixelCode        = neighborhoodSize / 2;
  
  std::queue<IndexType> propagQueue;
  BorderCellContainer borderContainer;
  unsigned long numberOfBorderPixels  = 0;

  // Neighborhood iterators used to track the surface.
  //
  // Note the region specified for the first neighborhood iterator is
  // the requested region for the tmp image not the output image. This
  // is necessary because the NeighborhoodIterator relies on the
  // specified region to determine if you will ever query a boundary
  // condition pixel.  Since we call SetLocation on the neighbor of a
  // specified pixel, we have to set the region for the interator to
  // include any pixel we may set our location to.
  NeighborhoodIterator<TempImageType> nit
    = NeighborhoodIterator<TempImageType>( m_Radius, tmpImage,
                                           tmpRequestedRegion );
  nit.OverrideBoundaryCondition(&cbc);

  ConstNeighborhoodIterator<TempImageType> nnit
    = ConstNeighborhoodIterator<TempImageType>( m_Radius, tmpImage,
                                                tmpRequestedRegion);
  nnit.OverrideBoundaryCondition(&cbc);

  
  while( !tmpRegIndexIt.IsAtEnd() )
    {
    // Test current pixel: it is active ( on ) or not?
    if( tmpRegIndexIt.Get() == onTag )
      {
      // The current pixel has not been treated previously.  That
      // means that we do not know that it is an inner pixel of a
      // border pixel.
      
      // Test current pixel: it is a border pixel or an inner pixel?
      bool bIsOnContour = false;
      
      for (i = 0; i < neighborhoodSize; ++i)
        {
        // If at least one neighbour pixel is off the center pixel
        // belongs to contour
        if( oNeighbIt.GetPixel(i) == backgroundTag )
          {
          bIsOnContour = true;
          break;
          }
        }

      if( !bIsOnContour )
        {
        tmpRegIndexIt.Set( innerTag );
        }
      else
        {
        // center pixel is a border pixel and due to the parsing, it is also
        // a pixel which belongs to a new border connected component
        // Now we will parse this border thanks to a burn procedure
        
        // mark pixel value as a border pixel
        tmpRegIndexIt.Set( borderTag );
        
        // add it to border container.
        // its code is center pixel code because it is the first pixel
        // of the connected component border
        BorderCell cell;
        cell.index    = tmpRegIndexIt.GetIndex();
        cell.code     = centerPixelCode;
        borderContainer.push_back( cell );
        ++numberOfBorderPixels;
        
        // add it to queue
        propagQueue.push( tmpRegIndexIt.GetIndex() );
        
        // now find all the border pixels
        while ( !propagQueue.empty() )
          {
          // Extract pixel index from queue
          IndexType currentIndex = propagQueue.front();
          propagQueue.pop();
          
          nit.SetLocation( currentIndex );
          
          for (i = 0; i < neighborhoodSize; ++i)
            {
            // If pixel has not been already treated and it is a pixel
            // on, test if it is an inner pixel or a border pixel
            
            // Remark: all the pixels outside the image are set to
            // backgroundTag thanks to boundary conditions. That means that if
            // we enter in the next if-statement we are sure that the
            // current neighbour pixel is in the image
            if( nit.GetPixel( i ) == onTag )
              {
              // Check if it is an inner or border neighbour pixel
              // Get index of current neighbour pixel
              IndexType neighbIndex = nit.GetIndex( i );
              
              // Force location of neighbour iterator
              nnit.SetLocation( neighbIndex );

              bool bIsOnBorder = false;
              
              for( j = 0; j < neighborhoodSize; ++j)
                {
                // If at least one neighbour pixel is off the center
                // pixel belongs to border
                if( nnit.GetPixel(j) == backgroundTag )
                  {
                  bIsOnBorder = true;
                  break;
                  } 
                }
              
              
              if( !bIsOnBorder )
                {
                // neighbour pixel is an inner pixel
                bool status;
                nit.SetPixel( i, innerTag, status );
                }
              else
                {
                // neighbour pixel is a border pixel
                // mark it
                bool status;
                nit.SetPixel( i, borderTag, status );

                // check whether we could set the pixel.  can only set
                // the pixel if it is within the tmpimage
                if (status) 
                  {
                  // add it to queue
                  propagQueue.push( neighbIndex );
                  
                  // add the pixel index to border container
                  BorderCell celln;
                  celln.index = neighbIndex;
                  celln.code  = i;
                  borderContainer.push_back( celln );
                  ++numberOfBorderPixels;
                  }
                }
              
              } // if( nit.GetPixel( i ) == onTag )
            
            } // for (i = 0; i < neighborhoodSize; ++i)
          
          } // while ( !propagQueue.empty() )
        
        } // if( bIsOnCountour )
      
      } // if( tmpRegIndexIt.Get() == onTag )
    
    // Here, the pixel is a background pixel ( value at 0 ) or an
    // already treated pixel:
    //     2 for border pixel, 3 for inner pixel
    
    ++tmpRegIndexIt;
    ++oNeighbIt;
    progress.CompletedPixel();
    
    } // while( !tmpRegIndexIt.IsAtEnd() )
  
  
  // Deallocate tmpImage
  tmpImage->Initialize();
  
  // Allocate and reset output. We copy the input to the output,
  // except for pixels with DilateValue.  These pixels are initially
  // replaced with BackgroundValue and potentially replaced later with
  // DilateValue as the Minkowski sums are performed.
  this->AllocateOutputs();
  ImageRegionIterator<OutputImageType> outIt
    = ImageRegionIterator<OutputImageType>(this->GetOutput(),
                                     this->GetOutput()->GetRequestedRegion());
  ImageRegionConstIterator<InputImageType> inIt
    = ImageRegionConstIterator<InputImageType>(this->GetInput(),
                                     this->GetOutput()->GetRequestedRegion());
  InputPixelType value;
  while ( !outIt.IsAtEnd() )
    {
    value = inIt.Get();
    // replace foreground pixels with the default background
    if (value == m_DilateValue)
      {
      outIt.Set( m_BackgroundValue );
      }
    // keep all of the original background values intact
    else
      {
      outIt.Set( static_cast<OutputPixelType>(value) );
      }
    ++outIt;
    ++inIt;
    }
  
  
  // Third Stage
  // traverse structure of border and SE CCs, and paint output image
  
  // Let's consider the the set of the ON elements of the input image as X.
  // 
  // Let's consider the structuring element as B = {B0, B1, ..., Bn},
  // where Bi denotes a connected component of B.
  //
  // Let's consider bi, i in [0,n], an arbitrary point of Bi.
  //

  // We use hence the next property in order to compute minkoswki
  // addition ( which will be written (+) ):
  //
  // X (+) B = ( Xb0 UNION Xb1 UNION ... Xbn ) UNION ( BORDER(X) (+) B ),
  //
  // where Xbi is the set X translated with respect to vector bi :
  //
  // Xbi = { x + bi, x belongs to X } where BORDER(X) is the extracted
  // border of X ( 8 connectivity in 2D, 26 in 3D )
  
  // Define boundaries conditions
  ConstantBoundaryCondition<TOutputImage> obc;
  obc.SetConstant( m_BackgroundValue );
  
  NeighborhoodIterator<OutputImageType> onit;
  onit = NeighborhoodIterator<OutputImageType>( m_Kernel.GetRadius(), output,
                                                outputRequestedRegion );
  
  onit.OverrideBoundaryCondition(&obc);
  // Paint SE     --> "( BORDER(X) (+) B )"
  for( typename BorderCellContainer::iterator it = borderContainer.begin();
       it != borderContainer.end(); ++it )
    {
    // Retrieve pixel index
    BorderCell cell    = *it;
    IndexType index = cell.index;
    unsigned int code = cell.code;
    
    // Force location of neighbour iterator 
    onit.SetLocation( index );

    // Thanks to code, we know the exact index of the region to paint
    NeighborIndexContainer::const_iterator itIndex;
    NeighborIndexContainer& indexDifferenceSet
      = m_KernelDifferenceSets[code];
    
    bool bIsInBound;
    for( itIndex = indexDifferenceSet.begin(); itIndex != indexDifferenceSet.end(); ++itIndex )
      {
      onit.SetPixel( *itIndex, m_DilateValue, bIsInBound );
      }
    
    progress.CompletedPixel();
    }
  
  // Fake progress in order to complete the progress
  unsigned long numberOfOutputPixels
    = outputRequestedRegion.GetNumberOfPixels();
  for( i = numberOfBorderPixels; i < numberOfOutputPixels; ++i )
    {
    progress.CompletedPixel();
    }
                
  // Paint input image translated with respect to the SE CCs vectors
  // --> "( Xb0 UNION Xb1 UNION ... Xbn )"
  typename std::vector< OffsetType >::const_iterator vecIt;

  // iterator on input image
  ImageRegionConstIterator<InputImageType> inRegIt;
  inRegIt = ImageRegionConstIterator<InputImageType>(input,
                                                     outputRequestedRegion );
  // iterator on output image
  ImageRegionIteratorWithIndex<OutputImageType> ouRegIndexIt;
  ouRegIndexIt
    = ImageRegionIteratorWithIndex<OutputImageType>(output,
                                                    outputRequestedRegion );

  ouRegIndexIt.GoToBegin(); 
  inRegIt.GoToBegin(); 

  while( !ouRegIndexIt.IsAtEnd() )
    {
    // Translate only ON pixels of input image
    if( inRegIt.Get() == m_DilateValue )
      {
      IndexType currentIndex  = ouRegIndexIt.GetIndex();
      for( vecIt = m_KernelCCVectors.begin();
           vecIt != m_KernelCCVectors.end(); ++vecIt )
        {
        // Translate
        IndexType translatedIndex = currentIndex + *vecIt;
        
        // Paint only if index is in bounds
        if( outputRequestedRegion.IsInside( translatedIndex ) )
          {
          output->SetPixel( translatedIndex, m_DilateValue );
          }                                       
        }
      } // if( inRegIt.Get() == m_DilateValue )

    ++ouRegIndexIt;
    ++inRegIt;
    progress.CompletedPixel();
    }
  
}


/**
 * Standard "PrintSelf" method
 */
template <class TInputImage, class TOutput, class TKernel>
void
FastIncrementalBinaryDilateImageFilter<TInputImage, TOutput, TKernel>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Radius which defines connectivity neighborhood: " << m_Radius << std::endl;
  os << indent << "Kernel which defines structuring element: " << m_Kernel << std::endl;
  os << indent << "Dilate Value: " << m_DilateValue << std::endl;
  os << indent << "Background Value: " << m_BackgroundValue << std::endl;
}

} // end namespace itk

#endif
