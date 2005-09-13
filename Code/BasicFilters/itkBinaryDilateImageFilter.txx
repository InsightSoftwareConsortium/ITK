/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryDilateImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBinaryDilateImageFilter_txx
#define _itkBinaryDilateImageFilter_txx

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
#include "itkBinaryDilateImageFilter.h"

namespace itk
{
        
template <class TInputImage, class TOutputImage, class TKernel>
BinaryDilateImageFilter<TInputImage, TOutputImage, TKernel>
::BinaryDilateImageFilter()
{
}


template< class TInputImage, class TOutputImage, class TKernel>
void
BinaryDilateImageFilter< TInputImage, TOutputImage, TKernel>
::GenerateData( void )
{
  unsigned int i,j;
                                
  // Retrieve input and output pointers
  typename OutputImageType::Pointer output = this->GetOutput();
  typename InputImageType::ConstPointer input  = this->GetInput();
                
  // Get values from superclass
  InputPixelType foregroundValue = this->GetForegroundValue();
  InputPixelType backgroundValue = this->GetBackgroundValue();
  KernelType kernel = this->GetKernel();
  InputSizeType radius = this->GetRadius();
  
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
  tmpRequestedRegion.PadByRadius( radius );
  tmpRequestedRegion.Crop(input->GetBufferedRegion());

  // Define regions of temp image
  tmpImage->SetRegions( tmpRequestedRegion );
                
  // Allocation.
  // Pay attention to the fact that here, the output is still not
  // allocated (so no extra memory needed for tmp image, if you
  // consider that you reserve som memory space for output)
  tmpImage->Allocate();
  
  // Support progress methods/callbacks
  // Setup a progress reporter.  We have 4 stages to the algorithm so
  // pretend we have 4 times the number of pixels
  ProgressReporter progress(this, 0,
                            3 * outputRequestedRegion.GetNumberOfPixels()
                            + tmpRequestedRegion.GetNumberOfPixels() );

  // Iterators on input and tmp image
  ImageRegionConstIterator<TInputImage> iRegIt;   // iterator on input
  ImageRegionIterator<TempImageType> tmpRegIt;    // iterator on tmp image
  iRegIt = ImageRegionConstIterator<InputImageType>(input, tmpRequestedRegion);
  tmpRegIt = ImageRegionIterator<TempImageType>(tmpImage, tmpRequestedRegion);

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
  
  // Copy the input image to the tmp image.
  iRegIt.GoToBegin();
  tmpRegIt.GoToBegin();

  while(!tmpRegIt.IsAtEnd())
    {
    OutputPixelType pxl = iRegIt.Get();
    if( pxl == foregroundValue )
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
  oNeighbIt = ConstNeighborhoodIterator<TempImageType>( radius, tmpImage,
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
    = NeighborhoodIterator<TempImageType>( radius, tmpImage,
                                           tmpRequestedRegion );
  nit.OverrideBoundaryCondition(&cbc);

  ConstNeighborhoodIterator<TempImageType> nnit
    = ConstNeighborhoodIterator<TempImageType>( radius, tmpImage,
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
    if (value == foregroundValue)
      {
      outIt.Set( backgroundValue );
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
  obc.SetConstant( backgroundValue );
  
  NeighborhoodIterator<OutputImageType> onit;
  onit = NeighborhoodIterator<OutputImageType>( kernel.GetRadius(), output,
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
    typename NeighborIndexContainer::const_iterator itIndex;
    NeighborIndexContainer& indexDifferenceSet
      = this->GetDifferenceSet(code);
    
    bool bIsInBound;
    for( itIndex = indexDifferenceSet.begin(); itIndex != indexDifferenceSet.end(); ++itIndex )
      {
      onit.SetPixel( *itIndex, foregroundValue, bIsInBound );
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
  typename Superclass::ComponentVectorConstIterator vecIt;
  typename Superclass::ComponentVectorConstIterator vecBeginIt;
  typename Superclass::ComponentVectorConstIterator vecEndIt;

  vecBeginIt = this->KernelCCVectorBegin();
  vecEndIt = this->KernelCCVectorEnd();
  
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
    if( inRegIt.Get() == foregroundValue )
      {
      IndexType currentIndex  = ouRegIndexIt.GetIndex();
      for( vecIt = vecBeginIt; vecIt != vecEndIt; ++vecIt )
        {
        // Translate
        IndexType translatedIndex = currentIndex + *vecIt;
        
        // Paint only if index is in bounds
        if( outputRequestedRegion.IsInside( translatedIndex ) )
          {
          output->SetPixel( translatedIndex, foregroundValue );
          }                                       
        }
      } // if( inRegIt.Get() == foregroundValue )

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
BinaryDilateImageFilter<TInputImage, TOutput, TKernel>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Dilate Value: " << this->GetForegroundValue() << std::endl;
}

} // end namespace itk

#endif
