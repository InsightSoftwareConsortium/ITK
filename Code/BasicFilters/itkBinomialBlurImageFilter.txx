#ifndef __itkBinomialBlurImageFilter_txx
#define __itkBinomialBlurImageFilter_txx

#include <iostream>
#include "vnl/vnl_vector_fixed.h"
#include "itkSize.h"
#include "itkPixelTraits.h"
#include "itkImageRegion.h"
#include "itkReflectiveImageRegionIterator.h"
#include "itkBinomialBlurImageFilter.h"

namespace itk
{

template< class TInputImage, class TOutputImage >
BinomialBlurImageFilter< TInputImage, TOutputImage >
::BinomialBlurImageFilter()
{
  std::cout << "BinomialBlurImageFilter::BinomialBlurImageFilter() called\n";

  // The default is to just do one repetition
  m_Repetitions = 1;
}


template< class TInputImage, class TOutputImage >
void
BinomialBlurImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  std::cout << "BinomialBlurImageFilter::GenerateData() called\n";

  // Get the input and output pointers
  InputImagePointer  inputPtr = this->GetInput(0);
  OutputImagePointer outputPtr = this->GetOutput(0);

  // Make sure we're getting everything
  inputPtr->SetRequestedRegionToLargestPossibleRegion();

  // How big is the input image?
  TInputImage::SizeType size = inputPtr->GetLargestPossibleRegion().GetSize();

  // Create a region object native to the output image type
  OutputImageRegionType outputRegion;

  // Resize the output region
  outputRegion.SetSize( size );

  // Set the largest legal region size (i.e. the size of the whole image)
  // to what we just defined
  outputPtr->SetLargestPossibleRegion( outputRegion );
  outputPtr->SetBufferedRegion( outputRegion );
  outputPtr->SetRequestedRegion( outputRegion );
  outputPtr->Allocate();

  // Create a temporary image used while processing the image
  // Processing with doubles eliminates possible rounding artifacts which may
  // accumulate over repeated integer division
  typedef Image<double, NDimensions> TTempImage;
  TTempImage::Pointer tempPtr
    = TTempImage::New();
  TTempImage::RegionType tempRegion;
  tempRegion.SetSize( size );
  tempPtr->SetLargestPossibleRegion( tempRegion );
  tempPtr->SetBufferedRegion( tempRegion );
  tempPtr->SetRequestedRegion( tempRegion );
  tempPtr->Allocate();

  // How many pixels are in the image
  unsigned long int numPixels = 0;
  numPixels = size[0];

  for (int i = 1; i < NDimensions; i++)
    numPixels = numPixels * size[i];

  TTempImage::IndexType* reverseIteratorArray = new TTempImage::IndexType[numPixels];

  // Copy the input image to the temporary image and build
  // the reverseIteratorArray
  typedef ImageRegionIterator<TTempImage> tempIterator;

  tempIterator tempIt = tempIterator(tempPtr,
                                     tempPtr->GetRequestedRegion());
  
  int counter  = 0;

  for ( tempIt.Begin(); !tempIt.IsAtEnd(); ++tempIt)
    {
    counter++;

    typename TTempImage::IndexType index;
    index = tempIt.GetIndex();
    reverseIteratorArray[numPixels - counter] = index;
    tempIt.Set( (double) inputPtr->GetPixel(index).GetScalar() );
    }

  std::cout << "There are "<< numPixels << " pixels in the image\n";

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  typename TTempImage::IndexType index;
  typename TTempImage::IndexType indexShift;

  // How many times has the algorithm executed? (for debug)
  int num_reps = 0;

  // Temporary pixel storage
  double pixelA, pixelB;

  // walk the output image forwards and compute blur
  for (int rep = 0; rep < m_Repetitions; rep++)
    {
    num_reps++;

    std::cout << "Repetition # " << rep << "\n";

    // blur each dimension
    for (unsigned int dim = 0; dim < NDimensions; dim ++)
      {
      typedef ImageRegionIterator<TTempImage> tempIterator;

      tempIterator tempIt = tempIterator(tempPtr,
                                         tempPtr->GetRequestedRegion());

      tempIt.Begin();
  
      while( !tempIt.IsAtEnd() )
        {
        // determine the index of the output pixel
        index = tempIt.GetIndex();

        if (index[dim] < (size[dim] - 1))
          {
          // Figure out the location of the "neighbor" pixel
          for (int i = 0; i < NDimensions; i++)
            {
            if ( i == dim )
              indexShift.m_Index[i] = index.m_Index[i] + 1;
            else
              indexShift.m_Index[i] = index.m_Index[i];
            
            }

          // Average the pixel of interest and shifted pixel
          pixelA = tempPtr->GetPixel(index);
          pixelB = tempPtr->GetPixel(indexShift);

          pixelA += pixelB;
          pixelA = pixelA / 2;

          tempPtr->SetPixel(index, pixelA);

          }

        ++tempIt;
    
        } // end walk the image forwards
      
      std::cout << "End processing forward dimension " << dim << "\n";

      //----------------------Reverse pass----------------------
      for (unsigned long int arrayPos = 0; arrayPos < numPixels; arrayPos ++)
        {
        // determine the index of the output pixel
        index = reverseIteratorArray[arrayPos];

        if (index[dim] > 0)
          {    
          // Figure out the location of the "neighbor" pixel
          for (int i = 0; i < NDimensions; i++)
            {
            if ( i == dim )
              indexShift.m_Index[i] = index.m_Index[i] - 1;
            else
              indexShift.m_Index[i] = index.m_Index[i];
            }

          /*
          std::cout << "Dimension = " << dim << ", reverse pass\n";
          std::cout << "Index = (" << index[0] << "," << index[1] << ","
            << index[2] << ")\n";
          std::cout << "shiftIndex = (" << indexShift[0] << "," << indexShift[1] << ","
            << indexShift[2] << ")\n";
            */

          // Average the pixel of interest and shifted pixel
          pixelA = tempPtr->GetPixel(index);
          pixelB = tempPtr->GetPixel(indexShift);

          pixelA += pixelB;
          pixelA = pixelA / 2;

          tempPtr->SetPixel(index, pixelA);

          }
    
        } // end walk the image backwards
      
      std::cout << "End processing reverse dimension " << dim << "\n";

      } // end dimension loop

    } // end number of repetitions loop

  // Now, copy the temporary image to the output image
  typedef ImageRegionIterator<TOutputImage> OutputIterator;

  OutputIterator outIt = OutputIterator(outputPtr,
                                        outputPtr->GetRequestedRegion());

  for ( outIt.Begin(); !outIt.IsAtEnd(); ++outIt)
    {
    typename TOutputImage::IndexType index;
    index = outIt.GetIndex();

    outputPtr->GetPixel(index).SetScalar(tempPtr->GetPixel(index));
    }

  std::cout << "Binomial filter executed " << num_reps << " times\n";

  std::cout << "BinomialBlurImageFilter::GenerateData() finished\n";
}

} // end namespace

#endif
