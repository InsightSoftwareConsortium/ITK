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
#ifndef itkBinaryThinningImageFilter_hxx
#define itkBinaryThinningImageFilter_hxx

#include <iostream>

#include "itkBinaryThinningImageFilter.h"
#include "itkImageRegionIterator.h"
#include <vector>

namespace itk
{
/**
 *    Constructor
 */
template< typename TInputImage, typename TOutputImage >
BinaryThinningImageFilter< TInputImage, TOutputImage >
::BinaryThinningImageFilter()
{
  this->SetNumberOfRequiredOutputs(1);

  OutputImagePointer thinImage = OutputImageType::New();
  this->SetNthOutput( 0, thinImage.GetPointer() );
}

/**
 *  Return the thinning Image pointer
 */
template< typename TInputImage, typename TOutputImage >
typename BinaryThinningImageFilter<
  TInputImage, TOutputImage >::OutputImageType *
BinaryThinningImageFilter< TInputImage, TOutputImage >
::GetThinning(void)
{
  return dynamic_cast< OutputImageType * >(
           this->ProcessObject::GetOutput(0) );
}

/**
 *  Prepare data for computation
 *  Copy the input image to the output image, changing from the input
 *  type to the output type.
 */
template< typename TInputImage, typename TOutputImage >
void
BinaryThinningImageFilter< TInputImage, TOutputImage >
::PrepareData(void)
{
  itkDebugMacro(<< "PrepareData Start");
  OutputImagePointer thinImage = GetThinning();

  InputImagePointer inputImage  =
    dynamic_cast< const TInputImage  * >( ProcessObject::GetInput(0) );

  thinImage->SetBufferedRegion( thinImage->GetRequestedRegion() );
  thinImage->Allocate();

  typename OutputImageType::RegionType region  = thinImage->GetRequestedRegion();

  ImageRegionConstIterator< TInputImage > it(inputImage,  region);
  ImageRegionIterator< TOutputImage >     ot(thinImage,  region);

  it.GoToBegin();
  ot.GoToBegin();

  itkDebugMacro(<< "PrepareData: Copy input to output");

  // Copy the input to the output, changing all foreground pixels to
  // have value 1 in the process.
  while ( !ot.IsAtEnd() )
    {
    if ( it.Get() )
      {
      ot.Set(NumericTraits< OutputImagePixelType >::OneValue());
      }
    else
      {
      ot.Set(NumericTraits< OutputImagePixelType >::ZeroValue());
      }
    ++it;
    ++ot;
    }
  itkDebugMacro(<< "PrepareData End");
}

/**
 *  Post processing for computing thinning
 */
template< typename TInputImage, typename TOutputImage >
void
BinaryThinningImageFilter< TInputImage, TOutputImage >
::ComputeThinImage()
{
  itkDebugMacro(<< "ComputeThinImage Start");
  OutputImagePointer thinImage          =  GetThinning();

  typename OutputImageType::RegionType region  = thinImage->GetRequestedRegion();

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill(1);
  NeighborhoodIteratorType ot(radius, thinImage, region);

  // Create a set of offsets from the center.
  // This numbering follows that of Gonzalez and Woods.
  typedef typename NeighborhoodIteratorType::OffsetType OffsetType;
  OffsetType o2 = { { 0, -1 } };
  OffsetType o3 = { { 1, -1 } };
  OffsetType o4 = { { 1, 0 } };
  OffsetType o5 = { { 1, 1 } };
  OffsetType o6 = { { 0, 1 } };
  OffsetType o7 = { { -1, 1 } };
  OffsetType o8 = { { -1, 0 } };
  OffsetType o9 = { { -1, -1 } };

  PixelType p2;
  PixelType p3;
  PixelType p4;
  PixelType p5;
  PixelType p6;
  PixelType p7;
  PixelType p8;
  PixelType p9;

  // These tests correspond to the conditions listed in Gonzalez and Woods
  bool testA;
  bool testB;
  bool testC;
  bool testD;

  std::vector< IndexType > pixelsToDelete;
  typename std::vector< IndexType >::iterator pixelsToDeleteIt;

  // Loop through the image several times until there is no change.
  bool noChange = false;
  while ( !noChange )
    {
    noChange = true;
    // Loop through the thinning steps.
    for ( int step = 1; step <= 4; step++ )
      {
      pixelsToDelete.clear();
      // Loop through the image.
      for ( ot.GoToBegin(); !ot.IsAtEnd(); ++ot )
        {
        // Each iteration over the image, set all tests to false.
        testA = false;
        testB = false;
        testC = false;
        testD = false;

        p2 = ot.GetPixel(o2);
        p3 = ot.GetPixel(o3);
        p4 = ot.GetPixel(o4);
        p5 = ot.GetPixel(o5);
        p6 = ot.GetPixel(o6);
        p7 = ot.GetPixel(o7);
        p8 = ot.GetPixel(o8);
        p9 = ot.GetPixel(o9);

        // Determine whether the pixel should be deleted in the
        // following if statements.
        if ( ot.GetCenterPixel() )
          {
          // TestA
          // Count the number of neighbors that are on.
          // TestA is violated when contour point p1 has only one or
          // seven 8-neighbors valued 1.  Having only one such
          // neighbor implies that p1 is the end point of a skeleton
          // stroke and obviously should not be deleted.  Deleting p1
          // if it has seven such neighbos would cause erosion into a region.
          PixelType numberOfOnNeighbors = p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9;

          if ( numberOfOnNeighbors > 1 && numberOfOnNeighbors < 7 )
            {
            testA = true;
            }

          // TestB
          // Count the number of 0-1 transitions in the ordered
          // sequence.
          // TestB is violated when it is applied to points on a
          // stroke 1 pixel thick.  Hence this test prevents
          // disconnetion of segments of a skeleton during the
          // thinning operation.
          // First find the total number of transitions, and then
          // divide by 2.
          const PixelType transitions = (
            std::abs( static_cast< int >( p3
                                         - p2 ) )
            + std::abs( static_cast< int >( p4
                                           - p3 ) )
            + std::abs( static_cast< int >( p5 - p4 ) ) + std::abs( static_cast< int >( p6 - p5 ) )
            + std::abs( static_cast< int >( p7
                                           - p6 ) )
            + std::abs( static_cast< int >( p8
                                           - p7 ) )
            + std::abs( static_cast< int >( p9 - p8 ) ) + std::abs( static_cast< int >( p2 - p9 ) )
            ) / 2;

          if ( transitions == 1 )
            {
            testB = true;
            }

          // TestC and TestD
          // Step 1 in Gonzalez and Woods is broken up here into two
          // steps; step 1 and step 2.
          // Steps 1 and 2 are the first two passes over the image for each
          // iteration of the algorithm.
          // A point that satisfies these tests as well as TestA
          // and TestB is an east or south boundary point or a
          // northwest corner point in the boundary.
          // Note that northeast and southwest corner points are
          // satisfied in both the combination of steps 1 and 2 and
          // the combination of steps 3 and 4.
          if ( step == 1 )
            {
            if ( p4 == 0 || p6 == 0 )
              {
              testC = true;
              testD = true;
              }
            }

          else if ( step == 2 )
            {
            if ( p2 == 0 && p8 == 0 )
              {
              testC = true;
              testD = true;
              }
            }

          // Step 2 in Gonzalez and Woods is broken up here into two
          // steps; step 3 and step 4.
          // Steps 3 and 4 are the second passes over the image for each
          // iteration of the algorithm.
          // A point that satisfies these tests as well as TestA
          // and TestB is a west or north boundary point or a
          // southeast corner point in the boundary.
          // Note that northeast and southwest corner points are
          // satisfied in both the combination of steps 1 and 2 and
          // the combination of steps 3 and 4.
          else if ( step == 3 )
            {
            if ( p2 == 0 || p8 == 0 )
              {
              testC = true;
              testD = true;
              }
            }
          else if ( step == 4 )
            {
            if ( p4 == 0 && p6 == 0 )
              {
              testC = true;
              testD = true;
              }
            }

          // If all tests pass, mark the pixel for removal
          if ( testA && testB && testC && testD )
            {
            pixelsToDelete.push_back( ot.GetIndex() );
            noChange = false;
            }
          }
        } // end image iteration loop

      //Loop through the vector of pixels to delete and set these pixels to 0 in
      // the image.
      for ( pixelsToDeleteIt = pixelsToDelete.begin();
            pixelsToDeleteIt != pixelsToDelete.end();
            pixelsToDeleteIt++ )
        {
        thinImage->SetPixel(*pixelsToDeleteIt, 0);
        }
      } // end step loop
    }   // end noChange while loop

  itkDebugMacro(<< "ComputeThinImage End");
}

/**
 *  Generate ThinImage
 */
template< typename TInputImage, typename TOutputImage >
void
BinaryThinningImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  this->PrepareData();

  itkDebugMacro(<< "GenerateData: Computing Thinning Image");
  this->ComputeThinImage();
} // end GenerateData()
} // end namespace itk

#endif
