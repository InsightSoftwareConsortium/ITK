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
#ifndef itkHessianImageFilter_hxx
#define itkHessianImageFilter_hxx


#include "itkHessianImageFilter.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"

#include "itkProgressReporter.h"
#include "itkProgressAccumulator.h"

namespace itk
{

/**
 *  Constructor
 */

template <typename TInputImage, typename TOutputImage >
HessianImageFilter<TInputImage,TOutputImage>
::HessianImageFilter( void )
{
}

/**
 * Enlarge Input Requested Region
 */
template< class TInputImage, class TOutputImage >
void
HessianImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename InputImageType::Pointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );

  if ( !inputPtr )
    {
    return;
    }


  // the hessaion just needs a 1 radius neighborhood
  const unsigned int radius = 1;

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius( radius );

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() ) )
    {
    inputPtr->SetRequestedRegion(inputRequestedRegion);
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion(inputRequestedRegion);

    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

/**
 * Threaded Data Generation
 */
template <typename TInputImage, typename TOutputImage >
void
HessianImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       ThreadIdType threadId)
{
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels() );

  const TInputImage *input = this->GetInput();

  const unsigned int ImageDimension = TInputImage::ImageDimension;

  TOutputImage *output = this->GetOutput();


  typedef typename OutputImageType::PixelType HessianType;
  ImageRegionIterator<OutputImageType> oit;

  itk::Size<ImageDimension> radius;
  radius.Fill( 1 );
  unsigned long center;
  unsigned long stride[ImageDimension];

  typename TInputImage::SpacingType spacing = input->GetSpacing();


  // compute the boundary faces of our region
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage > bC;
  faceList = bC( input, outputRegionForThread, radius );

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >::FaceListType::iterator fit;

  typedef ConstNeighborhoodIterator< TInputImage > NeighborhoodType;

  // get center and dimension strides for iterator neighborhoods
  NeighborhoodType it( radius, input, *faceList.begin() );
  center = it.Size()/2;
  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    stride[i] = it.GetStride(i);
    }

  // process each of the region "faces"
  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    // set up the iterator for the "face" and let the automatic
    // boundary condition detection work as needed
    it = NeighborhoodType( radius, input, *fit);

    oit = ImageRegionIterator<OutputImageType>( output, *fit );

    while ( !it.IsAtEnd() )
      {
      // symetric hessian
      HessianType H;


      //Calculate 2nd order derivative on the diaganal
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        H(i,i) = it.GetPixel(center + stride[i]) + it.GetPixel(center - stride[i])
          - 2.0 * it.GetPixel(center);
        H(i,i) /= spacing[i] * spacing[i];
        }

      //Calculate the 2nd derivatives
      for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
        {
        for ( unsigned int j = i + 1; j < ImageDimension; j++ )
          {
          H(i,j) = ( it.GetPixel(center - stride[i] - stride[j])
                     - it.GetPixel(center - stride[i] + stride[j])
                     - it.GetPixel(center + stride[i] - stride[j])
                     + it.GetPixel(center + stride[i] + stride[j])
            ) / ( 4.0 * spacing[i] * spacing[j] );
          }
        }

      oit.Set( H );

      ++oit;
      ++it;
       progress.CompletedPixel();
      }
    }

}

} // end namespace itk

#endif // itkHessianImageFilter_hxx
