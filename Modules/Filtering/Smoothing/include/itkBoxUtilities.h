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
#ifndef itkBoxUtilities_h
#define itkBoxUtilities_h

#include "itkProgressReporter.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkConstantBoundaryCondition.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkOffset.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Efficient implementation of kernel filtering"
 * by Beare R., Lehmann G
 * https://hdl.handle.net/1926/555
 * http://www.insight-journal.org/browse/publication/160
 *
 */

namespace
{

template< typename TIterator >
TIterator *
setConnectivityEarlyBox(TIterator *it, bool fullyConnected = false)
{
  // activate the "previous" neighbours
  typename TIterator::OffsetType offset;
  it->ClearActiveList();
  if ( !fullyConnected )
    {
    // only activate the neighbors that are face connected
    // to the current pixel. do not include the center pixel
    offset.Fill(0);
    for ( unsigned int d = 0; d < TIterator::Dimension; ++d )
      {
      offset[d] = -1;
      it->ActivateOffset(offset);
      offset[d] = 0;
      }
    }
  else
    {
    // activate all neighbors that are face+edge+vertex
    // connected to the current pixel. do not include the center pixel
    unsigned int centerIndex = it->GetCenterNeighborhoodIndex();
    for ( unsigned int d = 0; d < centerIndex; d++ )
      {
      offset = it->GetOffset(d);
      // check for positives in any dimension
      bool keep = true;
      for ( unsigned int i = 0; i < TIterator::Dimension; i++ )
        {
        if ( offset[i] > 0 )
          {
          keep = false;
          break;
          }
        }
      if ( keep )
        {
        it->ActivateOffset(offset);
        }
      }
    offset.Fill(0);
    it->DeactivateOffset(offset);
    }
  return it;
}

}

namespace itk
{

template< typename TInputImage, typename TOutputImage >
void
BoxAccumulateFunction(const TInputImage *inputImage,
                      const TOutputImage *outputImage,
                      typename TInputImage::RegionType inputRegion,
                      typename TOutputImage::RegionType outputRegion,
                      ProgressReporter & progress)
{
  // typedefs
  typedef TInputImage                      InputImageType;
  typedef typename TInputImage::OffsetType OffsetType;
  typedef TOutputImage                     OutputImageType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  typedef ImageRegionConstIterator< TInputImage > InputIterator;

  typedef ShapedNeighborhoodIterator< TOutputImage > NOutputIterator;
  InputIterator inIt(inputImage, inputRegion);
  typename TInputImage::SizeType kernelRadius;
  kernelRadius.Fill(1);

  NOutputIterator noutIt(kernelRadius, outputImage, outputRegion);
  // this iterator is fully connected
  setConnectivityEarlyBox(&noutIt, true);

  ConstantBoundaryCondition< OutputImageType > oBC;
  oBC.SetConstant(NumericTraits< OutputPixelType >::ZeroValue() );
  noutIt.OverrideBoundaryCondition(&oBC);
  // This uses several iterators. An alternative and probably better
  // approach would be to copy the input to the output and convolve
  // with the following weights (in 2D)
  //   -(dim - 1)  1
  //       1       1
  // The result of each convolution needs to get written back to the
  // image being convolved so that the accumulation propagates
  // This should be implementable with neighborhood operators.

  std::vector< int > weights;
  typename NOutputIterator::ConstIterator sIt;
  for ( typename NOutputIterator::IndexListType::const_iterator idxIt = noutIt.GetActiveIndexList().begin();
        idxIt != noutIt.GetActiveIndexList().end();
        idxIt++ )
    {
    OffsetType offset = noutIt.GetOffset(*idxIt);
    int        w = -1;
    for ( unsigned int k = 0; k < InputImageType::ImageDimension; k++ )
      {
      if ( offset[k] != 0 )
        {
        w *= offset[k];
        }
      }
//     std::cout << offset << "  " << w << std::endl;
    weights.push_back(w);
    }

  for ( inIt.GoToBegin(), noutIt.GoToBegin(); !noutIt.IsAtEnd(); ++inIt, ++noutIt )
    {
    OutputPixelType sum = 0;
    int             k;
    for ( k = 0, sIt = noutIt.Begin(); !sIt.IsAtEnd(); ++sIt, ++k )
      {
      sum += sIt.Get() * weights[k];
      }
    noutIt.SetCenterPixel( sum + inIt.Get() );
    progress.CompletedPixel();
    }
}

// a function to generate corners of arbitrary dimension box
template< typename TImage >
std::vector< typename TImage::OffsetType >
CornerOffsets(const TImage *im)
{
  typedef ShapedNeighborhoodIterator< TImage > NIterator;
  typename TImage::SizeType unitradius;
  unitradius.Fill(1);
  NIterator    n1( unitradius, im, im->GetRequestedRegion() );
  unsigned int centerIndex = n1.GetCenterNeighborhoodIndex();
  typename NIterator::OffsetType offset;
  std::vector< typename TImage::OffsetType > result;
  for ( unsigned int d = 0; d < centerIndex * 2 + 1; d++ )
    {
    offset = n1.GetOffset(d);
    // check whether this is a corner - corners have no zeros
    bool corner = true;
    for ( unsigned int k = 0; k < TImage::ImageDimension; k++ )
      {
      if ( offset[k] == 0 )
        {
        corner = false;
        break;
        }
      }
    if ( corner )
      {
      result.push_back(offset);
      }
    }
  return ( result );
}

template< typename TInputImage, typename TOutputImage >
void
BoxMeanCalculatorFunction(const TInputImage *accImage,
                          TOutputImage *outputImage,
                          typename TInputImage::RegionType inputRegion,
                          typename TOutputImage::RegionType outputRegion,
                          typename TInputImage::SizeType radius,
                          ProgressReporter & progress)
{
  // typedefs
  typedef TInputImage                           InputImageType;
  typedef typename TInputImage::RegionType      RegionType;
  typedef typename TInputImage::SizeType        SizeType;
  typedef typename TInputImage::IndexType       IndexType;
  typedef typename TInputImage::OffsetType      OffsetType;
  typedef TOutputImage                          OutputImageType;
  typedef typename TOutputImage::PixelType      OutputPixelType;
  // use the face generator for speed
  typedef NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType > FaceCalculatorType;
  typedef typename FaceCalculatorType::FaceListType                             FaceListType;
  typedef typename FaceCalculatorType::FaceListType::iterator                   FaceListTypeIt;
  FaceCalculatorType faceCalculator;

  FaceListType                                    faceList;
  FaceListTypeIt                                  fit;
  ZeroFluxNeumannBoundaryCondition< TInputImage > nbc;

  // this process is actually slightly asymmetric because we need to
  // subtract rectangles that are next to our kernel, not overlapping it
  SizeType kernelSize;
  SizeType internalRadius;
  SizeType regionLimit;

  IndexType regionStart = inputRegion.GetIndex();
  for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
    {
    kernelSize[i] = radius[i] * 2 + 1;
    internalRadius[i] = radius[i] + 1;
    regionLimit[i] = inputRegion.GetSize()[i] + regionStart[i] - 1;
    }

  typedef typename NumericTraits< OutputPixelType >::RealType AccPixType;
  // get a set of offsets to corners for a unit hypercube in this image
  std::vector< OffsetType > unitCorners = CornerOffsets< TInputImage >(accImage);
  std::vector< OffsetType > realCorners;
  std::vector< AccPixType > weights;
  // now compute the weights
  for ( unsigned int k = 0; k < unitCorners.size(); k++ )
    {
    int        prod = 1;
    OffsetType thisCorner;
    for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
      {
      prod *= unitCorners[k][i];
      if ( unitCorners[k][i] > 0 )
        {
        thisCorner[i] = radius[i];
        }
      else
        {
        thisCorner[i] = -( static_cast<OffsetValueType>(radius[i]) + 1 );
        }
      }
    weights.push_back( (AccPixType)prod );
    realCorners.push_back(thisCorner);
    }

  faceList = faceCalculator(accImage, outputRegion, internalRadius);
  // start with the body region
  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    if ( fit == faceList.begin() )
      {
      // this is the body region. This is meant to be an optimized
      // version that doesn't use neighborhood regions
      // compute the various offsets
      AccPixType pixelscount = 1;
      for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
        {
        pixelscount *= (AccPixType)( 2 * radius[i] + 1 );
        }

      typedef ImageRegionIterator< OutputImageType >     OutputIteratorType;
      typedef ImageRegionConstIterator< InputImageType > InputIteratorType;

      typedef std::vector< InputIteratorType > CornerItVecType;
      CornerItVecType cornerItVec;
      // set up the iterators for each corner
      for ( unsigned int k = 0; k < realCorners.size(); k++ )
        {
        typename InputImageType::RegionType tReg = ( *fit );
        tReg.SetIndex(tReg.GetIndex() + realCorners[k]);
        InputIteratorType tempIt(accImage, tReg);
        tempIt.GoToBegin();
        cornerItVec.push_back(tempIt);
        }
      // set up the output iterator
      OutputIteratorType oIt(outputImage, *fit);
      // now do the work
      for ( oIt.GoToBegin(); !oIt.IsAtEnd(); ++oIt )
        {
        AccPixType sum = 0;
        // check each corner
        for ( unsigned int k = 0; k < cornerItVec.size(); k++ )
          {
          sum += weights[k] * cornerItVec[k].Get();
          // increment each corner iterator
          ++( cornerItVec[k] );
          }
        oIt.Set( static_cast< OutputPixelType >( sum / pixelscount ) );
        progress.CompletedPixel();
        }
      }
    else
      {
      // now we need to deal with the border regions
      typedef ImageRegionIteratorWithIndex< OutputImageType > OutputIteratorType;
      OutputIteratorType oIt(outputImage, *fit);
      // now do the work
      for ( oIt.GoToBegin(); !oIt.IsAtEnd(); ++oIt )
        {
        // figure out the number of pixels in the box by creating an
        // equivalent region and cropping - this could probably be
        // included in the loop below.
        RegionType currentKernelRegion;
        currentKernelRegion.SetSize(kernelSize);
        // compute the region's index
        IndexType kernelRegionIdx = oIt.GetIndex();
        IndexType centIndex = kernelRegionIdx;
        for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
          {
          kernelRegionIdx[i] -= radius[i];
          }
        currentKernelRegion.SetIndex(kernelRegionIdx);
        currentKernelRegion.Crop(inputRegion);
        OffsetValueType edgepixelscount = currentKernelRegion.GetNumberOfPixels();
        AccPixType sum = 0;
        // rules are : for each corner,
        //               for each dimension
        //                  if dimension offset is positive -> this is
        //                  a leading edge. Crop if outside the input
        //                  region
        //                  if dimension offset is negative -> this is
        //                  a trailing edge. Ignore if it is outside
        //                  image region
        for ( unsigned int k = 0; k < realCorners.size(); k++ )
          {
          IndexType thisCorner = centIndex + realCorners[k];
          bool      includeCorner = true;
          for ( unsigned int j = 0; j < TInputImage::ImageDimension; j++ )
            {
            if ( unitCorners[k][j] > 0 )
              {
              // leading edge - crop it
              if ( thisCorner[j] > static_cast< OffsetValueType >( regionLimit[j] ) )
                {
                thisCorner[j] = static_cast< OffsetValueType >( regionLimit[j] );
                }
              }
            else
              {
              // trailing edge - check bounds
              if ( thisCorner[j] < regionStart[j] )
                {
                includeCorner = false;
                break;
                }
              }
            }
          if ( includeCorner )
            {
            sum += accImage->GetPixel(thisCorner) * weights[k];
            }
          }

        oIt.Set( static_cast< OutputPixelType >( sum / (AccPixType)edgepixelscount ) );
        progress.CompletedPixel();
        }
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
BoxSigmaCalculatorFunction(const TInputImage *accImage,
                           TOutputImage *outputImage,
                           typename TInputImage::RegionType inputRegion,
                           typename TOutputImage::RegionType outputRegion,
                           typename TInputImage::SizeType radius,
                           ProgressReporter & progress)
{
  // typedefs
  typedef TInputImage                           InputImageType;
  typedef typename TInputImage::RegionType      RegionType;
  typedef typename TInputImage::SizeType        SizeType;
  typedef typename TInputImage::IndexType       IndexType;
  typedef typename TInputImage::OffsetType      OffsetType;
  typedef TOutputImage                          OutputImageType;
  typedef typename TOutputImage::PixelType      OutputPixelType;
  typedef typename TInputImage::PixelType       InputPixelType;
  // use the face generator for speed
  typedef typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType > FaceCalculatorType;
  typedef typename FaceCalculatorType::FaceListType                                      FaceListType;
  typedef typename FaceCalculatorType::FaceListType::iterator                            FaceListTypeIt;
  FaceCalculatorType faceCalculator;

  FaceListType                                    faceList;
  FaceListTypeIt                                  fit;
  ZeroFluxNeumannBoundaryCondition< TInputImage > nbc;

  // this process is actually slightly asymmetric because we need to
  // subtract rectangles that are next to our kernel, not overlapping it
  SizeType  kernelSize;
  SizeType  internalRadius;
  SizeType  regionLimit;
  IndexType regionStart = inputRegion.GetIndex();
  for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
    {
    kernelSize[i] = radius[i] * 2 + 1;
    internalRadius[i] = radius[i] + 1;
    regionLimit[i] = inputRegion.GetSize()[i] + regionStart[i] - 1;
    }

  typedef typename NumericTraits< OutputPixelType >::RealType AccPixType;
  // get a set of offsets to corners for a unit hypercube in this image
  std::vector< OffsetType > unitCorners = CornerOffsets< TInputImage >(accImage);
  std::vector< OffsetType > realCorners;
  std::vector< AccPixType > weights;
  // now compute the weights
  for ( unsigned int k = 0; k < unitCorners.size(); k++ )
    {
    int        prod = 1;
    OffsetType thisCorner;
    for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
      {
      prod *= unitCorners[k][i];
      if ( unitCorners[k][i] > 0 )
        {
        thisCorner[i] = radius[i];
        }
      else
        {
        thisCorner[i] = -( static_cast<OffsetValueType>( radius[i] ) + 1 );
        }
      }
    weights.push_back( (AccPixType)prod );
    realCorners.push_back(thisCorner);
    }

  faceList = faceCalculator(accImage, outputRegion, internalRadius);
  // start with the body region
  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    if ( fit == faceList.begin() )
      {
      // this is the body region. This is meant to be an optimized
      // version that doesn't use neighborhood regions
      // compute the various offsets
      AccPixType pixelscount = 1;
      for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
        {
        pixelscount *= (AccPixType)( 2 * radius[i] + 1 );
        }

      typedef ImageRegionIterator< OutputImageType >     OutputIteratorType;
      typedef ImageRegionConstIterator< InputImageType > InputIteratorType;

      typedef std::vector< InputIteratorType > CornerItVecType;
      CornerItVecType cornerItVec;
      // set up the iterators for each corner
      for ( unsigned int k = 0; k < realCorners.size(); k++ )
        {
        typename InputImageType::RegionType tReg = ( *fit );
        tReg.SetIndex(tReg.GetIndex() + realCorners[k]);
        InputIteratorType tempIt(accImage, tReg);
        tempIt.GoToBegin();
        cornerItVec.push_back(tempIt);
        }
      // set up the output iterator
      OutputIteratorType oIt(outputImage, *fit);
      // now do the work
      for ( oIt.GoToBegin(); !oIt.IsAtEnd(); ++oIt )
        {
        AccPixType sum = 0;
        AccPixType squareSum = 0;
        // check each corner
        for ( unsigned int k = 0; k < cornerItVec.size(); k++ )
          {
          const InputPixelType & i = cornerItVec[k].Get();
          sum += weights[k] * i[0];
          squareSum += weights[k] * i[1];
          // increment each corner iterator
          ++( cornerItVec[k] );
          }

        oIt.Set( static_cast< OutputPixelType >( std::sqrt( ( squareSum - sum * sum / pixelscount ) / ( pixelscount - 1 ) ) ) );
        progress.CompletedPixel();
        }
      }
    else
      {
      // now we need to deal with the border regions
      typedef ImageRegionIteratorWithIndex< OutputImageType > OutputIteratorType;
      OutputIteratorType oIt(outputImage, *fit);
      // now do the work
      for ( oIt.GoToBegin(); !oIt.IsAtEnd(); ++oIt )
        {
        // figure out the number of pixels in the box by creating an
        // equivalent region and cropping - this could probably be
        // included in the loop below.
        RegionType currentKernelRegion;
        currentKernelRegion.SetSize(kernelSize);
        // compute the region's index
        IndexType kernelRegionIdx = oIt.GetIndex();
        IndexType centIndex = kernelRegionIdx;
        for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
          {
          kernelRegionIdx[i] -= radius[i];
          }
        currentKernelRegion.SetIndex(kernelRegionIdx);
        currentKernelRegion.Crop(inputRegion);
        SizeValueType edgepixelscount = currentKernelRegion.GetNumberOfPixels();
        AccPixType sum = 0;
        AccPixType squareSum = 0;
        // rules are : for each corner,
        //               for each dimension
        //                  if dimension offset is positive -> this is
        //                  a leading edge. Crop if outside the input
        //                  region
        //                  if dimension offset is negative -> this is
        //                  a trailing edge. Ignore if it is outside
        //                  image region
        for ( unsigned int k = 0; k < realCorners.size(); k++ )
          {
          IndexType thisCorner = centIndex + realCorners[k];
          bool      includeCorner = true;
          for ( unsigned int j = 0; j < TInputImage::ImageDimension; j++ )
            {
            if ( unitCorners[k][j] > 0 )
              {
              // leading edge - crop it
              if ( thisCorner[j] > static_cast< OffsetValueType >( regionLimit[j] ) )
                {
                thisCorner[j] = static_cast< OffsetValueType >( regionLimit[j] );
                }
              }
            else
              {
              // trailing edge - check bounds
              if ( thisCorner[j] < regionStart[j] )
                {
                includeCorner = false;
                break;
                }
              }
            }
          if ( includeCorner )
            {
            const InputPixelType & i = accImage->GetPixel(thisCorner);
            sum += weights[k] * i[0];
            squareSum += weights[k] * i[1];
            }
          }

        oIt.Set( static_cast< OutputPixelType >( std::sqrt( ( squareSum - sum * sum
                                                             / edgepixelscount ) / ( edgepixelscount - 1 ) ) ) );
        progress.CompletedPixel();
        }
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
BoxSquareAccumulateFunction(const TInputImage *inputImage,
                            TOutputImage *outputImage,
                            typename TInputImage::RegionType inputRegion,
                            typename TOutputImage::RegionType outputRegion,
                            ProgressReporter & progress)
{
  // typedefs
  typedef TInputImage                         InputImageType;
  typedef typename TInputImage::OffsetType    OffsetType;
  typedef TOutputImage                        OutputImageType;
  typedef typename TOutputImage::PixelType    OutputPixelType;
  typedef typename OutputPixelType::ValueType ValueType;
  typedef typename TInputImage::PixelType     InputPixelType;

  typedef ImageRegionConstIterator< TInputImage > InputIterator;

  typedef ShapedNeighborhoodIterator< TOutputImage > NOutputIterator;
  InputIterator inIt(inputImage, inputRegion);
  typename TInputImage::SizeType kernelRadius;
  kernelRadius.Fill(1);

  NOutputIterator noutIt(kernelRadius, outputImage, outputRegion);
  // this iterator is fully connected
  setConnectivityEarlyBox(&noutIt, true);

  ConstantBoundaryCondition< OutputImageType > oBC;
  oBC.SetConstant(NumericTraits< OutputPixelType >::ZeroValue() );
  noutIt.OverrideBoundaryCondition(&oBC);
  // This uses several iterators. An alternative and probably better
  // approach would be to copy the input to the output and convolve
  // with the following weights (in 2D)
  //   -(dim - 1)  1
  //       1       1
  // The result of each convolution needs to get written back to the
  // image being convolved so that the accumulation propagates
  // This should be implementable with neighborhood operators.

  std::vector< int > weights;
  typename NOutputIterator::ConstIterator sIt;
  for ( typename NOutputIterator::IndexListType::const_iterator idxIt = noutIt.GetActiveIndexList().begin();
        idxIt != noutIt.GetActiveIndexList().end();
        idxIt++ )
    {
    OffsetType offset = noutIt.GetOffset(*idxIt);
    int        w = -1;
    for ( unsigned int k = 0; k < InputImageType::ImageDimension; k++ )
      {
      if ( offset[k] != 0 )
        {
        w *= offset[k];
        }
      }
    weights.push_back(w);
    }

  for ( inIt.GoToBegin(), noutIt.GoToBegin(); !noutIt.IsAtEnd(); ++inIt, ++noutIt )
    {
    ValueType sum = 0;
    ValueType squareSum = 0;
    int       k;
    for ( k = 0, sIt = noutIt.Begin(); !sIt.IsAtEnd(); ++sIt, ++k )
      {
      const OutputPixelType & v = sIt.Get();
      sum += v[0] * weights[k];
      squareSum += v[1] * weights[k];
      }
    OutputPixelType        o;
    const InputPixelType & i = inIt.Get();
    o[0] = sum + i;
    o[1] = squareSum + i * i;
    noutIt.SetCenterPixel(o);
    progress.CompletedPixel();
    }
}
} //namespace itk

#endif
