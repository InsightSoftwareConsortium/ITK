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
#ifndef itkMorphologicalWatershedFromMarkersImageFilter_hxx
#define itkMorphologicalWatershedFromMarkersImageFilter_hxx

#include <algorithm>
#include <queue>
#include <list>
#include "itkMorphologicalWatershedFromMarkersImageFilter.h"
#include "itkProgressReporter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkConstantBoundaryCondition.h"
#include "itkSize.h"
#include "itkConnectedComponentAlgorithm.h"

namespace itk
{

template< typename TInputImage, typename TLabelImage >
MorphologicalWatershedFromMarkersImageFilter< TInputImage, TLabelImage >
::MorphologicalWatershedFromMarkersImageFilter():
  m_FullyConnected( false ),
  m_MarkWatershedLine( true )

{
  this->SetNumberOfRequiredInputs(2);
}


template< typename TInputImage, typename TLabelImage >
void
MorphologicalWatershedFromMarkersImageFilter< TInputImage, TLabelImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the inputs
  LabelImageType * marker =
    const_cast< LabelImageType * >( this->GetMarkerImage() );

  InputImageType * input =
    const_cast< InputImageType * >( this->GetInput() );

  if ( !marker || !input )
    {
    return;
    }

  // We need to
  // configure the inputs such that all the data is available.
  //
  marker->SetRequestedRegion( marker->GetLargestPossibleRegion() );
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}


template< typename TInputImage, typename TLabelImage >
void
MorphologicalWatershedFromMarkersImageFilter< TInputImage, TLabelImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  LabelImageType * output = this->GetOutput();
  output->SetRequestedRegion( output->GetLargestPossibleRegion() );
}


template< typename TInputImage, typename TLabelImage >
void
MorphologicalWatershedFromMarkersImageFilter< TInputImage, TLabelImage >
::GenerateData()
{
  // there is 2 possible cases: with or without watershed lines.
  // the algorithm with watershed lines is from Meyer
  // the algorithm without watershed lines is from beucher
  // The 2 algorithms are very similar and so are integrated in the same filter.

  //---------------------------------------------------------------------------
  // declare the vars common to the 2 algorithms: constants, iterators,
  // hierarchical queue, progress reporter, and status image
  // also allocate output images and verify preconditions
  //---------------------------------------------------------------------------

  // the label used to find background in the marker image
  static const LabelImagePixelType bgLabel =
    NumericTraits< LabelImagePixelType >::ZeroValue();
  // the label used to mark the watershed line in the output image
  static const LabelImagePixelType wsLabel =
    NumericTraits< LabelImagePixelType >::ZeroValue();

  this->AllocateOutputs();

  const LabelImageType * markerImage = this->GetMarkerImage();
  const InputImageType * inputImage = this->GetInput();
  LabelImageType * outputImage = this->GetOutput();

  // Set up the progress reporter
  // we can't found the exact number of pixel to process in the 2nd pass, so we
  // use the maximum number possible.
  ProgressReporter progress(this, 0, markerImage->GetRequestedRegion().GetNumberOfPixels() * 2);

  // mask and marker must have the same size
  if ( markerImage->GetRequestedRegion().GetSize() != inputImage->GetRequestedRegion().GetSize() )
    {
    itkExceptionMacro(<< "Marker and input must have the same size.");
    }

  // FAH (in french: File d'Attente Hierarchique)
  typedef std::queue< IndexType >                    QueueType;
  typedef std::map< InputImagePixelType, QueueType > MapType;
  MapType fah;

  // the radius which will be used for all the shaped iterators
  Size< ImageDimension > radius;
  radius.Fill(1);

  // iterator for the marker image
  typedef ConstShapedNeighborhoodIterator< LabelImageType > MarkerIteratorType;
  typename MarkerIteratorType::ConstIterator nmIt;
  MarkerIteratorType
  markerIt( radius, markerImage, markerImage->GetRequestedRegion() );
  // add a boundary constant to avoid adding pixels on the border in the fah
  ConstantBoundaryCondition< LabelImageType > lcbc;
  lcbc.SetConstant( NumericTraits< LabelImagePixelType >::max() );
  markerIt.OverrideBoundaryCondition(&lcbc);
  setConnectivity(&markerIt, m_FullyConnected);

  // iterator for the input image
  typedef ConstShapedNeighborhoodIterator< InputImageType > InputIteratorType;
  InputIteratorType
  inputIt( radius, inputImage, inputImage->GetRequestedRegion() );
  typename InputIteratorType::ConstIterator niIt;
  setConnectivity(&inputIt, m_FullyConnected);

  // iterator for the output image
  typedef ShapedNeighborhoodIterator< LabelImageType > OutputIteratorType;
  typedef typename OutputIteratorType::OffsetType      OffsetType;
  typename OutputIteratorType::Iterator noIt;
  OutputIteratorType
  outputIt( radius, outputImage, outputImage->GetRequestedRegion() );
  setConnectivity(&outputIt, m_FullyConnected);

  //---------------------------------------------------------------------------
  // Meyer's algorithm
  //---------------------------------------------------------------------------
  if ( m_MarkWatershedLine )
    {
    // first stage:
    //  - set markers pixels to already processed status
    //  - copy markers pixels to output image
    //  - init FAH with indexes of background pixels with marker pixel(s) in
    //    their neighborhood

    ConstantBoundaryCondition< LabelImageType > lcbc2;
    // outside pixel are watershed so they won't be use to find real watershed
    // pixels
    lcbc2.SetConstant(wsLabel);
    outputIt.OverrideBoundaryCondition(&lcbc2);

    // create a temporary image to store the state of each pixel (processed or
    // not)
    typedef Image< bool, ImageDimension > StatusImageType;
    typename StatusImageType::Pointer statusImage = StatusImageType::New();
    statusImage->SetRegions( markerImage->GetLargestPossibleRegion() );
    statusImage->Allocate();

    // iterator for the status image
    typedef ShapedNeighborhoodIterator< StatusImageType > StatusIteratorType;
    typename StatusIteratorType::Iterator nsIt;
    StatusIteratorType
                                                 statusIt( radius, statusImage, outputImage->GetRequestedRegion() );
    ConstantBoundaryCondition< StatusImageType > bcbc;
    bcbc.SetConstant(true);    // outside pixel are already processed
    statusIt.OverrideBoundaryCondition(&bcbc);
    setConnectivity(&statusIt, m_FullyConnected);

    // the status image must be initialized before the first stage. In the
    // first stage, the set to true are the neighbors of the marker (and the
    // marker) so it's difficult (impossible ?) to init the status image at
    // the same time
    // the overhead should be small
    statusImage->FillBuffer(false);

    for ( markerIt.GoToBegin(), statusIt.GoToBegin(), outputIt.GoToBegin(), inputIt.GoToBegin();
          !markerIt.IsAtEnd();
          ++markerIt, ++outputIt )
      {
      LabelImagePixelType markerPixel = markerIt.GetCenterPixel();
      if ( markerPixel != bgLabel )
        {
        IndexType idx = markerIt.GetIndex();

        // move the iterators to the right place
        OffsetType shift = idx - statusIt.GetIndex();
        statusIt += shift;
        inputIt += shift;

        // this pixel belongs to a marker
        // mark it as already processed
        statusIt.SetCenterPixel(true);
        // copy it to the output image
        outputIt.SetCenterPixel(markerPixel);
        // and increase progress because this pixel will not be used in the
        // flooding stage.
        progress.CompletedPixel();

        // search the background pixels in the neighborhood
        for ( nmIt = markerIt.Begin(), nsIt = statusIt.Begin(), niIt = inputIt.Begin();
              nmIt != markerIt.End();
              nmIt++, nsIt++, niIt++ )
          {
          if ( !nsIt.Get() && nmIt.Get() == bgLabel )
            {
            // this neighbor is a background pixel and is not already
            // processed; add its index to fah
            fah[niIt.Get()].push( markerIt.GetIndex()
                                  + nmIt.GetNeighborhoodOffset() );
            // mark it as already in the fah to avoid adding it several times
            nsIt.Set(true);
            }
          }
        }
      else
        {
        // Some pixels may be never processed so, by default, non marked pixels
        // must be marked as watershed
        outputIt.SetCenterPixel(wsLabel);
        }
      // one more pixel done in the init stage
      progress.CompletedPixel();
      }
    // fill the borders of the status image with "true"
    //FillSides<StatusImageType>(statusImage, true);
    // Now disable the boundary checks
    //outputIt.NeedToUseBoundaryConditionOff();
    //statusIt.NeedToUseBoundaryConditionOff();
    //inputIt.NeedToUseBoundaryConditionOff();
    // end of init stage

    // flooding
    // init all the iterators
    outputIt.GoToBegin();
    statusIt.GoToBegin();
    inputIt.GoToBegin();

    // and start flooding
    while ( !fah.empty() )
      {
      // store the current vars
      InputImagePixelType currentValue = fah.begin()->first;
      QueueType           currentQueue = fah.begin()->second;
      // and remove them from the fah
      fah.erase( fah.begin() );

      while ( !currentQueue.empty() )
        {
        IndexType idx = currentQueue.front();
        currentQueue.pop();

        // move the iterators to the right place
        OffsetType shift = idx - outputIt.GetIndex();
        outputIt += shift;
        statusIt += shift;
        inputIt += shift;

        // iterate over the neighbors. If there is only one marker value, give
        // that value to the pixel, else keep it as is (watershed line)
        LabelImagePixelType marker = wsLabel;
        bool                collision = false;
        for ( noIt = outputIt.Begin(); noIt != outputIt.End(); noIt++ )
          {
          LabelImagePixelType o = noIt.Get();
          if ( o != wsLabel )
            {
            if ( marker != wsLabel && o != marker )
              {
              collision = true;
              break;
              }
            else
                  { marker = o; }
            }
          }
        if ( !collision )
          {
          // set the marker value
          outputIt.SetCenterPixel(marker);
          // and propagate to the neighbors
          for ( niIt = inputIt.Begin(), nsIt = statusIt.Begin();
                niIt != inputIt.End();
                niIt++, nsIt++ )
            {
            if ( !nsIt.Get() )
              {
              // the pixel is not yet processed. add it to the fah
              InputImagePixelType GrayVal = niIt.Get();
              if ( GrayVal <= currentValue )
                {
                currentQueue.push( inputIt.GetIndex()
                                   + niIt.GetNeighborhoodOffset() );
                }
              else
                {
                fah[GrayVal].push( inputIt.GetIndex()
                                   + niIt.GetNeighborhoodOffset() );
                }
              // mark it as already in the fah
              nsIt.Set(true);
              }
            }
          }
        // one more pixel in the flooding stage
        progress.CompletedPixel();
        }
      }
    }

  //---------------------------------------------------------------------------
  // Beucher's algorithm
  //---------------------------------------------------------------------------
  else
    {
    // first stage:
    //  - copy markers pixels to output image
    //  - init FAH with indexes of pixels with background pixel in their
    //    neighborhood

    ConstantBoundaryCondition< LabelImageType > lcbc2;
    // outside pixel are watershed so they won't be use to find real watershed
    // pixels
    lcbc2.SetConstant( NumericTraits< LabelImagePixelType >::max() );
    outputIt.OverrideBoundaryCondition(&lcbc2);

    for ( markerIt.GoToBegin(), outputIt.GoToBegin(), inputIt.GoToBegin();
          !markerIt.IsAtEnd();
          ++markerIt, ++outputIt )
      {
      LabelImagePixelType markerPixel = markerIt.GetCenterPixel();
      if ( markerPixel != bgLabel )
        {
        IndexType  idx = markerIt.GetIndex();
        OffsetType shift = idx - inputIt.GetIndex();
        inputIt += shift;

        // this pixels belongs to a marker
        // copy it to the output image
        outputIt.SetCenterPixel(markerPixel);
        // search if it has background pixel in its neighborhood
        bool haveBgNeighbor = false;
        for ( nmIt = markerIt.Begin(); nmIt != markerIt.End(); nmIt++ )
          {
          if ( nmIt.Get() == bgLabel )
            {
            haveBgNeighbor = true;
            break;
            }
          }
        if ( haveBgNeighbor )
          {
          // there is a background pixel in the neighborhood; add to fah
          fah[inputIt.GetCenterPixel()].push( markerIt.GetIndex() );
          }
        else
          {
          // increase progress because this pixel will not be used in the
          // flooding stage.
          progress.CompletedPixel();
          }
        }
      else
        {
        outputIt.SetCenterPixel(wsLabel);
        }
      progress.CompletedPixel();
      }
    // end of init stage

    // flooding
    // init all the iterators
    outputIt.GoToBegin();
    inputIt.GoToBegin();

    // and start flooding
    while ( !fah.empty() )
      {
      // store the current vars
      InputImagePixelType currentValue = fah.begin()->first;
      QueueType           currentQueue = fah.begin()->second;
      // and remove them from the fah
      fah.erase( fah.begin() );

      while ( !currentQueue.empty() )
        {
        IndexType idx = currentQueue.front();
        currentQueue.pop();

        // move the iterators to the right place
        OffsetType shift = idx - outputIt.GetIndex();
        outputIt += shift;
        inputIt += shift;

        LabelImagePixelType currentMarker = outputIt.GetCenterPixel();
        // get the current value of the pixel
        // iterate over neighbors to propagate the marker
        for ( noIt = outputIt.Begin(), niIt = inputIt.Begin();
              noIt != outputIt.End();
              noIt++, niIt++ )
          {
          if ( noIt.Get() == wsLabel )
            {
            // the pixel is not yet processed. It can be labeled with the
            // current label
            noIt.Set(currentMarker);
            InputImagePixelType GrayVal = niIt.Get();
            if ( GrayVal <= currentValue )
              {
              currentQueue.push( inputIt.GetIndex()
                                 + noIt.GetNeighborhoodOffset() );
              }
            else
              {
              fah[GrayVal].push( inputIt.GetIndex()
                                 + noIt.GetNeighborhoodOffset() );
              }
            progress.CompletedPixel();
            }
          }
        }
      }
    }
}


template< typename TInputImage, typename TLabelImage >
void
MorphologicalWatershedFromMarkersImageFilter< TInputImage, TLabelImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "MarkWatershedLine: "  << m_MarkWatershedLine << std::endl;
}

} // end namespace itk
#endif
