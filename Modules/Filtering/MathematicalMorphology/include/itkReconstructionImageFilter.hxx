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
#ifndef itkReconstructionImageFilter_hxx
#define itkReconstructionImageFilter_hxx

#include "itkMath.h"
#include "itkReconstructionImageFilter.h"
#include "itkConstantBoundaryCondition.h"
#include "itkConnectedComponentAlgorithm.h"

#include "itkConstantPadImageFilter.h"
#include "itkCropImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TCompare >
ReconstructionImageFilter< TInputImage, TOutputImage, TCompare >
::ReconstructionImageFilter()
{
  m_FullyConnected = false;
  m_UseInternalCopy = true;
}

template< typename TInputImage, typename TOutputImage, typename TCompare >
void
ReconstructionImageFilter< TInputImage, TOutputImage, TCompare >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the inputs
  MarkerImagePointer markerPtr =
    const_cast< MarkerImageType * >( this->GetInput(0) );

  MaskImagePointer maskPtr =
    const_cast< MaskImageType * >( this->GetInput(1) );

  if ( !markerPtr || !maskPtr )
    {
    return;
    }
  markerPtr->SetRequestedRegion( markerPtr->GetLargestPossibleRegion() );
  maskPtr->SetRequestedRegion( maskPtr->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage, typename TCompare >
void
ReconstructionImageFilter< TInputImage, TOutputImage, TCompare >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage, typename TCompare >
void
ReconstructionImageFilter< TInputImage, TOutputImage, TCompare >
::SetMarkerImage(const MarkerImageType *markerImage)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput( 0, const_cast< MarkerImageType * >( markerImage ) );
}

template< typename TInputImage, typename TOutputImage, typename TCompare >
const typename ReconstructionImageFilter< TInputImage, TOutputImage, TCompare >::MarkerImageType *
ReconstructionImageFilter< TInputImage, TOutputImage, TCompare >::GetMarkerImage()
{
  return this->GetInput(0);
}

template< typename TInputImage, typename TOutputImage, typename TCompare >
void
ReconstructionImageFilter< TInputImage, TOutputImage, TCompare >::SetMaskImage(const MaskImageType *maskImage)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput( 1, const_cast< MaskImageType * >( maskImage ) );
}

template< typename TInputImage, typename TOutputImage, typename TCompare >
const typename ReconstructionImageFilter< TInputImage, TOutputImage, TCompare >::MaskImageType *
ReconstructionImageFilter< TInputImage, TOutputImage, TCompare >::GetMaskImage()
{
  return this->GetInput(1);
}

// a version that takes a padded copy of mask and marker
template< typename TInputImage, typename TOutputImage, typename TCompare >
void
ReconstructionImageFilter< TInputImage, TOutputImage, TCompare >
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();
  // there are 2 passes that use all pixels and a 3rd that uses some
  // subset of the pixels. We'll just pretend that the third pass
  // takes the same as each of the others. Is it OK to update more
  // often than pixels?
  ProgressReporter progress(this, 0, this->GetOutput()->GetRequestedRegion().GetNumberOfPixels() * 3);

  TCompare compare;

  MarkerImageConstPointer markerImage = this->GetMarkerImage();
  MaskImageConstPointer   maskImage = this->GetMaskImage();
  OutputImagePointer      output = this->GetOutput();

  // mask and marker must have the same size
  if ( this->GetMarkerImage()->GetRequestedRegion().GetSize() != this->GetMaskImage()->GetRequestedRegion().GetSize() )
    {
    itkExceptionMacro(<< "Marker and mask must have the same size.");
    }

  // create padded versions of the marker image and the mask image
  typedef typename itk::ConstantPadImageFilter< InputImageType, InputImageType > PadType;

  MarkerImageConstPointer markerImageP;
  MaskImageConstPointer   maskImageP;

  ISizeType padSize;

  if ( m_UseInternalCopy )
    {
    typename PadType::Pointer MaskPad = PadType::New();
    typename PadType::Pointer MarkerPad = PadType::New();
    padSize.Fill(1);

    MaskPad->SetConstant(m_MarkerValue);
    MarkerPad->SetConstant(m_MarkerValue);
    MaskPad->SetPadLowerBound(padSize);
    MaskPad->SetPadUpperBound(padSize);
    MarkerPad->SetPadLowerBound(padSize);
    MarkerPad->SetPadUpperBound(padSize);

    MaskPad->SetInput(maskImage);
    MarkerPad->SetInput(markerImage);
    MaskPad->Update();
    MarkerPad->Update();

    markerImageP = MarkerPad->GetOutput();
    maskImageP = MaskPad->GetOutput();
    }
  else
    {
    maskImageP = this->GetMaskImage();
    InputIteratorType inIt( markerImage,
                            output->GetRequestedRegion() );
    OutputIteratorType outIt( output,
                              output->GetRequestedRegion() );
    // copy marker to output - isn't there a better way?
    while ( !outIt.IsAtEnd() )
      {
      MarkerImagePixelType currentValue = inIt.Get();
      outIt.Set( static_cast< OutputImagePixelType >( currentValue ) );
      ++inIt;
      ++outIt;
      }
    markerImageP = output;
    }

  // declare our queue type
  typedef typename std::queue< OutputImageIndexType > FifoType;
  FifoType IndexFifo;

  NOutputIterator   outNIt;
  InputIteratorType mskIt;
  CNInputIterator   mskNIt;
  ISizeType         kernelRadius;
  kernelRadius.Fill(1);
  if ( m_UseInternalCopy )
    {
    FaceCalculatorType faceCalculator;

    FaceListType   faceList;
    FaceListTypeIt fit;

    faceList = faceCalculator(maskImageP, maskImageP->GetLargestPossibleRegion(),
                              kernelRadius);
    // we will only be processing the body region
    fit = faceList.begin();
    // must be a better way of doing this
    NOutputIterator tt(kernelRadius,
                       markerImageP,
                       *fit);
    outNIt = tt;

    InputIteratorType ttt(maskImageP,
                          *fit);
    mskIt = ttt;
    CNInputIterator tttt(kernelRadius,
                         maskImageP,
                         *fit);
    mskNIt = tttt;
    }
  else
    {
    NOutputIterator tt( kernelRadius,
                        markerImageP,
                        output->GetRequestedRegion() );
    outNIt = tt;

    InputIteratorType ttt( maskImageP,
                           output->GetRequestedRegion() );
    mskIt = ttt;
    CNInputIterator tttt( kernelRadius,
                          maskImageP,
                          output->GetRequestedRegion() );
    mskNIt = tttt;
    }

  setConnectivityPrevious(&outNIt, m_FullyConnected);

  ConstantBoundaryCondition< OutputImageType > oBC;
  oBC.SetConstant(m_MarkerValue);
  outNIt.OverrideBoundaryCondition(&oBC);

  mskIt.GoToBegin();
  // scan in forward raster order
  for ( outNIt.GoToBegin(), mskIt.GoToBegin(); !outNIt.IsAtEnd(); ++outNIt, ++mskIt )
    {
    InputImagePixelType V = outNIt.GetCenterPixel();
    InputImagePixelType iV = static_cast< OutputImagePixelType >( mskIt.Get() );

    // be sure that the pixels in the images follow the preconditions
    if ( compare(V, iV) )
      {
      if ( compare(0, 1) )
        {
        itkExceptionMacro(<< "Marker pixels must be <= mask pixels.");
        }
      else
        {
        itkExceptionMacro(<< "Marker pixels must be >= mask pixels.");
        }
      }

    // visit the previous neighbours
    typename NOutputIterator::ConstIterator sIt;
    for ( sIt = outNIt.Begin(); !sIt.IsAtEnd(); ++sIt )
      {
      InputImagePixelType VN = sIt.Get();
      if ( compare(VN, V) )
        {
        outNIt.SetCenterPixel(VN);
        V = VN;
        }
      }

    // this step clamps to the mask
    if ( compare(V, iV) )
      {
      outNIt.SetCenterPixel(iV);
      }

    progress.CompletedPixel();
    }

  // now for the reverse raster order pass
  // reset the neighborhood
  setConnectivityLater(&outNIt, m_FullyConnected);
  outNIt.OverrideBoundaryCondition(&oBC);
  outNIt.GoToEnd();
  //mskIt.GoToEnd();

  ConstantBoundaryCondition< InputImageType > iBC;
  iBC.SetConstant(m_MarkerValue);

  setConnectivityLater(&mskNIt, m_FullyConnected);
  mskNIt.OverrideBoundaryCondition(&iBC);

  typename NOutputIterator::IndexListType oIndexList, mIndexList;
  typename NOutputIterator::IndexListType::const_iterator oLIt, mLIt;

  oIndexList = outNIt.GetActiveIndexList();
  mIndexList = mskNIt.GetActiveIndexList();

  mskNIt.GoToEnd();
  while ( !outNIt.IsAtBegin() )
    {
    --outNIt;
    --mskNIt;
    InputImagePixelType V = outNIt.GetCenterPixel();
    typename NOutputIterator::ConstIterator sIt;
    for ( sIt = outNIt.Begin(); !sIt.IsAtEnd(); ++sIt )
      {
      InputImagePixelType VN = sIt.Get();
      if ( compare(VN, V) )
        {
        outNIt.SetCenterPixel(VN);
        V = VN;
        }
      }
    InputImagePixelType iV = mskNIt.GetCenterPixel();
    if ( compare(V, iV) )
      {
      outNIt.SetCenterPixel(iV);
      V = iV;
      }

    // now put indexes in the fifo
    //typename CNInputIterator::ConstIterator mIt;
    for ( oLIt = oIndexList.begin(), mLIt = mIndexList.begin(); oLIt != oIndexList.end(); ++oLIt, ++mLIt )
      {
      InputImagePixelType VN = outNIt.GetPixel(*oLIt);
      InputImagePixelType iN = mskNIt.GetPixel(*mLIt);
      if ( compare(V, VN) && compare(iN, VN) )
        {
        IndexFifo.push( outNIt.GetIndex() );
        break;
        }
      }
    progress.CompletedPixel();
    }

  // Now we want to check the full neighborhood
  setConnectivity(&outNIt, m_FullyConnected);
  setConnectivity(&mskNIt, m_FullyConnected);
  mskNIt.OverrideBoundaryCondition(&iBC);
  outNIt.OverrideBoundaryCondition(&oBC);
  oIndexList = outNIt.GetActiveIndexList();
  mIndexList = mskNIt.GetActiveIndexList();
  // now process the fifo - this fill the parts that weren't dealt
  // with by the raster and anti-raster passes
  //typename NOutputIterator::Iterator sIt;
  typename CNInputIterator::ConstIterator mIt;

  while ( !IndexFifo.empty() )
    {
    InputImageIndexType I = IndexFifo.front();
    IndexFifo.pop();
    // reposition the iterators
    outNIt += I - outNIt.GetIndex();
    mskNIt += I - mskNIt.GetIndex();
    InputImagePixelType V = outNIt.GetCenterPixel();
    for ( oLIt = oIndexList.begin(), mLIt = mIndexList.begin();
          oLIt != oIndexList.end();
          ++oLIt, ++mLIt )
      {
      InputImagePixelType VN = outNIt.GetPixel(*oLIt);
      InputImagePixelType iN = mskNIt.GetPixel(*mLIt);
      // candidate for dilation via flooding
      if ( compare(V, VN) && Math::NotAlmostEquals( iN, VN ) )
        {
        if ( compare(iN, V) )
          {
          // not clamped by the mask, propagate the center value
          outNIt.SetPixel(*oLIt, V);
          }
        else
          {
          // apply the clamping
          outNIt.SetPixel(*oLIt, iN);
          }
        IndexFifo.push( outNIt.GetIndex(*oLIt) );
        }
      }
    progress.CompletedPixel();
    }

  if ( m_UseInternalCopy )
    {
    typedef typename itk::CropImageFilter< InputImageType, OutputImageType > CropType;
    typename CropType::Pointer crop = CropType::New();

    crop->SetInput(markerImageP);
    crop->SetUpperBoundaryCropSize(padSize);
    crop->SetLowerBoundaryCropSize(padSize);
    crop->GraftOutput( this->GetOutput() );
    /** execute the minipipeline */
    crop->Update();

    /** graft the minipipeline output back into this filter's output */
    this->GraftOutput( crop->GetOutput() );
    }
}

template< typename TInputImage, typename TOutputImage, typename TCompare >
void
ReconstructionImageFilter< TInputImage, TOutputImage, TCompare >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "MarkerValue: " << m_MarkerValue << std::endl;
  os << indent << "UseInternalCopy: " << m_UseInternalCopy << std::endl;
}
}
#endif
