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
#ifndef itkPhaseCorrelationOperator_hxx
#define itkPhaseCorrelationOperator_hxx

#include "itkPhaseCorrelationOperator.h"
#include "itkImageRegionIterator.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"
#include "itkMetaDataObject.h"

namespace itk
{

/*
 * \author Jakub Bican, jakub.bican@matfyz.cz, Department of Image Processing,
 *         Institute of Information Theory and Automation,
 *         Academy of Sciences of the Czech Republic.
 *
 */


template < typename TRealPixel, unsigned int VImageDimension >
PhaseCorrelationOperator< TRealPixel, VImageDimension >
::PhaseCorrelationOperator()
{
  this->SetNumberOfRequiredInputs( 2 );
}


template < typename TRealPixel, unsigned int VImageDimension >
void
PhaseCorrelationOperator< TRealPixel, VImageDimension >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


template < typename TRealPixel, unsigned int VImageDimension >
void
PhaseCorrelationOperator< TRealPixel, VImageDimension >
::SetFixedImage( ImageType * fixedImage )
{
  this->SetNthInput(0, const_cast<ImageType *>( fixedImage ));
}


template < typename TRealPixel, unsigned int VImageDimension >
void
PhaseCorrelationOperator< TRealPixel, VImageDimension >
::SetMovingImage( ImageType * fixedImage )
{
  this->SetNthInput(1, const_cast<ImageType *>( fixedImage ));
}


template < typename TRealPixel, unsigned int VImageDimension >
void
PhaseCorrelationOperator< TRealPixel, VImageDimension >
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       ThreadIdType threadId)
{
  // Get the input and output pointers
  ImageConstPointer  fixed     = this->GetInput(0);
  ImageConstPointer  moving    = this->GetInput(1);
  ImagePointer       output    = this->GetOutput();

  //
  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel to "resample the two volumes".
  typename ImageType::IndexType
     movingStartIndex = moving->GetLargestPossibleRegion().GetIndex(),
     outputStartIndex = output->GetLargestPossibleRegion().GetIndex();
  typename ImageType::SizeType
     fixedSize  = fixed->GetLargestPossibleRegion().GetSize(),
     movingSize = moving->GetLargestPossibleRegion().GetSize();
  typename ImageType::IndexType fixedGapStart;
  typename ImageType::IndexType movingGapStart;
  typename ImageType::SizeType  fixedGapSize;
  typename ImageType::SizeType  movingGapSize;
  unsigned int i;

  // crop every dimension to make the size of fixed and moving the same
  for( i = 0; i < ImageType::ImageDimension; ++i )
    {
    fixedGapSize[i] = (fixedSize[i]-movingSize[i])>0 ?
                                            (fixedSize[i]-movingSize[i]) : 0;
    movingGapSize[i] = (movingSize[i]-fixedSize[i])>0 ?
                                            (movingSize[i]-fixedSize[i]) : 0;
    }

  i = 0; //reset the counter

  // for halved fft matrixes, must start the first dimension differently
  fixedGapStart[0] = fixedGapSize[0]>0 ? movingSize[0] : fixedSize[0];
  movingGapStart[0] = movingGapSize[0]>0 ? fixedSize[0] : movingSize[0];
  ++i;
  // all "normal" dimensions exclude central frequencies
  for (; i<ImageType::ImageDimension; i++)
    {
    if ( fixedGapSize[i]>0 ) // fixed is too large
      {
      if ( (fixedSize[i]%2 + fixedGapSize[i]%2) > 1 )
        {
        fixedGapStart[i] = (unsigned long)( Math::floor(fixedSize[i]/2.0) -
                                            Math::floor(fixedGapSize[i]/2.0) );
        }
      else
        {
        fixedGapStart[i] = (unsigned long)( Math::ceil(fixedSize[i]/2.0) -
                                            Math::floor(fixedGapSize[i]/2.0) );
        }
      movingGapStart[i] = movingSize[i];
      }
    else if ( movingGapSize[i]>0 ) // moving is too large
      {
      if ( (movingSize[i]%2 + movingGapSize[i]%2) > 1 )
        {
        movingGapStart[i] = (unsigned long)( Math::floor(movingSize[i]/2.0) -
                                             Math::floor(movingGapSize[i]/2.0) );
        }
      else
        {
        movingGapStart[i] = (unsigned long)( Math::ceil(movingSize[i]/2.0) -
                                             Math::floor(movingGapSize[i]/2.0) );
        }
      fixedGapStart[i] = fixedSize[i];
      }
    else // fixed and moving is the same size
      {
      fixedGapStart[i] = fixedSize[i];
      movingGapStart[i] = movingSize[i];
      }
    }

  //
  // Define/declare an iterator that will walk the output region for this
  // thread.
  typedef  ImageRegionIterator<ImageType> OutputIterator;

  OutputIterator outIt(output, outputRegionForThread);
  typename ImageType::IndexType fixedIndex;
  typename ImageType::IndexType movingIndex;
  typename ImageType::IndexType outputIndex;

  itkDebugMacro( "computing correlation surface" );
  // support progress methods/callbacks
  ProgressReporter progress(this, threadId,
                            outputRegionForThread.GetNumberOfPixels());

  typedef typename ImageType::IndexType::IndexValueType IndexValueType;

  // walk the output region, and sample the input image
  while ( !outIt.IsAtEnd() )
    {
    // determine the index of the output pixel
    outputIndex = outIt.GetIndex();

    // determine the input pixels location associated with this output pixel
    for (unsigned int i = 0; i<ImageType::ImageDimension; i++)
      {
      fixedIndex[i] = outputIndex[i]; //fixed and output images have same StartIndex
      movingIndex[i] = outputIndex[i]-outputStartIndex[i]+movingStartIndex[i];

      if (fixedIndex[i] >= fixedGapStart[i])
          fixedIndex[i] += fixedGapSize[i];
      if (movingIndex[i] >= movingGapStart[i])
          movingIndex[i] += movingGapSize[i];
      if (fixedIndex[i] < 0)
          fixedIndex[i] = 0;
      if (fixedIndex[i] > static_cast<IndexValueType>(fixedSize[i] - 1))
          fixedIndex[i] = fixedSize[i] - 1;
      if (movingIndex[i] < 0)
          movingIndex[i] = 0;
      if (movingIndex[i] > static_cast<IndexValueType>(movingSize[i] - 1))
          movingIndex[i] = movingSize[i] - 1;
      }

    // compute the phase correlation
    ComplexType fixedVal = fixed->GetPixel(fixedIndex);
    ComplexType movingVal = moving->GetPixel(movingIndex);

    outIt.Set( this->ComputeAtIndex(outputIndex,fixedVal,movingVal) );

    ++outIt;

    progress.CompletedPixel();
    }
}


template < typename TRealPixel, unsigned int VImageDimension >
typename PhaseCorrelationOperator< TRealPixel, VImageDimension >::ComplexType
PhaseCorrelationOperator< TRealPixel, VImageDimension >
::ComputeAtIndex(typename ImageType::IndexType & outputIndex,
                          ComplexType          & fixedValue,
                          ComplexType          & movingValue)
{
  PixelType real = fixedValue.real()*movingValue.real() +
                   fixedValue.imag()*movingValue.imag();
  PixelType imag = fixedValue.imag()*movingValue.real() -
                   fixedValue.real()*movingValue.imag();
  PixelType magn = std::sqrt( real*real + imag*imag );

  if (magn != 0 )
    {
    return ComplexType(real/magn,imag/magn);
    }
  else
    {
    return ComplexType( 0, 0);
    }
}


/**
 *  Request all available data. This filter is cropping from the center.
 */
template < typename TRealPixel, unsigned int VImageDimension >
void
PhaseCorrelationOperator< TRealPixel, VImageDimension >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the inputs
  ImagePointer fixed  = const_cast<ImageType *> (this->GetInput(0));
  ImagePointer moving = const_cast<ImageType *> (this->GetInput(1));

  if ( !fixed || !moving )
    {
    return;
    }

  fixed->SetRequestedRegion(  fixed->GetLargestPossibleRegion()  );
  moving->SetRequestedRegion( moving->GetLargestPossibleRegion() );
}

/**
 *  The output will have the lower size of the two input images in all
 *  dimensions.
 */
template < typename TRealPixel, unsigned int VImageDimension >
void
PhaseCorrelationOperator< TRealPixel, VImageDimension >
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the inputs and output
  ImageConstPointer fixed  = this->GetInput(0);
  ImageConstPointer moving = this->GetInput(1);
  ImagePointer      output = this->GetOutput();

  if ( !fixed || !moving || !output )
    {
    return;
    }

  itkDebugMacro( "adjusting size of output image" );
  // we need to compute the output spacing, the output image size, and the
  // output image start index
  unsigned int i;
  const typename ImageType::SpacingType&
    fixedSpacing     =  fixed->GetSpacing(),
    movingSpacing    = moving->GetSpacing();
  const typename ImageType::SizeType&
    fixedSize        =  fixed->GetLargestPossibleRegion().GetSize(),
    movingSize       = moving->GetLargestPossibleRegion().GetSize();
  const typename ImageType::IndexType&
    fixedStartIndex  =  fixed->GetLargestPossibleRegion().GetIndex();

  typename ImageType::SpacingType  outputSpacing;
  typename ImageType::SizeType     outputSize;
  typename ImageType::IndexType    outputStartIndex;

  for (i = 0; i < ImageType::ImageDimension; i++)
    {
    outputSpacing[i]    = fixedSpacing[i] >= movingSpacing[i] ?
                                            fixedSpacing[i] : movingSpacing[i];
    outputSize[i]       = fixedSize[i] <= movingSize[i] ?
                                            fixedSize[i] : movingSize[i];
    outputStartIndex[i] = fixedStartIndex[i];
    }

  // additionally adjust the data size
  this->AdjustOutputInformation( outputSpacing, outputStartIndex, outputSize );

  output->SetSpacing( outputSpacing );

  typename ImageType::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize( outputSize );
  outputLargestPossibleRegion.SetIndex( outputStartIndex );

  output->SetLargestPossibleRegion( outputLargestPossibleRegion );

  //
  // Pass the metadata with the actual size of the image.
  // The size must be adjusted according to the cropping and scaling
  // that will be made on the image!
  itkDebugMacro( "storing size of pre-FFT image in MetaData" );
  typedef typename ImageType::SizeValueType SizeScalarType;

  SizeScalarType fixedX = NumericTraits< SizeScalarType >::Zero;
  SizeScalarType movingX = NumericTraits< SizeScalarType >::Zero;
  SizeScalarType outputX = NumericTraits< SizeScalarType >::Zero;

  MetaDataDictionary &fixedDic  = const_cast<MetaDataDictionary &>(fixed->GetMetaDataDictionary());
  MetaDataDictionary &movingDic = const_cast<MetaDataDictionary &>(moving->GetMetaDataDictionary());
  MetaDataDictionary &outputDic = const_cast<MetaDataDictionary &>(output->GetMetaDataDictionary());

  if(ExposeMetaData < SizeScalarType >
          (fixedDic,std::string("FFT_Actual_RealImage_Size"),fixedX)
     &&
     ExposeMetaData < SizeScalarType >
          (movingDic,std::string("FFT_Actual_RealImage_Size"),movingX))
    {
    outputX = fixedX > movingX ? movingX : fixedX;

    EncapsulateMetaData<SizeScalarType>(outputDic,
                                        std::string("FFT_Actual_RealImage_Size"),
                                        outputX);
    }
}


template < typename TRealPixel, unsigned int VImageDimension >
void
PhaseCorrelationOperator< TRealPixel, VImageDimension >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

} //end namespace itk

#endif
