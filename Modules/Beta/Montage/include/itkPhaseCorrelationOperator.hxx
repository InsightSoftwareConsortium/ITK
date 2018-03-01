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
  // Define/declare an iterator that will walk the output region for this
  // thread.
  typedef  ImageRegionConstIterator<ImageType> InputIterator;
  typedef  ImageRegionIterator<ImageType>      OutputIterator;

  InputIterator fixedIt(fixed, outputRegionForThread);
  InputIterator movingIt(moving, outputRegionForThread);
  OutputIterator outIt(output, outputRegionForThread);

  itkDebugMacro( "computing correlation surface" );
  // support progress methods/callbacks
  ProgressReporter progress(this, threadId,
                            outputRegionForThread.GetNumberOfPixels());

  // walk the output region, and sample the input image
  while ( !outIt.IsAtEnd() )
    {
    // compute the phase correlation
    const PixelType real = fixedIt.Value().real() * movingIt.Value().real() +
                           fixedIt.Value().imag() * movingIt.Value().imag();
    const PixelType imag = fixedIt.Value().imag() * movingIt.Value().real() -
                           fixedIt.Value().real() * movingIt.Value().imag();
    const PixelType magn = std::sqrt( real*real + imag*imag );

    if (magn != 0 )
      {
      outIt.Set( ComplexType( real/magn, imag/magn ) );
      }
    else
      {
      outIt.Set( ComplexType( 0, 0 ) );
      }

    ++fixedIt;
    ++movingIt;
    ++outIt;

    progress.CompletedPixel();
    }
}


template < typename TRealPixel, unsigned int VImageDimension >
void
PhaseCorrelationOperator< TRealPixel, VImageDimension >
::GenerateInputRequestedRegion()
{
  /**
   *  Request all available data. This filter is cropping from the center.
   */

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


template < typename TRealPixel, unsigned int VImageDimension >
void
PhaseCorrelationOperator< TRealPixel, VImageDimension >
::GenerateOutputInformation()
{
  /**
   *  The output will have the lower size of the two input images in all
   *  dimensions.
   */

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
