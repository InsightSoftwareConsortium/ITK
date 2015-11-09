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
#ifndef itkGradientRecursiveGaussianImageFilter_hxx
#define itkGradientRecursiveGaussianImageFilter_hxx

#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GradientRecursiveGaussianImageFilter()
{
  m_NormalizeAcrossScale = false;
  this->m_UseImageDirection = true;

  itkStaticAssert(ImageDimension > 0, "Images shall have one dimension at least");
  const unsigned int imageDimensionMinus1 = ImageDimension - 1;
  if ( ImageDimension > 1 )
    {
    m_SmoothingFilters.resize(imageDimensionMinus1);

    for ( unsigned int i = 0; i != imageDimensionMinus1; ++i )
      {
      m_SmoothingFilters[i] = GaussianFilterType::New();
      m_SmoothingFilters[i]->SetOrder(GaussianFilterType::ZeroOrder);
      m_SmoothingFilters[i]->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
      m_SmoothingFilters[i]->InPlaceOn();
      m_SmoothingFilters[i]->ReleaseDataFlagOn();
      }
    }

  m_DerivativeFilter = DerivativeFilterType::New();
  m_DerivativeFilter->SetOrder(DerivativeFilterType::FirstOrder);
  m_DerivativeFilter->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
  m_DerivativeFilter->ReleaseDataFlagOn();
  m_DerivativeFilter->InPlaceOff();
  m_DerivativeFilter->SetInput( this->GetInput() );

  if ( ImageDimension > 1 )
    {
    m_SmoothingFilters[0]->SetInput( m_DerivativeFilter->GetOutput() );
    for ( unsigned int i = 1; i != imageDimensionMinus1; ++i )
      {
      m_SmoothingFilters[i]->SetInput(m_SmoothingFilters[i - 1]->GetOutput() );
      }
    }

  m_ImageAdaptor = OutputImageAdaptorType::New();

  // NB: We must call SetSigma in order to initialize the smoothing
  // filters with the default scale.  However, m_Sigma must first be
  // initialized (it is used inside SetSigma), and it must be different
  // from 1.0 or the call will be ignored.
  this->m_Sigma.Fill(0.0);
  this->SetSigma(1.0);
}

/**
 * Set value of Sigma along all dimensions.
 */
template< typename TInputImage, typename TOutputImage >
void
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetSigma(ScalarRealType sigma)
{
  SigmaArrayType sigmas(sigma);
  this->SetSigmaArray(sigmas);
}

/**
 * Set value of Sigma array.
 */
template< typename TInputImage, typename TOutputImage >
void
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetSigmaArray(const SigmaArrayType & sigma)
{
  if( this->m_Sigma != sigma )
  {
    this->m_Sigma = sigma;
    itkStaticAssert(ImageDimension > 0, "Images shall have one dimension at least");
    const unsigned int imageDimensionMinus1 = ImageDimension - 1;
    for ( unsigned int i = 0; i != imageDimensionMinus1; ++i )
      {
      m_SmoothingFilters[i]->SetSigma(m_Sigma[i]);
      }
    m_DerivativeFilter->SetSigma(sigma[imageDimensionMinus1]);

    this->Modified();
  }
}

/**
 * Get the Sigma array.
 */
template< typename TInputImage, typename TOutputImage >
typename GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SigmaArrayType
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GetSigmaArray() const
{
  return m_Sigma;
}

/**
 * Get value of Sigma. Returns the sigma along the first dimension.
 */
template< typename TInputImage, typename TOutputImage >
typename GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::ScalarRealType
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GetSigma() const
{
  return m_Sigma[0];
}

/**
 * Set Normalize Across Scale Space
 */
template< typename TInputImage, typename TOutputImage >
void
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetNormalizeAcrossScale(bool normalize)
{
  m_NormalizeAcrossScale = normalize;

  itkStaticAssert(ImageDimension > 0, "Images shall have one dimension at least");
  const unsigned int imageDimensionMinus1 = ImageDimension - 1;
  for ( unsigned int i = 0; i != imageDimensionMinus1; i++ )
    {
    m_SmoothingFilters[i]->SetNormalizeAcrossScale(normalize);
    }
  m_DerivativeFilter->SetNormalizeAcrossScale(normalize);

  this->Modified();
}

//
//
//
template< typename TInputImage, typename TOutputImage >
void
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // This filter needs all of the input
  typename GradientRecursiveGaussianImageFilter< TInputImage,
                                                 TOutputImage >::InputImagePointer image =
    const_cast< InputImageType * >( this->GetInput() );
  if ( image )
    {
    image->SetRequestedRegion( this->GetInput()->GetLargestPossibleRegion() );
    }
}

//
//
//
template< typename TInputImage, typename TOutputImage >
void
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  TOutputImage *out = dynamic_cast< TOutputImage * >( output );

  if ( out )
    {
    out->SetRequestedRegion( out->GetLargestPossibleRegion() );
    }
}

/**
 * Compute filter for Gaussian kernel
 */
template< typename TInputImage, typename TOutputImage >
void
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GenerateData(void)
{
  // Create a process accumulator for tracking the progress of this
  // minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Compute the contribution of each filter to the total progress.
  const double weight = 1.0 / ( ImageDimension * ImageDimension );

  itkStaticAssert(ImageDimension > 0, "Images shall have one dimension at least");
  const unsigned int imageDimensionMinus1 = ImageDimension - 1;
  if( ImageDimension > 1 )
    {
    for( unsigned int i = 0; i != imageDimensionMinus1; ++i )
      {
      progress->RegisterInternalFilter(m_SmoothingFilters[i], weight);
      }
    }

  progress->RegisterInternalFilter(m_DerivativeFilter, weight);

  const typename TInputImage::ConstPointer inputImage( this->GetInput() );
  typename TOutputImage::Pointer           outputImage( this->GetOutput() );

  unsigned int nComponents = inputImage->GetNumberOfComponentsPerPixel();
  /* An Image of VariableLengthVectors will return 0 */
  if (nComponents == 0 )
    {
    const typename InputImageType::IndexType idx = inputImage->GetLargestPossibleRegion().GetIndex();
    nComponents = NumericTraits<typename InputImageType::PixelType>::GetLength( inputImage->GetPixel(idx) );
    }

  m_ImageAdaptor->SetImage( outputImage );

  m_ImageAdaptor->SetLargestPossibleRegion(
    inputImage->GetLargestPossibleRegion() );

  m_ImageAdaptor->SetBufferedRegion(
    inputImage->GetBufferedRegion() );

  m_ImageAdaptor->SetRequestedRegion(
    inputImage->GetRequestedRegion() );

  m_ImageAdaptor->Allocate();

  m_DerivativeFilter->SetInput(inputImage);

  // For variable length output pixel types
  ImageRegionIteratorWithIndex<OutputImageType> initGradIt(
    outputImage, this->m_ImageAdaptor->GetRequestedRegion() );


  for ( unsigned int nc = 0; nc < nComponents; ++nc )
    {
    for ( unsigned int dim = 0; dim < ImageDimension; ++dim )
      {
      unsigned int i = 0;
      int j = 0;
      while( i != imageDimensionMinus1 )
        {
        if( i == dim )
          {
          ++j;
          }
        m_SmoothingFilters[i]->SetDirection(j);
        ++i;
        ++j;
        }
      m_DerivativeFilter->SetDirection(dim);

      GaussianFilterPointer lastFilter;

      if ( ImageDimension > 1 )
        {
        const unsigned int imageDimensionMinus2 = static_cast< unsigned int >( ImageDimension - 2 );
        lastFilter = m_SmoothingFilters[imageDimensionMinus2];
        lastFilter->UpdateLargestPossibleRegion();
        }
      else
        {
        m_DerivativeFilter->UpdateLargestPossibleRegion();
        }

      // Copy the results to the corresponding component
      // on the output image of vectors
      m_ImageAdaptor->SelectNthElement(nc*ImageDimension + dim);

      typename RealImageType::Pointer derivativeImage;
      if ( ImageDimension > 1 )
        {
        derivativeImage = lastFilter->GetOutput();
        }
      else
        {
        derivativeImage = m_DerivativeFilter->GetOutput();
        }

      ImageRegionIteratorWithIndex< RealImageType > it(
        derivativeImage,
        derivativeImage->GetRequestedRegion() );

      ImageRegionIteratorWithIndex< OutputImageAdaptorType > ot(
        m_ImageAdaptor,
        m_ImageAdaptor->GetRequestedRegion() );

      const ScalarRealType spacing = inputImage->GetSpacing()[dim];

      it.GoToBegin();
      ot.GoToBegin();
      while ( !it.IsAtEnd() )
        {
        OutputComponentType outValue = static_cast<OutputComponentType>( DefaultConvertPixelTraits<InternalRealType>::GetNthComponent( nc, it.Get() / spacing ) );
        ot.Set( outValue );
        ++it;
        ++ot;
        }
      }
    }

  // manually release memory in last filter in the mini-pipeline
  if ( ImageDimension > 1 )
    {
    int temp_dim = static_cast< int >( ImageDimension ) - 2;
    m_SmoothingFilters[temp_dim]->GetOutput()->ReleaseData();
    }
  else
    {
    m_DerivativeFilter->GetOutput()->ReleaseData();
    }

  // If the flag for using the input image direction is ON,
  // then we apply the direction correction to all the pixels
  // of the output gradient image.
  if ( this->m_UseImageDirection )
    {

    OutputImageType *gradientImage = outputImage;
    ImageRegionIterator< OutputImageType > itr( gradientImage,
                                                gradientImage->GetRequestedRegion() );

    while ( !itr.IsAtEnd() )
      {

      TransformOutputPixel( itr );
      ++itr;
      }

    }
}

template< typename TInputImage, typename TOutputImage >
void
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  // this methods is overloaded so that if the output image is a
  // VectorImage then the correct number of components are set.

  Superclass::GenerateOutputInformation();

  OutputImageType* output = this->GetOutput();
  const typename TInputImage::ConstPointer inputImage( this->GetInput() );

  const unsigned int nComponents = inputImage->GetNumberOfComponentsPerPixel() * ImageDimension;

  output->SetNumberOfComponentsPerPixel( nComponents );
}

template< typename TInputImage, typename TOutputImage >
void
GradientRecursiveGaussianImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
  os << indent << "UseImageDirection :   "
     << ( this->m_UseImageDirection ? "On" : "Off" ) << std::endl;
  os << "Sigma: " << m_Sigma << std::endl;
}

} // end namespace itk

#endif
