/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHistogramMatchingImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkHistogramMatchingImageFilter_txx
#define _itkHistogramMatchingImageFilter_txx

#include "itkHistogramMatchingImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkNumericTraits.h"
#include <vector>

namespace itk
{

/*
 *
 */
template <class TInputImage, class TOutputImage>
HistogramMatchingImageFilter<TInputImage,TOutputImage>
::HistogramMatchingImageFilter()
{
  this->SetNumberOfRequiredInputs( 2 );

  m_NumberOfHistogramLevels = 256;
  m_NumberOfMatchPoints = 1;

  m_QuantileTable.resize( 2, m_NumberOfMatchPoints + 2 );
  m_Gradients.resize( m_NumberOfMatchPoints + 1 );

  m_ThresholdAtMeanIntensity = true;

}


/*
 *
 */
template <class TInputImage, class TOutputImage>
void 
HistogramMatchingImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "NumberOfHistogramLevels: ";
  os << m_NumberOfHistogramLevels << std::endl;
  os << indent << "NumberOfMatchPoints: ";
  os << m_NumberOfMatchPoints << std::endl;
  os << indent << "ThresholdAtMeanIntensity: ";
  os << m_ThresholdAtMeanIntensity << std::endl;
  
  os << indent << "SourceIntensityThreshold: ";
  os << m_SourceIntensityThreshold << std::endl;
  os << indent << "ReferenceIntensityThreshold: ";
  os << m_ReferenceIntensityThreshold << std::endl;
  os << indent << "QuantileTable: " << std::endl;
  os << m_QuantileTable << std::endl;
  os << indent << "Gradients: " << std::endl;
  os << m_Gradients << std::endl;
  os << indent << "LowerGradient: ";
  os << m_LowerGradient << std::endl;
  os << indent << "UpperGradient: ";
  os << m_UpperGradient << std::endl;

}


/*
 *
 */
template <class TInputImage, class TOutputImage>
void 
HistogramMatchingImageFilter<TInputImage,TOutputImage>
::SetReferenceImage( const InputImageType * reference )
{
  this->ProcessObject::SetNthInput(1, 
    const_cast< InputImageType * >( reference ) );
}


/*
 *
 */
template <class TInputImage, class TOutputImage>
const typename HistogramMatchingImageFilter<TInputImage,TOutputImage>
::InputImageType *
HistogramMatchingImageFilter<TInputImage,TOutputImage>
::GetReferenceImage()
{
  if ( this->GetNumberOfInputs() < 2 )
    {
    return NULL;
    }

  return dynamic_cast<TInputImage*>(
    this->ProcessObject::GetInput(1) );
}


/*
 * This filter requires all of the input images to be
 * in the buffer.
 */
template <class TInputImage, class TOutputImage>
void
HistogramMatchingImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  this->Superclass::GenerateInputRequestedRegion();

  for ( unsigned int idx = 0; idx < this->GetNumberOfInputs(); ++idx )
    {
    if ( this->GetInput(idx) )
      {
      InputImagePointer image = 
           const_cast< InputImageType * >( this->GetInput(idx) );
      image->SetRequestedRegionToLargestPossibleRegion();
      }
    }
}


/*
 * 
 */
template <class TInputImage, class TOutputImage>
void
HistogramMatchingImageFilter<TInputImage,TOutputImage>
::BeforeThreadedGenerateData()
{

  unsigned int j;

  InputImageConstPointer  source    = this->GetSourceImage();
  InputImageConstPointer  reference = this->GetReferenceImage();

  // Create histograms.
  HistogramPointer sourceHistogram = HistogramType::New();
  HistogramPointer referenceHistogram = HistogramType::New();

  this->ComputeMinMaxMean( source, m_SourceMinValue, 
    m_SourceMaxValue, m_SourceMeanValue );
  this->ComputeMinMaxMean( reference, m_ReferenceMinValue, 
    m_ReferenceMaxValue, m_ReferenceMeanValue );

  if ( m_ThresholdAtMeanIntensity )
    {
    m_SourceIntensityThreshold    = static_cast<InputPixelType>(m_SourceMeanValue);
    m_ReferenceIntensityThreshold = static_cast<InputPixelType>(m_ReferenceMeanValue);
    }
  else
    {
    m_SourceIntensityThreshold    = static_cast<InputPixelType>(m_SourceMinValue);
    m_ReferenceIntensityThreshold = static_cast<InputPixelType>(m_ReferenceMinValue);
    }

  this->ConstructHistogram( source, sourceHistogram, 
    m_SourceIntensityThreshold, m_SourceMaxValue );
  this->ConstructHistogram( reference, referenceHistogram, 
    m_ReferenceIntensityThreshold, m_ReferenceMaxValue );    
   
  // Fill in the quantile table.
  m_QuantileTable.resize( 2, m_NumberOfMatchPoints + 2 );
  m_QuantileTable[0][0] = m_SourceIntensityThreshold;
  m_QuantileTable[1][0] = m_ReferenceIntensityThreshold;

  m_QuantileTable[0][m_NumberOfMatchPoints + 1] = m_SourceMaxValue;
  m_QuantileTable[1][m_NumberOfMatchPoints + 1] = m_ReferenceMaxValue;
 
  double delta = 1.0 / ( double(m_NumberOfMatchPoints) + 1.0 );

  for ( unsigned long j = 1; j < m_NumberOfMatchPoints + 1; j++ )
    {
    m_QuantileTable[0][j] = sourceHistogram->Quantile( 
      0, double(j) * delta );
    m_QuantileTable[1][j] = referenceHistogram->Quantile(
      0, double(j) * delta );
    } 

  // Fill in the gradient array.
  m_Gradients.resize( m_NumberOfMatchPoints + 1 );
  double denominator;
  for ( j = 0; j < m_NumberOfMatchPoints + 1; j++ )
    {
    denominator = m_QuantileTable[0][j+1] - 
      m_QuantileTable[0][j];
    if ( denominator != 0 )
      {
      m_Gradients[j] = m_QuantileTable[1][j+1] - 
        m_QuantileTable[1][j];
      m_Gradients[j] /= denominator;
      }
    else
      {
      m_Gradients[j] = 0.0;
      }
    }

    denominator = m_QuantileTable[0][0] - m_SourceMinValue;
    if ( denominator != 0 )
      {
      m_LowerGradient = m_QuantileTable[1][0] - m_ReferenceMinValue;
      m_LowerGradient /= denominator;
      }
    else
      {
      m_LowerGradient = 0.0;
      }

    denominator = m_QuantileTable[0][m_NumberOfMatchPoints+1] - 
      m_SourceMaxValue;
    if ( denominator != 0 )
      { 
      m_UpperGradient = m_QuantileTable[1][m_NumberOfMatchPoints+1] - 
        m_ReferenceMaxValue;
      m_UpperGradient /= denominator;
      }
    else
      {
      m_UpperGradient = 0.0;
      }

}

/*
 *
 */
template <class TInputImage, class TOutputImage>
void
HistogramMatchingImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId )
{

  int i;
  unsigned int j;

  // Get the input and output pointers;
  InputImageConstPointer  input  = this->GetInput();  
  OutputImagePointer      output = this->GetOutput();


  // Transform the source image and write to output.
  typedef ImageRegionConstIterator<InputImageType> InputConstIterator;
  typedef ImageRegionIterator<OutputImageType> OutputIterator;

  InputConstIterator inIter(  input, outputRegionForThread );
  OutputIterator     outIter( output, outputRegionForThread );


  // support progress methods/callbacks
  unsigned long updateVisits = 0;
  unsigned long totalPixels = 0;
  if ( threadId == 0 )
    {
    totalPixels = outputRegionForThread.GetNumberOfPixels();
    updateVisits = totalPixels / 10;
    if( updateVisits < 1 ) updateVisits = 1;
    }


  double srcValue, mappedValue;

  for ( i = 0; !outIter.IsAtEnd(); ++inIter, ++outIter, i++ )
    {

    if ( threadId == 0 && !(i % updateVisits ) )
        {
        this->UpdateProgress((float)i / (float)totalPixels);
        }

    srcValue = static_cast<double>( inIter.Get() );

    for ( j = 0; j < m_NumberOfMatchPoints + 2; j++ )
      {
      if ( srcValue < m_QuantileTable[0][j] )
        {
        break;
        }
      }

    if ( j == 0 )
      {
      // Linear interpolate from min to point[0]
      mappedValue = m_ReferenceMinValue  +
        ( srcValue - m_SourceMinValue ) * m_LowerGradient;
      }
    else if ( j == m_NumberOfMatchPoints + 2 )
      {
      // Linear interpolate from point[m_NumberOfMatchPoints+1] to max
      mappedValue = m_ReferenceMaxValue +
        ( srcValue - m_SourceMaxValue ) * m_UpperGradient;
      }
    else
      {
      // Linear interpolate from point[j] and point[j+1].
      mappedValue = m_QuantileTable[1][j-1] +
        ( srcValue - m_QuantileTable[0][j-1] ) * m_Gradients[j-1];
      }

    outIter.Set( static_cast<OutputPixelType>( mappedValue ) );

    }

}


/*
 * Compute min, max and mean of an image.
 */
template <class TInputImage, class TOutputImage>
void
HistogramMatchingImageFilter<TInputImage,TOutputImage>
::ComputeMinMaxMean(
  const InputImageType * image,
  double& minValue,
  double& maxValue,
  double& meanValue )
{
  typedef ImageRegionConstIterator<InputImageType> ConstIterator;
  ConstIterator iter( image, image->GetBufferedRegion() );

  double sum = 0.0;
  long int count = 0;
 
  minValue = static_cast<double>( iter.Get() );
  maxValue = minValue;

  double value;
  while ( !iter.IsAtEnd() )
    {
    value = static_cast<double>( iter.Get() );
    sum += value;

    if ( value < minValue ) { minValue = value; }
    if ( value > maxValue ) { maxValue = value; }

    ++iter;
    ++count;

    }

  meanValue = ( sum / count );

}


/*
 * Construct a histogram from an image.
 */
template <class TInputImage, class TOutputImage>
void
HistogramMatchingImageFilter<TInputImage,TOutputImage>
::ConstructHistogram(
  const InputImageType * image,
  HistogramType  * histogram,
  double minValue,
  double maxValue )
{

  // allocate memory for the histogram
  typename HistogramType::SizeType size;
  size[0] = m_NumberOfHistogramLevels;

  histogram->Initialize( size );

  // set up min/max values in the histogram
  float stepSize = ( maxValue - minValue ) / 
    static_cast<float>( m_NumberOfHistogramLevels );

  unsigned long ibin;
  for ( ibin = 0; ibin < m_NumberOfHistogramLevels - 1; ibin++ )
    {
    histogram->SetBinMin( 0, ibin, ibin * stepSize + minValue );
    histogram->SetBinMax( 0, ibin, ( ibin + 1 ) * stepSize + minValue );
    }

  histogram->SetBinMin( 0, ibin, ibin * stepSize + minValue );
  histogram->SetBinMax( 0, ibin, maxValue + stepSize );


  // put each image pixel into the histogram
  typedef ImageRegionConstIterator<InputImageType> ConstIterator;
  ConstIterator iter( image, image->GetBufferedRegion() );

  while ( !iter.IsAtEnd() )
    {

    InputPixelType value = iter.Get();

    if ( static_cast<double>(value) >= minValue &&
         static_cast<double>(value) <= maxValue )
      {
      
      // add sample to histogram

      typename HistogramType::IndexType index;
      typename HistogramType::InstanceIdentifier id;
      typename HistogramType::MeasurementVectorType measurement;
  
      measurement[0] = value;
      index = histogram->GetIndex( measurement );
      id = histogram->GetInstanceIdentifier( index );
      histogram->IncreaseFrequency( id, 1 );

      }

    ++iter;
    
    }


}


} // end namespace itk

#endif
