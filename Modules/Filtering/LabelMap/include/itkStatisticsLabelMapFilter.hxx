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
#ifndef __itkStatisticsLabelMapFilter_hxx
#define __itkStatisticsLabelMapFilter_hxx

#include "itkStatisticsLabelMapFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkProgressReporter.h"
#include "vnl/algo/vnl_real_eigensystem.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"
#include "vnl/vnl_math.h"

namespace itk
{
template< typename TImage, typename TFeatureImage >
StatisticsLabelMapFilter< TImage, TFeatureImage >
::StatisticsLabelMapFilter()
{
  m_NumberOfBins = 128;
  m_ComputeHistogram = true;
  this->SetNumberOfRequiredInputs(2);
}

template< typename TImage, typename TFeatureImage >
void
StatisticsLabelMapFilter< TImage, TFeatureImage >
::BeforeThreadedGenerateData()
{
  Superclass::BeforeThreadedGenerateData();

  // get the min and max of the feature image, to use those value as the bounds
  // of our
  // histograms
  typedef MinimumMaximumImageCalculator< FeatureImageType > MinMaxCalculatorType;
  typename MinMaxCalculatorType::Pointer minMax = MinMaxCalculatorType::New();
  minMax->SetImage( this->GetFeatureImage() );
  minMax->Compute();

  m_Minimum = minMax->GetMinimum();
  m_Maximum = minMax->GetMaximum();
}

template< typename TImage, typename TFeatureImage >
void
StatisticsLabelMapFilter< TImage, TFeatureImage >
::ThreadedProcessLabelObject(LabelObjectType *labelObject)
{
  Superclass::ThreadedProcessLabelObject(labelObject);

  ImageType *             output = this->GetOutput();
  const FeatureImageType *featureImage = this->GetFeatureImage();

  typedef typename LabelObjectType::HistogramType HistogramType;

  typename HistogramType::SizeType histogramSize;
  histogramSize.SetSize(1);
  histogramSize.Fill(m_NumberOfBins);

  typename HistogramType::MeasurementVectorType featureImageMin;
  featureImageMin.SetSize(1);
  featureImageMin.Fill(m_Minimum);

  typename HistogramType::MeasurementVectorType featureImageMax;
  featureImageMax.SetSize(1);
  featureImageMax.Fill(m_Maximum);

  typename HistogramType::Pointer histogram = HistogramType::New();
  histogram->SetMeasurementVectorSize(1);
  histogram->SetClipBinsAtEnds(false);
  histogram->Initialize(histogramSize, featureImageMin, featureImageMax);

  FeatureImagePixelType min = NumericTraits< FeatureImagePixelType >::max();
  FeatureImagePixelType max = NumericTraits< FeatureImagePixelType >::NonpositiveMin();
  double                sum = 0;
  double                sum2 = 0;
  double                sum3 = 0;
  double                sum4 = 0;
  IndexType             minIdx;
  minIdx.Fill(0);
  IndexType maxIdx;
  maxIdx.Fill(0);
  PointType centerOfGravity;
  centerOfGravity.Fill(0);
  MatrixType centralMoments;
  centralMoments.Fill(0);
  MatrixType principalAxes;
  principalAxes.Fill(0);
  VectorType principalMoments;
  principalMoments.Fill(0);

  typename HistogramType::MeasurementVectorType mv;
  mv.SetSize(1);
  // iterate over all the indexes
  typename LabelObjectType::ConstIndexIterator it( labelObject );
  while( ! it.IsAtEnd() )
    {
    const IndexType & idx = it.GetIndex();
    const FeatureImagePixelType & v = featureImage->GetPixel(idx);
    mv[0] = v;
    histogram->IncreaseFrequencyOfMeasurement(mv, 1);

    // update min and max
    if ( v <= min )
      {
      min = v;
      minIdx = idx;
      }
    if ( v >= max )
      {
      max = v;
      maxIdx = idx;
      }

    //increase the sums
    sum += v;
    sum2 += vcl_pow( (double)v, 2 );
    sum3 += vcl_pow( (double)v, 3 );
    sum4 += vcl_pow( (double)v, 4 );

    // moments
    PointType physicalPosition;
    output->TransformIndexToPhysicalPoint(idx, physicalPosition);
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      centerOfGravity[i] += physicalPosition[i] * v;
      centralMoments[i][i] += v * physicalPosition[i] * physicalPosition[i];
      for ( unsigned int j = i + 1; j < ImageDimension; j++ )
        {
        double weight = v * physicalPosition[i] * physicalPosition[j];
        centralMoments[i][j] += weight;
        centralMoments[j][i] += weight;
        }
      }
    ++it;
    }

  // final computations
  const typename HistogramType::AbsoluteFrequencyType & totalFreq = histogram->GetTotalFrequency();
  const double mean = sum / totalFreq;
  const double variance = ( sum2 - ( vcl_pow(sum, 2) / totalFreq ) ) / ( totalFreq - 1 );
  const double sigma = vcl_sqrt(variance);
  const double mean2 = mean * mean;
  double skewness;
  if(vcl_abs(variance * sigma) > itk::NumericTraits<double>::min())
    {
    skewness = ( ( sum3 - 3.0 * mean * sum2 ) / totalFreq + 2.0 * mean * mean2 ) / ( variance * sigma );
    }
  else
    {
    skewness = 0.0;
    }
  double kurtosis;
  if(vcl_abs(variance) > itk::NumericTraits<double>::min())
    {
    kurtosis = ( ( sum4 - 4.0 * mean * sum3 + 6.0 * mean2
                   * sum2 ) / totalFreq - 3.0 * mean2 * mean2 ) /
      ( variance * variance ) - 3.0;
    }
  else
    {
    kurtosis = 0.0;
    }

  // the median
  double median = 0;
  double count = 0;  // will not be fully set, so do not use later !
  for ( SizeValueType i = 0; i < histogram->Size(); i++ )
    {
    count += histogram->GetFrequency(i);

    if ( count >= ( totalFreq / 2 ) )
      {
      median = histogram->GetMeasurementVector(i)[0];
      break;
      }
    }

  double elongation = 0;
  double flatness = 0;
  if ( sum != 0 )
    {
    // Normalize using the total mass
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      centerOfGravity[i] /= sum;
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        centralMoments[i][j] /= sum;
        }
      }

    // Center the second order moments
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        centralMoments[i][j] -= centerOfGravity[i] * centerOfGravity[j];
        }
      }

    // the normalized second order central moment of a pixel
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      centralMoments[i][i] += output->GetSpacing()[i] * output->GetSpacing()[i] / 12.0;
      }

    // Compute principal moments and axes
    vnl_symmetric_eigensystem< double > eigen( centralMoments.GetVnlMatrix() );
    vnl_diag_matrix< double >           pm = eigen.D;
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      //    principalMoments[i] = 4 * vcl_sqrt( pm(i,i) );
      principalMoments[i] = pm(i, i);
      }
    principalAxes = eigen.V.transpose();

    // Add a final reflection if needed for a proper rotation,
    // by multiplying the last row by the determinant
    vnl_real_eigensystem                     eigenrot( principalAxes.GetVnlMatrix() );
    vnl_diag_matrix< vcl_complex< double > > eigenval = eigenrot.D;
    vcl_complex< double >                    det(1.0, 0.0);

    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      det *= eigenval(i, i);
      }

    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      principalAxes[ImageDimension - 1][i] *= std::real(det);
      }

    if ( ImageDimension < 2 )
      {
      elongation = 1;
      flatness = 1;
      }
    else if ( principalMoments[0] != 0 )
      {
      //    elongation = principalMoments[ImageDimension-1] /
      // principalMoments[0];
      elongation = vcl_sqrt(principalMoments[ImageDimension - 1] / principalMoments[ImageDimension - 2]);
      flatness = vcl_sqrt(principalMoments[1] / principalMoments[0]);
      }
    }
  else
    {
    // can't compute anything in that case - just set everything to a default
    // value
    // Normalize using the total mass
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      centerOfGravity[i] = 0;
      principalMoments[i] = 0;
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        principalAxes[i][j] = 0;
        }
      }
    }

  // finally put the values in the label object
  labelObject->SetMinimum( (double)min );
  labelObject->SetMaximum( (double)max );
  labelObject->SetSum(sum);
  labelObject->SetMean(mean);
  labelObject->SetMedian(median);
  labelObject->SetVariance(variance);
  labelObject->SetStandardDeviation(sigma);
  labelObject->SetMinimumIndex(minIdx);
  labelObject->SetMaximumIndex(maxIdx);
  labelObject->SetCenterOfGravity(centerOfGravity);
  labelObject->SetWeightedPrincipalAxes(principalAxes);
  labelObject->SetWeightedFlatness(flatness);
  labelObject->SetWeightedPrincipalMoments(principalMoments);
  // labelObject->SetCentralMoments( centralMoments );
  labelObject->SetSkewness(skewness);
  labelObject->SetKurtosis(kurtosis);
  labelObject->SetWeightedElongation(elongation);
  if ( m_ComputeHistogram )
    {
    labelObject->SetHistogram(histogram);
    }
}

template< typename TImage, typename TFeatureImage >
void
StatisticsLabelMapFilter< TImage, TFeatureImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ComputeHistogram: " << m_ComputeHistogram << std::endl;
  os << indent << "NumberOfBins: " << m_NumberOfBins << std::endl;
}
} // end namespace itk
#endif
