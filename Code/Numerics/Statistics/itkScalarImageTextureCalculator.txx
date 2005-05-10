/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarImageTextureCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkScalarImageTextureCalculator_txx
#define _itkScalarImageTextureCalculator_txx

#include "itkScalarImageTextureCalculator.h"
#include "itkNeighborhood.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace Statistics {

template< class TImage, class THistogramFrequencyContainer >
ScalarImageTextureCalculator< TImage, THistogramFrequencyContainer >::
ScalarImageTextureCalculator()
{
  m_GLCMGenerator = GLCMGeneratorType::New();
  m_FeatureMeans = FeatureValueVector::New();
  m_FeatureStandardDeviations = FeatureValueVector::New();
      
  // Set the requested features to the default value:
  // {Energy, Entropy, InverseDifferenceMoment, Inertia, ClusterShade, ClusterProminence}
  FeatureNameVectorPointer requestedFeatures = FeatureNameVector::New(); 
  // can't directly set m_RequestedFeatures since it is const!
  requestedFeatures->push_back(Energy);
  requestedFeatures->push_back(Entropy);
  requestedFeatures->push_back(InverseDifferenceMoment);
  requestedFeatures->push_back(Inertia);
  requestedFeatures->push_back(ClusterShade);
  requestedFeatures->push_back(ClusterProminence);
  this->SetRequestedFeatures(requestedFeatures);
  
  // Set the offset directions to their defaults: half of all the possible
  // directions 1 pixel away. (The other half is included by symmetry.)
  // We use a neighborhood iterator to calculate the appropriate offsets.
  typedef Neighborhood<ITK_TYPENAME ImageType::PixelType, ::itk::GetImageDimension< 
    ImageType >::ImageDimension > NeighborhoodType;
  NeighborhoodType hood;
  hood.SetRadius(1);
      
  // select all "previous" neighbors that are face+edge+vertex
  // connected to the current pixel. do not include the center pixel.
  unsigned int centerIndex = hood.GetCenterNeighborhoodIndex();
  OffsetType offset;
  OffsetVectorPointer offsets = OffsetVector::New();
  for (unsigned int d=0; d < centerIndex; d++)
    {
    offset = hood.GetOffset(d);
    offsets->push_back(offset);
    }
  this->SetOffsets(offsets);
  m_FastCalculations = false;
}

template< class TImage, class THistogramFrequencyContainer >
void
ScalarImageTextureCalculator< TImage, THistogramFrequencyContainer >::    
Compute(void)
{
  if (m_FastCalculations) 
    {
    this->FastCompute();
    }
  else 
    {
    this->FullCompute();
    }
}  

template< class TImage, class THistogramFrequencyContainer >
void
ScalarImageTextureCalculator< TImage, THistogramFrequencyContainer >::    
FullCompute(void)
{
  int numOffsets = m_Offsets->size();
  int numFeatures = m_RequestedFeatures->size();
  double **features;

  features = new double *[numOffsets];
  for (int i = 0; i < numOffsets; i++)
    {
    features[i] = new double [numFeatures];
    }
      
  // For each offset, calculate each feature
  typename OffsetVector::ConstIterator offsetIt;
  int offsetNum, featureNum;
      
  for(offsetIt = m_Offsets->Begin(), offsetNum = 0;
      offsetIt != m_Offsets->End(); offsetIt++, offsetNum++)
    {
    m_GLCMGenerator->SetOffset(offsetIt.Value());
    m_GLCMGenerator->Compute();
    typename GLCMCalculatorType::Pointer glcmCalc = GLCMCalculatorType::New();
    glcmCalc->SetHistogram(m_GLCMGenerator->GetOutput());
    glcmCalc->Compute();
        
    typename FeatureNameVector::ConstIterator fnameIt;
    for(fnameIt = m_RequestedFeatures->Begin(), featureNum = 0; 
        fnameIt != m_RequestedFeatures->End(); fnameIt++, featureNum++)
      {
      features[offsetNum][featureNum] = glcmCalc->GetFeature(fnameIt.Value());
      }
    }
      
  // Now get the mean and deviaton of each feature across the offsets.
  m_FeatureMeans->clear();
  m_FeatureStandardDeviations->clear();
  double *tempFeatureMeans = new double [numFeatures];
  double *tempFeatureDevs = new double [numFeatures];
      
  /*Compute incremental mean and SD, a la Knuth, "The  Art of Computer 
    Programming, Volume 2: Seminumerical Algorithms",  section 4.2.2. 
    Compute mean and standard deviation using the recurrence relation:
    M(1) = x(1), M(k) = M(k-1) + (x(k) - M(k-1) ) / k
    S(1) = 0, S(k) = S(k-1) + (x(k) - M(k-1)) * (x(k) - M(k))
    for 2 <= k <= n, then
    sigma = vcl_sqrt(S(n) / n) (or divide by n-1 for sample SD instead of
    population SD).
  */
      
  // Set up the initial conditions (k = 1)
  for (featureNum = 0; featureNum < numFeatures; featureNum++)
    {
    tempFeatureMeans[featureNum] = features[0][featureNum];
    tempFeatureDevs[featureNum] = 0;
    }
  // Run through the recurrence (k = 2 ... N)
  for (offsetNum = 1; offsetNum < numOffsets; offsetNum++)
    {
    int k = offsetNum + 1;
    for (featureNum = 0; featureNum < numFeatures; featureNum++)
      {
      double M_k_minus_1 = tempFeatureMeans[featureNum];
      double S_k_minus_1 = tempFeatureDevs[featureNum];
      double x_k = features[offsetNum][featureNum];
          
      double M_k = M_k_minus_1 + (x_k - M_k_minus_1) / k;
      double S_k = S_k_minus_1 + (x_k - M_k_minus_1) * (x_k - M_k);
          
      tempFeatureMeans[featureNum] = M_k;
      tempFeatureDevs[featureNum] = S_k;
      }
    }
  for (featureNum = 0; featureNum < numFeatures; featureNum++)
    {
    tempFeatureDevs[featureNum] = vcl_sqrt(tempFeatureDevs[featureNum] / numOffsets);
        
    m_FeatureMeans->push_back(tempFeatureMeans[featureNum]);
    m_FeatureStandardDeviations->push_back(tempFeatureDevs[featureNum]);
    }
  delete [] tempFeatureMeans;
  delete [] tempFeatureDevs;
  for(int i=0; i < numOffsets; i++)
    {
    delete [] features[i];
    }
  delete[] features;
}
    
template< class TImage, class THistogramFrequencyContainer >
void
ScalarImageTextureCalculator< TImage, THistogramFrequencyContainer >::    
FastCompute(void)
{      
  // For each offset, calculate each feature
  typename OffsetVector::ConstIterator offsetIt;      
  for(offsetIt = m_Offsets->Begin(); offsetIt != m_Offsets->End(); offsetIt++)
    {
    m_GLCMGenerator->SetOffset(offsetIt.Value());
    }
  
  m_GLCMGenerator->Compute();
  typename GLCMCalculatorType::Pointer glcmCalc = GLCMCalculatorType::New();
  glcmCalc->SetHistogram(m_GLCMGenerator->GetOutput());
  glcmCalc->Compute();
  
  m_FeatureMeans->clear();
  m_FeatureStandardDeviations->clear();
  typename FeatureNameVector::ConstIterator fnameIt;
  for(fnameIt = m_RequestedFeatures->Begin(); 
      fnameIt != m_RequestedFeatures->End(); fnameIt++)
    {
    m_FeatureMeans->push_back(glcmCalc->GetFeature(fnameIt.Value()));
    m_FeatureStandardDeviations->push_back(0.0);
    }
}


template< class TImage, class THistogramFrequencyContainer >
void
ScalarImageTextureCalculator< TImage, THistogramFrequencyContainer >::    
SetInput( const ImageType * inputImage )
{
  itkDebugMacro("setting Input to " << inputImage);
  m_GLCMGenerator->SetInput(inputImage);
  this->Modified();
}
    
template< class TImage, class THistogramFrequencyContainer >
void
ScalarImageTextureCalculator< TImage, THistogramFrequencyContainer >::    
SetNumberOfBinsPerAxis( unsigned int numberOfBins )
{
  itkDebugMacro("setting NumberOfBinsPerAxis to " << numberOfBins);
  m_GLCMGenerator->SetNumberOfBinsPerAxis(numberOfBins);
  this->Modified();
}
    
template< class TImage, class THistogramFrequencyContainer >
void
ScalarImageTextureCalculator< TImage, THistogramFrequencyContainer >::    
SetPixelValueMinMax( PixelType min, PixelType max )
{
  itkDebugMacro("setting Min to " << min << "and Max to " << max);
  m_GLCMGenerator->SetPixelValueMinMax(min, max);
  this->Modified();
}
    
template< class TImage, class THistogramFrequencyContainer >
void
ScalarImageTextureCalculator< TImage, THistogramFrequencyContainer >::    
SetImageMask( const ImageType* imageMask)
{
  itkDebugMacro("setting ImageMask to " << imageMask);
  m_GLCMGenerator->SetImageMask(imageMask);
  this->Modified();
}
    
template< class TImage, class THistogramFrequencyContainer >
void
ScalarImageTextureCalculator< TImage, THistogramFrequencyContainer >::
SetInsidePixelValue(PixelType insidePixelValue)
{
  itkDebugMacro("setting InsidePixelValue to " << insidePixelValue);
  m_GLCMGenerator->SetInsidePixelValue(insidePixelValue);
  this->Modified();
}
    
template< class TImage, class THistogramFrequencyContainer >
void
ScalarImageTextureCalculator< TImage, THistogramFrequencyContainer >::    
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}
    
} // end of namespace Statistics 

} // end of namespace itk 


#endif
