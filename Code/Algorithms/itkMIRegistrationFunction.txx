/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMIRegistrationFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMIRegistrationFunction_txx_
#define _itkMIRegistrationFunction_txx_

#include "itkMIRegistrationFunction.h"
#include "itkImageRandomIteratorWithIndex.h"
#include "itkExceptionObject.h"
#include "vnl/vnl_math.h"

#include <vnl/vnl_matrix.h>

namespace itk {

/*
 * Default constructor
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
MIRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::MIRegistrationFunction()
{

  RadiusType r;
  unsigned int j;
  m_NumberOfSamples=1;
  m_NumberOfBins=4;

  for( j = 0; j < ImageDimension; j++ )
    {
    r[j] = 2;
    m_NumberOfSamples*=(r[j]*2+1);
    }
  this->SetRadius(r);

  m_MetricTotal=0.0;

  m_TimeStep = 1.0;
  m_minnorm=1.0;
  m_DenominatorThreshold = 1e-9;
  m_IntensityDifferenceThreshold = 0.001;
  m_MovingImage = NULL;
  m_FixedImage = NULL;
  m_FixedImageSpacing = NULL;
  m_FixedImageOrigin = NULL;
  m_FixedImageGradientCalculator = GradientCalculatorType::New();

  m_DoInverse = true;
  m_DoInverse = false;

  if (m_DoInverse) 
    m_MovingImageGradientCalculator = GradientCalculatorType::New();


  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_MovingImageInterpolator = static_cast<InterpolatorType*>(
    interp.GetPointer() );


}


/*
 * Standard "PrintSelf" method.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
MIRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PrintSelf(std::ostream& os, Indent indent) const
{/*
  
  Superclass::PrintSelf(os, indent);

  os << indent << "MovingImageIterpolator: ";
  os << m_MovingImageInterpolator.GetPointer() << std::endl;
  os << indent << "FixedImageGradientCalculator: ";
  os << m_FixedImageGradientCalculator.GetPointer() << std::endl;
  os << indent << "DenominatorThreshold: ";
  os << m_DenominatorThreshold << std::endl;
  os << indent << "IntensityDifferenceThreshold: ";
  os << m_IntensityDifferenceThreshold << std::endl;
*/
}


/*
 * Set the function state values before each iteration
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
MIRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::InitializeIteration()
{
  if( !m_MovingImage || !m_FixedImage || !m_MovingImageInterpolator )
    {
    itkExceptionMacro( << "MovingImage, FixedImage and/or Interpolator not set" );
    throw ExceptionObject(__FILE__,__LINE__);
    }

  // cache fixed image information
  m_FixedImageSpacing    = m_FixedImage->GetSpacing();
  m_FixedImageOrigin     = m_FixedImage->GetOrigin();

  // setup gradient calculator
  m_FixedImageGradientCalculator->SetInputImage( m_FixedImage );

  if (m_DoInverse)
  {
    // setup gradient calculator
    m_MovingImageGradientCalculator->SetInputImage( m_MovingImage );
    std::cout  << " Doing inverse metric " << std::endl;
  }
  // setup moving image interpolator
  m_MovingImageInterpolator->SetInputImage( m_MovingImage );

  std::cout << " total metric " << m_MetricTotal << std::endl;
  m_MetricTotal=0.0;

}


/*
 * Compute update at a non boundary neighbourhood
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
typename MIRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PixelType
MIRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::ComputeUpdate(const NeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{
// we compute the derivative of MI w.r.t. the infinitesimal 
// displacement, following viola and wells. 

// 1)  collect samples from  M (Moving) and F (Fixed) 
// 2)  compute minimum and maximum values of M and F 
// 3)  discretized M and F into N bins
// 4)  estimate joint probability P(M,F) and P(F)
// 5)  derivatives is given as :  
//
//  $$ \nabla MI = \frac{1}{N} \sum_i \sum_j (F_i-F_j)
//    ( W(F_i,F_j) \frac{1}{\sigma_v} -
//                W((F_i,M_i),(F_j,M_j)) \frac{1}{\sigma_vv} ) \nabla F
//
// NOTE : must estimate sigma for each pdf

  if (m_DoInverse) 
    return ComputeForwardAndInverseUpdate(it,globalData,offset);


  typedef vnl_matrix<double>        matrixType;
  typedef std::vector<double>      sampleContainerType;
  typedef std::vector<CovariantVectorType> gradContainerType;
  typedef std::vector<double> gradMagContainerType;
  typedef std::vector<unsigned int> inImageIndexContainerType;


  PixelType update;
  PixelType derivative;
  unsigned int j=0;

  IndexType oindex = it.GetIndex();

  unsigned int indct=0;
  unsigned int hoodlen=it.Size();

  for (indct=0;indct<ImageDimension;indct++)
  {
    update[indct]=0.0;
    derivative[indct]=0.0;
  }


  float thresh2=0.0; ///255.;//  FIX ME : FOR PET LUNG ONLY !!
  float thresh1=0.0; ///255.;
  if ( m_MovingImage->GetPixel(oindex) <= thresh1 &&
     m_FixedImage->GetPixel(oindex) <= thresh2 ) return update;

  typename FixedImageType::SizeType hradius=this->GetRadius();
 
  FixedImageType* img =const_cast<FixedImageType *>(m_FixedImage.GetPointer());
  typename FixedImageType::SizeType imagesize=img->GetLargestPossibleRegion().GetSize();
  

  bool inimage=false; 

// now collect the samples 
  sampleContainerType fixedSamplesA;
  sampleContainerType movingSamplesA;
  sampleContainerType fixedSamplesB;
  sampleContainerType movingSamplesB;
  inImageIndexContainerType inImageIndicesA;
  gradContainerType  fixedGradientsA;
  gradMagContainerType fixedGradMagsA;
  inImageIndexContainerType inImageIndicesB;
  gradContainerType  fixedGradientsB;
  gradMagContainerType fixedGradMagsB;
  
  unsigned int samplestep=2; //m_Radius[0];
 
  double minf=1.e9,minm=1.e9,maxf=0.0,maxm=0.0;
  double movingMean=0.0; 
  double fixedMean=0.0;
  double fixedValue=0,movingValue=0;

  unsigned int sampct=0;


// first get the density-related sample
  for(indct=0; indct<hoodlen; indct=indct+samplestep)
  {
    IndexType index=it.GetIndex(indct);
    inimage=true;
    for (unsigned int dd=0; dd<ImageDimension; dd++)
    {
      if ( index[dd] < 0 || index[dd] > imagesize[dd]-1 ) inimage=false;
    }
    if (inimage)
    {

      fixedValue=0.;
      movingValue=0.0;
      PointType mappedPoint;
      CovariantVectorType fixedGradient;

        // Get fixed image related information
      fixedValue = (double) m_FixedImage->GetPixel( index );
      for( j = 0; j < ImageDimension; j++ )
      {
        fixedGradient[j] = m_FixedImageGradientCalculator->EvaluateAtIndex( index, j );
      } 
      // Get moving image related information
      typename DeformationFieldType::PixelType itvec=m_DeformationField->GetPixel(index);

      for( j = 0; j < ImageDimension; j++ )
      {
        mappedPoint[j] = double( index[j] ) * m_FixedImageSpacing[j] + 
        m_FixedImageOrigin[j];
        mappedPoint[j] += itvec[j];
      }
      if( m_MovingImageInterpolator->IsInsideBuffer( mappedPoint ) )
      {
        movingValue = m_MovingImageInterpolator->Evaluate( mappedPoint );
      }
      else
      {
        movingValue = 0.0;
      }
      
      if (fixedValue > maxf) maxf=fixedValue;
      else if (fixedValue < minf) minf=fixedValue;
      if (movingValue > maxm) maxm=movingValue;
      else if (movingValue < minm) minm=movingValue;
      
      fixedMean+=fixedValue;
      movingMean+=movingValue;      

        fixedSamplesA.insert(fixedSamplesA.begin(),(double)fixedValue);
        fixedGradientsA.insert(fixedGradientsA.begin(),fixedGradient);
        movingSamplesA.insert(movingSamplesA.begin(),(double)movingValue);

        
//        fixedSamplesB.insert(fixedSamplesB.begin(),(double)fixedValue);
//        fixedGradientsB.insert(fixedGradientsB.begin(),fixedGradient);
//        movingSamplesB.insert(movingSamplesB.begin(),(double)movingValue);

      sampct++;
    }  
    
  }


// BEGIN RANDOM A SAMPLES
  bool getrandasamples=true;
  if (getrandasamples)
  {

  typename FixedImageType::RegionType region=img->GetLargestPossibleRegion();

  ImageRandomIteratorWithIndex<FixedImageType> it(img,region);
  unsigned int numberOfSamples=1000;
  it.SetNumberOfSamples( numberOfSamples ); 
  numberOfSamples=100;

  indct=0;
 
  it.GoToBegin();
  while( !it.IsAtEnd() &&  indct < numberOfSamples )
  {
    IndexType index=it.GetIndex();
    inimage=true;

    float d=0.0;
    for (unsigned int dd=0; dd<ImageDimension; dd++)
    {
      if ( index[dd] < 0 || index[dd] > imagesize[dd]-1 ) inimage=false;
      d+=(index[dd]-oindex[dd])*(index[dd]-oindex[dd]);
    }
    
    if (inimage )
    {
      fixedValue=0.;
      movingValue=0.0;
      PointType mappedPoint;
      CovariantVectorType fixedGradient;
      double fgm=0;
        // Get fixed image related information
      fixedValue = (double) m_FixedImage->GetPixel( index );
      for( j = 0; j < ImageDimension; j++ )
      {
        fixedGradient[j] = m_FixedImageGradientCalculator->EvaluateAtIndex( index, j );
        fgm+=fixedGradient[j] *fixedGradient[j];
      } 
      // Get moving image related information
      typename DeformationFieldType::PixelType itvec=m_DeformationField->GetPixel(index);

      for( j = 0; j < ImageDimension; j++ )
      {
        mappedPoint[j] = double( index[j] ) * m_FixedImageSpacing[j] + 
        m_FixedImageOrigin[j];
        mappedPoint[j] += itvec[j];
      }
      if( m_MovingImageInterpolator->IsInsideBuffer( mappedPoint ) )
      {
        movingValue = m_MovingImageInterpolator->Evaluate( mappedPoint );
      }
      else
      {
        movingValue = 0.0;
      }
      
  //      if ( (fixedValue > 0 || movingValue > 0 || fgm > 0) || !filtersamples)
  
      if ( fixedValue > 0 || movingValue > 0 || fgm > 0 )
      { 
      fixedMean+=fixedValue;
      movingMean+=movingValue;      

        fixedSamplesA.insert(fixedSamplesA.begin(),(double)fixedValue);
        fixedGradientsA.insert(fixedGradientsA.begin(),fixedGradient);
        movingSamplesA.insert(movingSamplesA.begin(),(double)movingValue);
        sampct++;
        indct++;
      }
      
    }
    ++it;  
  }

  }
// END RANDOM A SAMPLES




//std::cout << " num a sam " << fixedSamplesA.size() << std::endl;








  for (j=0;j<ImageDimension; j++) hradius[j]=0;
  NeighborhoodIterator<DeformationFieldType> 
    hoodIt( hradius , m_DeformationField,m_DeformationField->GetRequestedRegion());
  hoodIt.SetLocation(oindex);
 
// then get the entropy ( and MI derivative ) related sample
  for(indct=0; indct<hoodIt.Size(); indct=indct+1)
  {
    IndexType index=hoodIt.GetIndex(indct);
    inimage=true;
    float d=0.0;
    for (unsigned int dd=0; dd<ImageDimension; dd++)
    {
      if ( index[dd] < 0 || index[dd] > imagesize[dd]-1 ) inimage=false;
      d+=(index[dd]-oindex[dd])*(index[dd]-oindex[dd]);
    }
    if (inimage  && sqrt(d) <= 1.0)
    {

      fixedValue=0.;
      movingValue=0.0;
      PointType mappedPoint;
      CovariantVectorType fixedGradient;

        // Get fixed image related information
      fixedValue = (double) m_FixedImage->GetPixel( index );
      for(j = 0; j < ImageDimension; j++ )
      {
        fixedGradient[j] = m_FixedImageGradientCalculator->EvaluateAtIndex( index, j );
      } 
      // Get moving image related information


      // Get moving image related information
      typename DeformationFieldType::PixelType hooditvec=m_DeformationField->GetPixel(index);

      for( j = 0; j < ImageDimension; j++ )
      {
        mappedPoint[j] = double( index[j] ) * m_FixedImageSpacing[j] + 
        m_FixedImageOrigin[j];
        mappedPoint[j] += hooditvec[j];
      }
      if( m_MovingImageInterpolator->IsInsideBuffer( mappedPoint ) )
      {
        movingValue = m_MovingImageInterpolator->Evaluate( mappedPoint );
      }
      else
      {
        movingValue = 0.0;
      }
      

        fixedSamplesB.insert(fixedSamplesB.begin(),(double)fixedValue);
        fixedGradientsB.insert(fixedGradientsB.begin(),fixedGradient);
        movingSamplesB.insert(movingSamplesB.begin(),(double)movingValue);
/*        fixedSamplesA.insert(fixedSamplesA.begin(),(double)fixedValue);
        fixedGradientsA.insert(fixedGradientsA.begin(),fixedGradient);
        movingSamplesA.insert(movingSamplesA.begin(),(double)movingValue);
*/      
    }  
    
  }


  double fsigma=0.0;
  double msigma=0.0;
  double jointsigma=0.0;

  double numsamplesB= (double) fixedSamplesB.size();
  double numsamplesA= (double) fixedSamplesA.size();
  double nsamp=numsamplesB;
//  if (maxf == minf && maxm == minm) return update; 
//    else std::cout << " b samps " << fixedSamplesB.size() 
//    << " a samps " <<  fixedSamplesA.size() << 
//    oindex  << hoodIt.Size() << it.Size() << std::endl;
  
  fixedMean/=(double)sampct;
  movingMean/=(double)sampct;



  bool mattes=false;

  for(indct=0; indct<(unsigned int)numsamplesA; indct++)
  {
// Get fixed image related information
    fixedValue=fixedSamplesA[indct];
    movingValue=movingSamplesA[indct];

    fsigma+=(fixedValue-fixedMean)*(fixedValue-fixedMean);
    msigma+=(movingValue-movingMean)*(movingValue-movingMean);
    jointsigma+=fsigma+msigma;

    if (mattes)
    {
      fixedSamplesA[indct]=fixedSamplesA[indct]-minf;
      movingSamplesA[indct]=movingSamplesA[indct]-minm;
      if (indct < numsamplesB)
      {
        fixedSamplesB[indct]=fixedSamplesB[indct]-minf;
        movingSamplesB[indct]=movingSamplesB[indct]-minm;
      }
    } 
  }
  

  fsigma=sqrt(fsigma/numsamplesA);
  float sigmaw=0.8;
  double m_FixedImageStandardDeviation=fsigma*sigmaw;
  msigma=sqrt(msigma/numsamplesA);
  double m_MovingImageStandardDeviation=msigma*sigmaw;
  jointsigma=sqrt(jointsigma/numsamplesA);
  
  if (fsigma < 1.e-7 || msigma < 1.e-7 ) return update;


  double m_MinProbability = 0.0001;
  double dLogSumFixed=0.,dLogSumMoving=0.,dLogSumJoint=0.0;
  unsigned int bsamples=0;
  unsigned int asamples=0;

  // the B samples estimate the entropy
  for(bsamples=0; bsamples<(unsigned int)numsamplesB; bsamples++)
  {  
    double dDenominatorMoving = m_MinProbability;
    double dDenominatorJoint = m_MinProbability;
    double dDenominatorFixed = m_MinProbability;
    double dSumFixed = m_MinProbability;


  // this loop estimates the density
    for(asamples=0; asamples<(unsigned int)numsamplesA; asamples++)
    {  
      double valueFixed;
      double valueMoving;

      valueFixed = ( fixedSamplesB[bsamples] - fixedSamplesA[asamples] )
        / m_FixedImageStandardDeviation;
      valueFixed = exp(-0.5*valueFixed*valueFixed);

      valueMoving = ( movingSamplesB[bsamples] - movingSamplesA[asamples] )
        / m_MovingImageStandardDeviation;
      valueMoving = exp(-0.5*valueMoving*valueMoving);

      dDenominatorMoving += valueMoving;
      dDenominatorFixed += valueFixed;
      dSumFixed += valueFixed;
      
// everything above here can be pre-computed only once and stored, 
//  assuming const v.f. in small n-hood

      dDenominatorJoint += valueMoving * valueFixed;


      } // end of sample A loop

    dLogSumFixed -= log( dSumFixed );
    dLogSumMoving    -= log( dDenominatorMoving );
    dLogSumJoint  -= log( dDenominatorJoint );

     
  // this loop estimates the density
    for(asamples=0; asamples<(unsigned int)numsamplesA; asamples++)
    {  
  
      double valueFixed;
      double valueMoving;
      double weightFixed;
      double weightMoving;
      double weightJoint;
      double weight;

      valueFixed = ( fixedSamplesB[bsamples] - fixedSamplesA[asamples] )
        / m_FixedImageStandardDeviation;
      valueFixed = exp(-0.5*valueFixed*valueFixed);

      valueMoving = ( movingSamplesB[bsamples] - movingSamplesA[asamples] )
        / m_MovingImageStandardDeviation;
      valueMoving = exp(-0.5*valueMoving*valueMoving);

      weightMoving = valueMoving / dDenominatorMoving;
      weightFixed = valueFixed / dDenominatorFixed;


// dDenominatorJoint and weightJoint are what need to be computed each time
      weightJoint = valueMoving * valueFixed / dDenominatorJoint;

// begin where we may switch fixed and moving
      weight = ( weightFixed - weightJoint );
      weight *=  ( fixedSamplesB[bsamples] - fixedSamplesA[asamples] );
// end where we may switch fixed and moving

// this can also be stored away
      for (int i=0; i<ImageDimension;i++)
      derivative[i]+= ( fixedGradientsB[bsamples][i] - fixedGradientsA[asamples][i] ) * weight;

      } // end of sample A loop

    } // end of sample B loop

  double threshold = -0.1 * nsamp * log( m_MinProbability );
  if( dLogSumMoving > threshold || dLogSumFixed > threshold ||
      dLogSumJoint > threshold  )
    {
    // at least half the samples in B did not occur within
    // the Parzen window width of samples in A
    return update;
//    itkExceptionMacro(<<"Standard deviation is too small" );
    }

  double value=0.0;
  value  = dLogSumFixed + dLogSumMoving - dLogSumJoint;
  value /= nsamp;
  value += log( nsamp );

  m_MetricTotal+=value;  
  m_Energy+=value;

  derivative  /= nsamp;
  derivative  /= vnl_math_sqr( m_FixedImageStandardDeviation );

  double updatenorm=0.0;
  for (unsigned int tt=0; tt<ImageDimension; tt++)
  {
    updatenorm+=derivative[tt]*derivative[tt];
  }
  updatenorm=sqrt(updatenorm);
  
  if (updatenorm > 1.e-20 && m_NormalizeGradient) 
  {
    derivative=derivative/updatenorm;
  } 
  
  if (m_DoInverse)
  { 
    PixelType invderivative = ComputeInverseUpdate( it, globalData, offset);

    float idmag=0.0;
    for (j=0; j<ImageDimension; j++)
    {
      derivative[j]+=invderivative[j];
    }
//    if (idmag > 0.) std::cout << " inv deriv " << invderivative << std::endl;
//    if (idmag > 0.) std::cout << " deriv " << derivative << std::endl;
    derivative/=2.0;
  }

  return derivative/m_GradientStep;
}





/*
 * Compute inverse update at a non boundary neighbourhood
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
typename MIRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PixelType
MIRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::ComputeInverseUpdate(const NeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{
// we compute the derivative of MI w.r.t. the infinitesimal 
// displacement, following viola and wells. 

// 1)  collect samples from  M (Moving) and F (Fixed) 
// 2)  compute minimum and maximum values of M and F 
// 3)  discretized M and F into N bins
// 4)  estimate joint probability P(M,F) and P(F)
// 5)  derivatives is given as :  
//
//  $$ \nabla MI = \frac{1}{N} \sum_i \sum_j (F_i-F_j)
//    ( W(F_i,F_j) \frac{1}{\sigma_v} -
//                W((F_i,M_i),(F_j,M_j)) \frac{1}{\sigma_vv} ) \nabla F
//
// NOTE : must estimate sigma for each pdf


  typedef vnl_matrix<double>        matrixType;
  typedef std::vector<double>      sampleContainerType;
  typedef std::vector<CovariantVectorType> gradContainerType;
  typedef std::vector<double> gradMagContainerType;
  typedef std::vector<unsigned int> inImageIndexContainerType;


  PixelType update;
  PixelType derivative;
  unsigned int j;

  IndexType oindex = it.GetIndex();

  unsigned int indct=0;
  unsigned int hoodlen=it.Size();

  for (indct=0;indct<ImageDimension;indct++)
  {
    update[indct]=0.0;
    derivative[indct]=0.0;
  }

  float thresh2=59.0/255.;//  FIX ME : FOR PET LUNG ONLY !!
  float thresh1=2.0/255.;
  if ( m_MovingImage->GetPixel(oindex) < thresh1 &&
     m_FixedImage->GetPixel(oindex) < thresh2 ) return update;

  typename FixedImageType::SizeType hradius=this->GetRadius();
 
  FixedImageType* img =const_cast<FixedImageType *>(m_FixedImage.GetPointer());
  typename FixedImageType::SizeType imagesize=img->GetLargestPossibleRegion().GetSize();
  

  bool inimage=false; 

// now collect the samples 
  sampleContainerType fixedSamplesA;
  sampleContainerType movingSamplesA;
  sampleContainerType fixedSamplesB;
  sampleContainerType movingSamplesB;
  inImageIndexContainerType inImageIndicesA;
  gradContainerType  movingGradientsA;
  gradMagContainerType fixedGradMagsA;
  inImageIndexContainerType inImageIndicesB;
  gradContainerType  movingGradientsB;
  gradMagContainerType fixedGradMagsB;
  
  unsigned int samplestep=2; //m_Radius[0];
 
  double minf=1.e9,minm=1.e9,maxf=0.0,maxm=0.0;
  double movingMean=0.0; 
  double fixedMean=0.0;
  double fixedValue=0,movingValue=0;

  unsigned int sampct=0;

  
  // first get the density-related sample
  for(indct=0; indct<hoodlen; indct=indct+samplestep)
  {
    IndexType index=it.GetIndex(indct);
    inimage=true;
    for (unsigned int dd=0; dd<ImageDimension; dd++)
    {
      if ( index[dd] < 0 || index[dd] > imagesize[dd]-1 ) inimage=false;
    }
    if (inimage)
    {

      fixedValue=0.;
      movingValue=0.0;
      PointType mappedPoint;
      CovariantVectorType movingGradient;
  
        // Get fixed image related information
      fixedValue = (double) m_FixedImage->GetPixel( index );
      // Get moving image related information
      typename DeformationFieldType::PixelType itvec=m_DeformationField->GetPixel(index);

      IndexType mappedIndex;
      for( j = 0; j < ImageDimension; j++ )
      {
        mappedPoint[j] = double( index[j] ) * m_FixedImageSpacing[j] + 
        m_FixedImageOrigin[j];
        mappedPoint[j] += itvec[j];
        mappedIndex[j] = index[j]+(unsigned long) (itvec[j]+0.5);
      }
      if( m_MovingImageInterpolator->IsInsideBuffer( mappedPoint ) )
      {
        movingValue = m_MovingImageInterpolator->Evaluate( mappedPoint );
        for( j = 0; j < ImageDimension; j++ )
        {
//          std::cout << " mappedIndex " << mappedIndex << std::endl;
//          std::cout << " mappedpoint " << mappedPoint << std::endl;
          movingGradient[j] = m_MovingImageGradientCalculator->EvaluateAtIndex( mappedIndex, j );
//          if (movingGradient[j] != 0.0) std::cout <<  " gradient not zero " << std::endl;
        } 
      }
      else
      {
        movingValue = 0.0;        
        for( j = 0; j < ImageDimension; j++ )
        {
          movingGradient[j] = 0;
        } 
      }
      
      if (fixedValue > maxf) maxf=fixedValue;
      else if (fixedValue < minf) minf=fixedValue;
      if (movingValue > maxm) maxm=movingValue;
      else if (movingValue < minm) minm=movingValue;
      
      fixedMean+=fixedValue;
      movingMean+=movingValue;      

      fixedSamplesA.insert(fixedSamplesA.begin(),(double)fixedValue);
      movingGradientsA.insert(movingGradientsA.begin(),movingGradient);
      movingSamplesA.insert(movingSamplesA.begin(),(double)movingValue);

      sampct++;
    }  
    
  }



  for (j=0;j<ImageDimension; j++) hradius[j]=1;
  NeighborhoodIterator<DeformationFieldType> 
    hoodIt( hradius , m_DeformationField,m_DeformationField->GetRequestedRegion());
  hoodIt.SetLocation(oindex);
 
// then get the entropy ( and MI derivative ) related sample
  for(indct=0; indct<hoodIt.Size(); indct=indct+1)
  {
    IndexType index=hoodIt.GetIndex(indct);
    inimage=true;
    float d=0.0;
    for (unsigned int dd=0; dd<ImageDimension; dd++)
    {
      if ( index[dd] < 0 || index[dd] > imagesize[dd]-1 ) inimage=false;
      d+=(index[dd]-oindex[dd])*(index[dd]-oindex[dd]);
    }
    if (inimage  && sqrt(d) <= 1.0)
    {

      fixedValue=0.;
      movingValue=0.0;
      PointType mappedPoint;
      CovariantVectorType movingGradient;

        // Get fixed image related information
      fixedValue = (double) m_FixedImage->GetPixel( index );
      // Get moving image related information
      typename DeformationFieldType::PixelType hooditvec=m_DeformationField->GetPixel(index);

      IndexType mappedIndex;
      for( j = 0; j < ImageDimension; j++ )
      {
        mappedPoint[j] = double( index[j] ) * m_FixedImageSpacing[j] + 
        m_FixedImageOrigin[j];
        mappedPoint[j] += hooditvec[j];
        mappedIndex[j] = index[j]+(unsigned long) (hooditvec[j]+0.5);
      }
      if( m_MovingImageInterpolator->IsInsideBuffer( mappedPoint ) )
      {
        movingValue = m_MovingImageInterpolator->Evaluate( mappedPoint );
        for( j = 0; j < ImageDimension; j++ )
        {
          movingGradient[j] = m_MovingImageGradientCalculator->EvaluateAtIndex( mappedIndex, j );
        } 
      }
      else
      {
        movingValue = 0.0;        
        for( j = 0; j < ImageDimension; j++ )
        {
          movingGradient[j] = 0;
        } 
      }
      

      fixedSamplesB.insert(fixedSamplesB.begin(),(double)fixedValue);
      movingGradientsB.insert(movingGradientsB.begin(),movingGradient);
      movingSamplesB.insert(movingSamplesB.begin(),(double)movingValue);
    }  
    
  }


  double fsigma=0.0;
  double msigma=0.0;
  double jointsigma=0.0;

  double numsamplesB= (double) fixedSamplesB.size();
  double numsamplesA= (double) fixedSamplesA.size();
  double nsamp=numsamplesB;
  if (maxf == minf && maxm == minm) return update; 
//    else std::cout << " b samps " << fixedSamplesB.size() 
//    << " a samps " <<  fixedSamplesA.size() << 
//    oindex  << hoodIt.Size() << it.Size() << std::endl;
  
  fixedMean/=(double)sampct;
  movingMean/=(double)sampct;



  bool mattes=false;

  for(indct=0; indct<(unsigned int)numsamplesA; indct++)
  {
// Get fixed image related information
    fixedValue=fixedSamplesA[indct];
    movingValue=movingSamplesA[indct];

    fsigma+=(fixedValue-fixedMean)*(fixedValue-fixedMean);
    msigma+=(movingValue-movingMean)*(movingValue-movingMean);
    jointsigma+=fsigma+msigma;

    if (mattes)
    {
      fixedSamplesA[indct]=fixedSamplesA[indct]-minf;
      movingSamplesA[indct]=movingSamplesA[indct]-minm;
      if (indct < numsamplesB)
      {
        fixedSamplesB[indct]=fixedSamplesB[indct]-minf;
        movingSamplesB[indct]=movingSamplesB[indct]-minm;
      }
    } 
  }
  

  fsigma=sqrt(fsigma/numsamplesA);
  float sigmaw=0.8;
  double m_FixedImageStandardDeviation=fsigma*sigmaw;
  msigma=sqrt(msigma/numsamplesA);
  double m_MovingImageStandardDeviation=msigma*sigmaw;
  jointsigma=sqrt(jointsigma/numsamplesA);
  
  if (fsigma < 1.e-4 || msigma < 1.e-4 ) return update;


  double m_MinProbability = 0.0001;
  double dLogSumFixed=0.,dLogSumMoving=0.,dLogSumJoint=0.0;
  unsigned int bsamples=0;
  unsigned int asamples=0;

  // the B samples estimate the entropy
  for(bsamples=0; bsamples<(unsigned int)numsamplesB; bsamples++)
  {  
    double dDenominatorMoving = m_MinProbability;
    double dDenominatorJoint = m_MinProbability;
    double dDenominatorFixed = m_MinProbability;
    double dSumMoving = m_MinProbability;


  // this loop estimates the density
    for(asamples=0; asamples<(unsigned int)numsamplesA; asamples++)
    {  
      double valueFixed;
      double valueMoving;

      valueFixed = ( fixedSamplesB[bsamples] - fixedSamplesA[asamples] )
        / m_FixedImageStandardDeviation;
      valueFixed = exp(-0.5*valueFixed*valueFixed);

      valueMoving = ( movingSamplesB[bsamples] - movingSamplesA[asamples] )
        / m_MovingImageStandardDeviation;
      valueMoving = exp(-0.5*valueMoving*valueMoving);

      dDenominatorMoving += valueMoving;
      dDenominatorFixed += valueFixed;
      dSumMoving += valueMoving;
      
// everything above here can be pre-computed only once and stored, 
//  assuming const v.f. in small n-hood

      dDenominatorJoint += valueMoving * valueFixed;


      } // end of sample A loop

    dLogSumMoving -= log( dSumMoving );
    dLogSumFixed    -= log( dDenominatorFixed );
    dLogSumJoint  -= log( dDenominatorJoint );

     
  // this loop estimates the density
    for(asamples=0; asamples<(unsigned int)numsamplesA; asamples++)
    {  
  
      double valueFixed;
      double valueMoving;
      double weightFixed;
      double weightMoving;
      double weightJoint;
      double weight;

      valueFixed = ( fixedSamplesB[bsamples] - fixedSamplesA[asamples] )
        / m_FixedImageStandardDeviation;
      valueFixed = exp(-0.5*valueFixed*valueFixed);

      valueMoving = ( movingSamplesB[bsamples] - movingSamplesA[asamples] )
        / m_MovingImageStandardDeviation;
      valueMoving = exp(-0.5*valueMoving*valueMoving);

      weightMoving = valueMoving / dDenominatorMoving;
      weightFixed = valueFixed / dDenominatorFixed;


// dDenominatorJoint and weightJoint are what need to be computed each time
      weightJoint = valueMoving * valueFixed / dDenominatorJoint;

// begin where we may switch fixed and moving
      weight = ( weightMoving - weightJoint );
      weight *=  ( movingSamplesB[bsamples] - movingSamplesA[asamples] );
// end where we may switch fixed and moving

// this can also be stored away
      for (int i=0; i<ImageDimension;i++)
      derivative[i]+= ( movingGradientsB[bsamples][i] - movingGradientsA[asamples][i] ) * weight;

      } // end of sample A loop

    } // end of sample B loop

  double threshold = -0.5 * nsamp * log( m_MinProbability );
  if( dLogSumMoving > threshold || dLogSumFixed > threshold ||
      dLogSumJoint > threshold  )
    {
    // at least half the samples in B did not occur within
    // the Parzen window width of samples in A
    return update;
//    itkExceptionMacro(<<"Standard deviation is too small" );
    }

  double value=0.0;
  value  = dLogSumFixed + dLogSumMoving - dLogSumJoint;
  value /= nsamp;
  value += log( nsamp );


  derivative  /= nsamp;
  derivative  /= vnl_math_sqr( m_FixedImageStandardDeviation );

  double updatenorm=0.0;
  for (unsigned int tt=0; tt<ImageDimension; tt++)
  {
    updatenorm+=derivative[tt]*derivative[tt];
  }
  updatenorm=sqrt(updatenorm);
  if (updatenorm > 1.e-20 && m_NormalizeGradient) 
  {
    derivative=derivative*1.0/(m_GradientStep*updatenorm);
  }
  
  return derivative/m_GradientStep;
}





/*
 * Compute inverse update at a non boundary neighbourhood
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
typename MIRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PixelType
MIRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::ComputeForwardAndInverseUpdate(const NeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{
// we compute the derivative of MI w.r.t. the infinitesimal 
// displacement, following viola and wells. 

// 1)  collect samples from  M (Moving) and F (Fixed) 
// 2)  compute minimum and maximum values of M and F 
// 3)  discretized M and F into N bins
// 4)  estimate joint probability P(M,F) and P(F)
// 5)  derivatives is given as :  
//
//  $$ \nabla MI = \frac{1}{N} \sum_i \sum_j (F_i-F_j)
//    ( W(F_i,F_j) \frac{1}{\sigma_v} -
//                W((F_i,M_i),(F_j,M_j)) \frac{1}{\sigma_vv} ) \nabla F
//
// NOTE : must estimate sigma for each pdf


  typedef vnl_matrix<double>        matrixType;
  typedef std::vector<double>      sampleContainerType;
  typedef std::vector<CovariantVectorType> gradContainerType;
  typedef std::vector<double> gradMagContainerType;
  typedef std::vector<unsigned int> inImageIndexContainerType;


  PixelType update;
  PixelType derivative;
  PixelType invderivative;
  unsigned int j;

  IndexType oindex = it.GetIndex();

  unsigned int indct=0;
  unsigned int hoodlen=it.Size();

  for (indct=0;indct<ImageDimension;indct++)
  {
    update[indct]=0.0;
    derivative[indct]=0.0;
  }


  float thresh1=2.0/255.;
  float thresh2=2.0/255.;
  if ( m_MovingImage->GetPixel(oindex) < thresh1 &&
     m_FixedImage->GetPixel(oindex) < thresh2 ) return update;

  typename FixedImageType::SizeType hradius=this->GetRadius();
 
  FixedImageType* img =const_cast<FixedImageType *>(m_FixedImage.GetPointer());
  typename FixedImageType::SizeType imagesize=img->GetLargestPossibleRegion().GetSize();
  

  bool inimage=false; 

// now collect the samples 
  sampleContainerType fixedSamplesA;
  sampleContainerType movingSamplesA;
  sampleContainerType fixedSamplesB;
  sampleContainerType movingSamplesB;
  inImageIndexContainerType inImageIndicesA;
  gradContainerType  movingGradientsA;
  gradContainerType  fixedGradientsA;
  inImageIndexContainerType inImageIndicesB;
  gradContainerType  movingGradientsB;
  gradContainerType  fixedGradientsB;
  
  unsigned int samplestep=2; //m_Radius[0];
 
  double minf=1.e9,minm=1.e9,maxf=0.0,maxm=0.0;
  double movingMean=0.0; 
  double fixedMean=0.0;
  double fixedValue=0,movingValue=0;

  unsigned int sampct=0;

  
  // first get the density-related sample
  for(indct=0; indct<hoodlen; indct=indct+samplestep)
  {
    IndexType index=it.GetIndex(indct);
    inimage=true;
    for (unsigned int dd=0; dd<ImageDimension; dd++)
    {
      if ( index[dd] < 0 || index[dd] > imagesize[dd]-1 ) inimage=false;
    }
    if (inimage)
    {

      fixedValue=0.;
      movingValue=0.0;
      PointType mappedPoint;
      CovariantVectorType movingGradient;
      CovariantVectorType fixedGradient;
  
        // Get fixed image related information
      fixedValue = (double) m_FixedImage->GetPixel( index );
      // Get moving image related information
      typename DeformationFieldType::PixelType itvec=m_DeformationField->GetPixel(index);

      IndexType mappedIndex;
      for( j = 0; j < ImageDimension; j++ )
      {
        mappedPoint[j] = double( index[j] ) * m_FixedImageSpacing[j] + 
        m_FixedImageOrigin[j];
        mappedPoint[j] += itvec[j];
        mappedIndex[j] = index[j]+(unsigned long) (itvec[j]+0.5);
        fixedGradient[j] = m_FixedImageGradientCalculator->EvaluateAtIndex( index, j );
      }
      if( m_MovingImageInterpolator->IsInsideBuffer( mappedPoint ) )
      {
        movingValue = m_MovingImageInterpolator->Evaluate( mappedPoint );
        for( j = 0; j < ImageDimension; j++ )
        {
          movingGradient[j] = m_MovingImageGradientCalculator->EvaluateAtIndex( mappedIndex, j );
        } 
      }
      else
      {
        movingValue = 0.0;        
        for( j = 0; j < ImageDimension; j++ )
        {
          movingGradient[j] = 0;
        } 
      }
      
      if (fixedValue > maxf) maxf=fixedValue;
      else if (fixedValue < minf) minf=fixedValue;
      if (movingValue > maxm) maxm=movingValue;
      else if (movingValue < minm) minm=movingValue;
      
      fixedMean+=fixedValue;
      movingMean+=movingValue;      

      fixedSamplesA.insert(fixedSamplesA.begin(),(double)fixedValue);
      movingGradientsA.insert(movingGradientsA.begin(),movingGradient);
      fixedGradientsA.insert(fixedGradientsA.begin(),fixedGradient);
      movingSamplesA.insert(movingSamplesA.begin(),(double)movingValue);

      sampct++;
    }  
    
  }



  for (j=0;j<ImageDimension; j++) hradius[j]=1;
  NeighborhoodIterator<DeformationFieldType> 
    hoodIt( hradius , m_DeformationField,m_DeformationField->GetRequestedRegion());
  hoodIt.SetLocation(oindex);
 
// then get the entropy ( and MI derivative ) related sample
  for(indct=0; indct<hoodIt.Size(); indct=indct+1)
  {
    IndexType index=hoodIt.GetIndex(indct);
    inimage=true;
    float d=0.0;
    for (unsigned int dd=0; dd<ImageDimension; dd++)
    {
      if ( index[dd] < 0 || index[dd] > imagesize[dd]-1 ) inimage=false;
      d+=(index[dd]-oindex[dd])*(index[dd]-oindex[dd]);
    }
    if (inimage  && sqrt(d) <= 1.0)
    {

      fixedValue=0.;
      movingValue=0.0;
      PointType mappedPoint;
      CovariantVectorType movingGradient;
      CovariantVectorType fixedGradient;

        // Get fixed image related information
      fixedValue = (double) m_FixedImage->GetPixel( index );
      // Get moving image related information
      typename DeformationFieldType::PixelType hooditvec=m_DeformationField->GetPixel(index);

      IndexType mappedIndex;
      for( j = 0; j < ImageDimension; j++ )
      {
        mappedPoint[j] = double( index[j] ) * m_FixedImageSpacing[j] + 
        m_FixedImageOrigin[j];
        mappedPoint[j] += hooditvec[j];
        mappedIndex[j] = index[j]+(unsigned long) (hooditvec[j]+0.5);
        fixedGradient[j] = m_FixedImageGradientCalculator->EvaluateAtIndex( index, j );
      }
      if( m_MovingImageInterpolator->IsInsideBuffer( mappedPoint ) )
      {
        movingValue = m_MovingImageInterpolator->Evaluate( mappedPoint );
        for( j = 0; j < ImageDimension; j++ )
        {
          movingGradient[j] = m_MovingImageGradientCalculator->EvaluateAtIndex( mappedIndex, j );
        } 
      }
      else
      {
        movingValue = 0.0;        
        for( j = 0; j < ImageDimension; j++ )
        {
          movingGradient[j] = 0;
        } 
      }
      

      fixedSamplesB.insert(fixedSamplesB.begin(),(double)fixedValue);
      movingGradientsB.insert(movingGradientsB.begin(),movingGradient);
      fixedGradientsB.insert(fixedGradientsB.begin(),fixedGradient);
      movingSamplesB.insert(movingSamplesB.begin(),(double)movingValue);
    }  
    
  }


  double fsigma=0.0;
  double msigma=0.0;
  double jointsigma=0.0;

  double numsamplesB= (double) fixedSamplesB.size();
  double numsamplesA= (double) fixedSamplesA.size();
  double nsamp=numsamplesB;
  if (maxf == minf && maxm == minm) return update; 
//    else std::cout << " b samps " << fixedSamplesB.size() 
//    << " a samps " <<  fixedSamplesA.size() << 
//    oindex  << hoodIt.Size() << it.Size() << std::endl;
  
  fixedMean/=(double)sampct;
  movingMean/=(double)sampct;



  bool mattes=false;

  for(indct=0; indct<(unsigned int)numsamplesA; indct++)
  {
// Get fixed image related information
    fixedValue=fixedSamplesA[indct];
    movingValue=movingSamplesA[indct];

    fsigma+=(fixedValue-fixedMean)*(fixedValue-fixedMean);
    msigma+=(movingValue-movingMean)*(movingValue-movingMean);
    jointsigma+=fsigma+msigma;

    if (mattes)
    {
      fixedSamplesA[indct]=fixedSamplesA[indct]-minf;
      movingSamplesA[indct]=movingSamplesA[indct]-minm;
      if (indct < numsamplesB)
      {
        fixedSamplesB[indct]=fixedSamplesB[indct]-minf;
        movingSamplesB[indct]=movingSamplesB[indct]-minm;
      }
    } 
  }
  

  fsigma=sqrt(fsigma/numsamplesA);
  float sigmaw=1.0;
  double m_FixedImageStandardDeviation=fsigma*sigmaw;
  msigma=sqrt(msigma/numsamplesA);
  double m_MovingImageStandardDeviation=msigma*sigmaw;
  jointsigma=sqrt(jointsigma/numsamplesA);
  
  if (fsigma < 1.e-4 || msigma < 1.e-4 ) return update;


  double m_MinProbability = 0.0001;
  double dLogSumFixed=0.,dLogSumMoving=0.,dLogSumJoint=0.0;
  unsigned int bsamples=0;
  unsigned int asamples=0;

  // the B samples estimate the entropy
  for(bsamples=0; bsamples<(unsigned int)numsamplesB; bsamples++)
  {  
    double dDenominatorMoving = m_MinProbability;
    double dDenominatorJoint = m_MinProbability;
    double dDenominatorFixed = m_MinProbability;
    double dSumMoving = m_MinProbability;


  // this loop estimates the density
    for(asamples=0; asamples<(unsigned int)numsamplesA; asamples++)
    {  
      double valueFixed;
      double valueMoving;

      valueFixed = ( fixedSamplesB[bsamples] - fixedSamplesA[asamples] )
        / m_FixedImageStandardDeviation;
      valueFixed = exp(-0.5*valueFixed*valueFixed);

      valueMoving = ( movingSamplesB[bsamples] - movingSamplesA[asamples] )
        / m_MovingImageStandardDeviation;
      valueMoving = exp(-0.5*valueMoving*valueMoving);

      dDenominatorMoving += valueMoving;
      dDenominatorFixed += valueFixed;
      dSumMoving += valueMoving;
      
// everything above here can be pre-computed only once and stored, 
//  assuming const v.f. in small n-hood

      dDenominatorJoint += valueMoving * valueFixed;


      } // end of sample A loop

    dLogSumMoving -= log( dSumMoving );
    dLogSumFixed    -= log( dDenominatorFixed );
    dLogSumJoint  -= log( dDenominatorJoint );

     
  // this loop estimates the density
    for(asamples=0; asamples<(unsigned int)numsamplesA; asamples++)
    {  
  
      double valueFixed;
      double valueMoving;
      double weightFixed;
      double weightMoving;
      double weightJoint;
      double invweight;
      double weight;

      valueFixed = ( fixedSamplesB[bsamples] - fixedSamplesA[asamples] )
        / m_FixedImageStandardDeviation;
      valueFixed = exp(-0.5*valueFixed*valueFixed);

      valueMoving = ( movingSamplesB[bsamples] - movingSamplesA[asamples] )
        / m_MovingImageStandardDeviation;
      valueMoving = exp(-0.5*valueMoving*valueMoving);

      weightMoving = valueMoving / dDenominatorMoving;
      weightFixed = valueFixed / dDenominatorFixed;


// dDenominatorJoint and weightJoint are what need to be computed each time
      weightJoint = valueMoving * valueFixed / dDenominatorJoint;

// begin where we may switch fixed and moving
      invweight = ( weightMoving - weightJoint );
      invweight *=  ( movingSamplesB[bsamples] - movingSamplesA[asamples] );
      weight = ( weightFixed - weightJoint );
      weight *=  ( fixedSamplesB[bsamples] - fixedSamplesA[asamples] );
// end where we may switch fixed and moving


// this can also be stored away
      for (int i=0; i<ImageDimension;i++)
      {
        invderivative[i]+= ( movingGradientsB[bsamples][i] - movingGradientsA[asamples][i] ) * invweight; 
        derivative[i]+= ( fixedGradientsB[bsamples][i] - fixedGradientsA[asamples][i] ) * weight;
      }

      } // end of sample A loop

    } // end of sample B loop

  double threshold = -0.5 * nsamp * log( m_MinProbability );
  if( dLogSumMoving > threshold || dLogSumFixed > threshold ||
      dLogSumJoint > threshold  )
    {
    // at least half the samples in B did not occur within
    // the Parzen window width of samples in A
    return update;
//    itkExceptionMacro(<<"Standard deviation is too small" );
    }

  double value=0.0;
  value  = dLogSumFixed + dLogSumMoving - dLogSumJoint;
  value /= nsamp;
  value += log( nsamp );

  m_MetricTotal+=value;  
  m_Energy+=value;

  invderivative  /= nsamp;
  invderivative  /= vnl_math_sqr( m_FixedImageStandardDeviation );

  derivative  /= nsamp;
  derivative  /= vnl_math_sqr( m_FixedImageStandardDeviation );

  double invupdatenorm=0.0;
  double updatenorm=0.0;
  for (unsigned int tt=0; tt<ImageDimension; tt++)
  {
    invupdatenorm+=invderivative[tt]*invderivative[tt];
    updatenorm+=derivative[tt]*derivative[tt];
  }
  updatenorm=sqrt(updatenorm);
  invupdatenorm=sqrt(invupdatenorm);


  if (updatenorm > 1.e-20 && m_NormalizeGradient) 
  {

    invderivative=invderivative*1.0/(m_GradientStep*invupdatenorm);
    derivative=derivative*1.0/(m_GradientStep*updatenorm);
 //   std::cout << " invupdate " << invderivative << std::endl;
 //   std::cout << " update " << derivative << std::endl;

  } else return invderivative*(0.0);
  
  return ((derivative-invderivative)/m_GradientStep); // BUG FIXME
}



} // end namespace itk

#endif
