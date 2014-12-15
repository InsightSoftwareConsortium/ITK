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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkBayesianClassifierInitializationImageFilter_hxx
#define itkBayesianClassifierInitializationImageFilter_hxx

#include "itkBayesianClassifierInitializationImageFilter.h"
#include "itkScalarImageKmeansImageFilter.h"

#include "itkGaussianMembershipFunction.h"

namespace itk
{
template< typename TInputImage, typename TProbabilityPrecisionType >
BayesianClassifierInitializationImageFilter< TInputImage, TProbabilityPrecisionType >
::BayesianClassifierInitializationImageFilter():
  m_UserSuppliesMembershipFunctions(false),
  m_NumberOfClasses(0)
{
  m_MembershipFunctionContainer = ITK_NULLPTR;
}

// GenerateOutputInformation method. Here we force update on the entire input
// image. It does not make sense having K-Means etc otherwise.
template< typename TInputImage, typename TProbabilityPrecisionType >
void
BayesianClassifierInitializationImageFilter< TInputImage,
                                             TProbabilityPrecisionType >
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  typename OutputImageType::Pointer outputPtr = this->GetOutput();
  if ( !outputPtr )
    {
    return;
    }

  // Set the size of the output region
  outputPtr->SetBufferedRegion( this->GetInput()->GetLargestPossibleRegion() );
  outputPtr->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );

  if ( m_NumberOfClasses == 0 )
    {
    itkExceptionMacro(
      << "Number of classes unspecified");
    }
  outputPtr->SetVectorLength(m_NumberOfClasses);
}

template< typename TInputImage, typename TProbabilityPrecisionType >
void
BayesianClassifierInitializationImageFilter< TInputImage,
                                             TProbabilityPrecisionType >
::InitializeMembershipFunctions()
{
  // Typedefs for the KMeans filter, Covariance calculator...
  typedef ScalarImageKmeansImageFilter< InputImageType > KMeansFilterType;
  typedef typename KMeansFilterType::OutputImageType     KMeansOutputImageType;
  typedef ImageRegionConstIterator<
    KMeansOutputImageType >                ConstKMeansIteratorType;

  typedef Array< double > CovarianceArrayType;
  typedef Array< double > ClassCountArrayType;

  typedef Statistics::GaussianMembershipFunction<
    MeasurementVectorType >                        GaussianMembershipFunctionType;
  typedef VectorContainer< unsigned short, typename
                           GaussianMembershipFunctionType::MeanVectorType * >          MeanEstimatorsContainerType;
  typedef VectorContainer< unsigned short, typename
                           GaussianMembershipFunctionType::CovarianceMatrixType * >    CovarianceEstimatorsContainerType;

  // Run k means to get the means from the input image
  typename KMeansFilterType::Pointer kmeansFilter = KMeansFilterType::New();
  kmeansFilter->SetInput( this->GetInput() );
  kmeansFilter->SetUseNonContiguousLabels(false);

  for ( unsigned k = 0; k < m_NumberOfClasses; k++ )
    {
    const double userProvidedInitialMean = k;
    //TODO: Choose more reasonable defaults for specifying the initial means
    //to the KMeans filter. We could also add this as an option of the filter.
    kmeansFilter->AddClassWithInitialMean(userProvidedInitialMean);
    }

  try
    {
    kmeansFilter->Update();
    }
  catch ( ExceptionObject & err )
    {
    // Pass exception to caller
    throw err;
    }

  typename KMeansFilterType::ParametersType
  estimatedMeans = kmeansFilter->GetFinalMeans();         // mean of each class

  // find class covariances from the kmeans output to initialize the gaussian
  // density functions.
  ConstKMeansIteratorType itrKMeansImage( kmeansFilter->GetOutput(),
                                          kmeansFilter->GetOutput()->GetBufferedRegion() );
  CovarianceArrayType sumsOfSquares(m_NumberOfClasses);          // sum of the
                                                                 // square
                                                                 // intensities
                                                                 // for each
                                                                 // class
  CovarianceArrayType sums(m_NumberOfClasses);                   // sum of the
                                                                 // intensities
                                                                 // for each
                                                                 // class
  ClassCountArrayType classCount(m_NumberOfClasses);             // m_Number of
                                                                 // pixels
                                                                 // belonging to
                                                                 // each class
  CovarianceArrayType estimatedCovariances(m_NumberOfClasses);   // covariance
                                                                 // of each
                                                                 // class

  // initialize the arrays
  sumsOfSquares.Fill(0.0);
  sums.Fill(0.0);
  classCount.Fill(0);

  const InputImageType *inputImage = this->GetInput();
  typename InputImageType::RegionType imageRegion  = inputImage->GetLargestPossibleRegion();
  InputImageIteratorType itrInputImage(inputImage, imageRegion);

  itrInputImage.GoToBegin();
  itrKMeansImage.GoToBegin();

  // find sumsOfSquares, sums, and classCount by indexing using the kmeans
  // output labelmap
  while ( !itrInputImage.IsAtEnd() )
    {
    sumsOfSquares[(unsigned int)itrKMeansImage.Get()] =
      sumsOfSquares[(unsigned int)itrKMeansImage.Get()]
      + itrInputImage.Get() * itrInputImage.Get();
    sums[(unsigned int)itrKMeansImage.Get()] =
      sums[(unsigned int)itrKMeansImage.Get()] + itrInputImage.Get();
    ++classCount[(unsigned int)itrKMeansImage.Get()];
    ++itrInputImage;
    ++itrKMeansImage;
    }

  // calculate the class covariances using the sumsOfSquares, sums, and
  // classCount information
  itkDebugMacro(<< "Estimated parameters after Kmeans filter");
  for ( unsigned int i = 0; i < m_NumberOfClasses; ++i )
    {
    estimatedCovariances[i] =
      ( sumsOfSquares[i] / classCount[i] )
      - ( ( sums[i] * sums[i] ) / ( classCount[i] * classCount[i] ) );
    if ( estimatedCovariances[i] < 0.0000001 )  // set lower limit for
                                                // covariance
      {
      estimatedCovariances[i] = 0.0000001;
      }
    itkDebugMacro(<< "cluster[" << i << "]-- ");
    itkDebugMacro(<< " estimated mean : " << estimatedMeans[i]);
    itkDebugMacro(<< " estimated covariance : " << estimatedCovariances[i]);
    }

  // Create gaussian membership functions.
  typename MeanEstimatorsContainerType::Pointer meanEstimatorsContainer =
    MeanEstimatorsContainerType::New();
  typename CovarianceEstimatorsContainerType::Pointer covarianceEstimatorsContainer =
    CovarianceEstimatorsContainerType::New();
  meanEstimatorsContainer->Reserve(m_NumberOfClasses);
  covarianceEstimatorsContainer->Reserve(m_NumberOfClasses);

  m_MembershipFunctionContainer = MembershipFunctionContainerType::New();
  m_MembershipFunctionContainer->Initialize(); // Clear elements
  for ( unsigned int i = 0; i < m_NumberOfClasses; ++i )
    {
    meanEstimatorsContainer->InsertElement( i,
                                            new typename GaussianMembershipFunctionType::MeanVectorType(1) );
    covarianceEstimatorsContainer->
    InsertElement( i, new typename GaussianMembershipFunctionType::CovarianceMatrixType() );
    typename GaussianMembershipFunctionType::MeanVectorType *       meanEstimators =
      const_cast< typename GaussianMembershipFunctionType::MeanVectorType * >
      ( meanEstimatorsContainer->GetElement(i) );
    typename GaussianMembershipFunctionType::CovarianceMatrixType * covarianceEstimators =
      const_cast< typename GaussianMembershipFunctionType::CovarianceMatrixType * >
      ( covarianceEstimatorsContainer->GetElement(i) );
    covarianceEstimators->SetSize(1, 1);

    meanEstimators->Fill(estimatedMeans[i]);
    covarianceEstimators->Fill(estimatedCovariances[i]);
    typename GaussianMembershipFunctionType::Pointer gaussianDensityFunction =
      GaussianMembershipFunctionType::New();
    gaussianDensityFunction->SetMean( *( meanEstimatorsContainer->GetElement(i) ) );
    gaussianDensityFunction->SetCovariance( *( covarianceEstimatorsContainer->GetElement(i) ) );

    m_MembershipFunctionContainer->InsertElement( i,
                                                  dynamic_cast< MembershipFunctionType * >( gaussianDensityFunction.
                                                                                            GetPointer() ) );
    }

  // delete allocated elements in meanEstimators
  while ( !meanEstimatorsContainer->empty() )
    {
    delete meanEstimatorsContainer->back();
    meanEstimatorsContainer->pop_back();
    }
  // delete allocated elements in covarianceEstimators
  while ( !covarianceEstimatorsContainer->empty() )
    {
    delete covarianceEstimatorsContainer->back();
    covarianceEstimatorsContainer->pop_back();
    }
}

template< typename TInputImage, typename TProbabilityPrecisionType >
void
BayesianClassifierInitializationImageFilter< TInputImage, TProbabilityPrecisionType >
::GenerateData()
{
  // TODO Check if we need a progress accumulator
  const InputImageType *inputImage = this->GetInput();

  typename InputImageType::RegionType imageRegion  = inputImage->GetLargestPossibleRegion();
  InputImageIteratorType itrInputImage(inputImage, imageRegion);

  if ( !m_UserSuppliesMembershipFunctions )
    {
    // Perform Kmeans classification to initialize the gaussian density function
    // find class means via kmeans classification
    this->InitializeMembershipFunctions();
    }

  if ( m_MembershipFunctionContainer->Size() != m_NumberOfClasses )
    {
    itkExceptionMacro(
      << "Number of membership functions should be the same as the number of classes");
    }

  this->AllocateOutputs();

  // create vector image of membership probabilities
  OutputImageType *membershipImage = this->GetOutput();

  MembershipImageIteratorType itrMembershipImage(membershipImage, imageRegion);
  MembershipPixelType         membershipPixel(m_NumberOfClasses);
  MeasurementVectorType       mv;

  itrMembershipImage.GoToBegin();
  itrInputImage.GoToBegin();
  while ( !itrMembershipImage.IsAtEnd() )
    {
    mv[0] = itrInputImage.Get();
    for ( unsigned int i = 0; i < m_NumberOfClasses; i++ )
      {
      membershipPixel[i] = ( m_MembershipFunctionContainer->GetElement(i) )->Evaluate(mv);
      }
    itrMembershipImage.Set(membershipPixel);
    ++itrInputImage;
    ++itrMembershipImage;
    }
}

template< typename TInputImage, typename TProbabilityPrecisionType >
void
BayesianClassifierInitializationImageFilter< TInputImage, TProbabilityPrecisionType >
::SetMembershipFunctions(MembershipFunctionContainerType *membershipFunction)
{
  if ( m_NumberOfClasses )
    {
    if ( membershipFunction->Size() != m_NumberOfClasses )
      {
      itkExceptionMacro(
        << "Number of membership functions should be the same as the number of classes");
      }
    }
  else
    {
    m_NumberOfClasses = membershipFunction->Size();
    }

  this->m_MembershipFunctionContainer = membershipFunction;
  m_UserSuppliesMembershipFunctions = true;
  this->Modified();
}

template< typename TInputImage, typename TProbabilityPrecisionType >
void
BayesianClassifierInitializationImageFilter< TInputImage, TProbabilityPrecisionType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "NumberOfClasses: " << m_NumberOfClasses << std::endl;
  if ( m_MembershipFunctionContainer )
    {
    os << indent << "Membership function container:"
       << m_MembershipFunctionContainer << std::endl;
    }
  if ( m_UserSuppliesMembershipFunctions )
    {
    os << indent << "Membership functions provided" << std::endl;
    }
  else
    {
    os << indent << "Membership functions not provided" << std::endl;
    }
}
} // end namespace itk

#endif
