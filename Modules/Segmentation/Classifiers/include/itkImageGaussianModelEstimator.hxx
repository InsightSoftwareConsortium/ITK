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
#ifndef itkImageGaussianModelEstimator_hxx
#define itkImageGaussianModelEstimator_hxx

#include "itkImageGaussianModelEstimator.h"
#include "itkMath.h"
#include "itkNumericTraits.h"

namespace itk
{
template< typename TInputImage,
          typename TMembershipFunction,
          typename TTrainingImage >
ImageGaussianModelEstimator< TInputImage, TMembershipFunction, TTrainingImage >
::ImageGaussianModelEstimator(void):
  m_Covariance(ITK_NULLPTR)
{}

template< typename TInputImage,
          typename TMembershipFunction,
          typename TTrainingImage >
ImageGaussianModelEstimator< TInputImage, TMembershipFunction, TTrainingImage >
::~ImageGaussianModelEstimator(void)
{
  delete[] m_Covariance;
}

/**
 * PrintSelf
 */
template< typename TInputImage,
          typename TMembershipFunction,
          typename TTrainingImage >
void
ImageGaussianModelEstimator< TInputImage, TMembershipFunction, TTrainingImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "                   " << std::endl;
  os << indent << "Gaussian Models generated from the training data." << std::endl;
  os << indent << "TrainingImage: ";
  os << m_TrainingImage.GetPointer() << std::endl;
  os << indent << "Results printed in the superclass " << std::endl;
  os << indent << "                   " << std::endl;

  Superclass::PrintSelf(os, indent);
} // end PrintSelf

/**
 * Generate data (start the model building process)
 */
template< typename TInputImage,
          typename TMembershipFunction,
          typename TTrainingImage >
void
ImageGaussianModelEstimator< TInputImage, TMembershipFunction, TTrainingImage >
::GenerateData()
{
  this->EstimateModels();
} // end Generate data

// Takes a set of training images and returns the means
// and variance of the various classes defined in the
// training set.

template< typename TInputImage,
          typename TMembershipFunction,
          typename TTrainingImage >
void
ImageGaussianModelEstimator< TInputImage, TMembershipFunction, TTrainingImage >
::EstimateModels()
{
  //Do some error checking
  InputImageConstPointer inputImage = this->GetInputImage();

  // Check if the training and input image dimensions are the same
  if ( (int)(TInputImage::ImageDimension) != (int)(TTrainingImage::ImageDimension) )
    {
    throw ExceptionObject(__FILE__, __LINE__, "Training and input image dimensions are not the same.", ITK_LOCATION);
    }

  InputImageSizeType inputImageSize = inputImage->GetBufferedRegion().GetSize();

  TrainingImageConstPointer trainingImage = this->GetTrainingImage();

  typedef InputImageSizeType TrainingImageSizeType;
  TrainingImageSizeType trainingImageSize = trainingImage->GetBufferedRegion().GetSize();

  // Check if size of the two inputs are the same
  for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
    {
    if ( inputImageSize[i] != trainingImageSize[i] ) { throw ExceptionObject(
                                                               __FILE__,
                                                               __LINE__,
                                                               "Input image size is not the same as the training image size.",
                                                               ITK_LOCATION); }
    }

  //-------------------------------------------------------------------
  // Set up the gaussian membership calculators
  //-------------------------------------------------------------------

  unsigned int numberOfModels = this->GetNumberOfModels();

  //-------------------------------------------------------------------
  // Call local function to estimate mean variances of the various
  // class labels in the training set.
  // The statistics class functions have not been used since all the
  // class statistics are calculated simultaneously here.
  //-------------------------------------------------------------------

  this->EstimateGaussianModelParameters();

  //-------------------------------------------------------------------
  // Populate the membership functions for all the classes
  //-------------------------------------------------------------------
  MembershipFunctionPointer membershipFunction;
  typename MembershipFunctionType::MeanVectorType tmean;
  typename MembershipFunctionType::CovarianceMatrixType tcov;

  NumericTraits<typename MembershipFunctionType::MeanVectorType>::SetLength(tmean, VectorDimension);
  for ( unsigned int classIndex = 0; classIndex < numberOfModels; classIndex++ )
    {
    membershipFunction = TMembershipFunction::New();

    // Convert to the datatype used for the mean
    for (unsigned int i=0; i < VectorDimension; ++i)
      {
      tmean[i] = m_Means.get(classIndex, i);
      }
    membershipFunction->SetMean( tmean );

    tcov = m_Covariance[classIndex]; // convert cov for membership fn
    membershipFunction->SetCovariance(tcov);

    this->AddMembershipFunction(membershipFunction);
    }
} // end train classifier

template< typename TInputImage,
          typename TMembershipFunction,
          typename TTrainingImage >
void
ImageGaussianModelEstimator< TInputImage, TMembershipFunction, TTrainingImage >
::EstimateGaussianModelParameters()
{
  // Set the iterators and the pixel type definition for the input image
  InputImageConstPointer  inputImage = this->GetInputImage();
  InputImageConstIterator inIt( inputImage, inputImage->GetBufferedRegion() );

  //-------------------------------------------------------------------

  //-------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the training image
  TrainingImageConstPointer trainingImage = this->GetTrainingImage();

  TrainingImageConstIterator trainingImageIt( trainingImage, trainingImage->GetBufferedRegion() );

  //-------------------------------------------------------------------

  unsigned int numberOfModels = ( this->GetNumberOfModels() );

  //-------------------------------------------------------------------
  // Set up the matrices to hold the means and the covariance for the
  // training data

  m_Means.set_size(numberOfModels, VectorDimension);
  m_Means.fill(0);

  m_NumberOfSamples.set_size(numberOfModels, 1);
  m_NumberOfSamples.fill(0);

  // delete previous allocation first
  delete[] m_Covariance;
  //Number of covariance matrices are equal to the number of classes
  m_Covariance = (MatrixType *)new MatrixType[numberOfModels];

  for ( unsigned int i = 0; i < numberOfModels; i++ )
    {
    m_Covariance[i].set_size(VectorDimension, VectorDimension);
    m_Covariance[i].fill(0);
    }

  for ( inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt, ++trainingImageIt )
    {
    unsigned int classIndex = (unsigned int)trainingImageIt.Get();

    // Training data assumed =1 band; also the class indices go
    // from 1, 2, ..., n while the corresponding memory goes from
    // 0, 1, ..., n-1.

    //Ensure that the training data is labelled appropriately
    if ( classIndex > numberOfModels )
      {
      throw ExceptionObject(__FILE__, __LINE__);
      }

    if ( classIndex > 0 )
      {
      m_NumberOfSamples[classIndex][0] += 1;
      InputImagePixelType inImgVec = inIt.Get();

      for ( unsigned int band_x = 0; band_x < VectorDimension; band_x++ )
        {
        m_Means[classIndex][band_x] += inImgVec[band_x];
        for ( unsigned int band_y = 0; band_y <= band_x; band_y++ )
          {
          m_Covariance[classIndex][band_x][band_y] += inImgVec[band_x] * inImgVec[band_y];
          }
        }
      }
    } // end for

  //Loop through the classes to calculate the means and covariance
  for ( unsigned int classIndex = 0; classIndex < numberOfModels; classIndex++ )
    {
    if ( Math::NotAlmostEquals( m_NumberOfSamples[classIndex][0], 0.0 ) )
      {
      for ( unsigned int i = 0; i < VectorDimension; i++ )
        {
        m_Means[classIndex][i] /= m_NumberOfSamples[classIndex][0];
        }
      } // end if

    else
      {
      for ( unsigned int i = 0; i < VectorDimension; i++ )
        {
        m_Means[classIndex][i] = 0;
        }
      } // end else

    if ( Math::NotAlmostEquals( ( m_NumberOfSamples[classIndex][0] - 1 ), 0.0 ) )
      {
      for ( unsigned int band_x = 0; band_x < VectorDimension; band_x++ )
        {
        for ( unsigned int band_y = 0; band_y <= band_x; band_y++ )
          {
          m_Covariance[classIndex][band_x][band_y] /=
            ( m_NumberOfSamples[classIndex][0] - 1 );
          } // end for band_y loop
        }   // end for band_x loop
      }     // end if

    else
      {
      for ( unsigned int band_x = 0; band_x < VectorDimension; band_x++ )
        {
        for ( unsigned int band_y = 0; band_y <= band_x; band_y++ )
          {
          m_Covariance[classIndex][band_x][band_y] = 0;
          }
        }
      } // end else

    MatrixType tempMeanSq;
    tempMeanSq.set_size(VectorDimension, VectorDimension);
    tempMeanSq.fill(0);

    for ( unsigned int band_x = 0; band_x < VectorDimension; band_x++ )
      {
      for ( unsigned int band_y = 0; band_y <= band_x; band_y++ )
        {
        tempMeanSq[band_x][band_y] =
          m_Means[classIndex][band_x] * m_Means[classIndex][band_y];
        }
      } // end for band_x loop

    if ( Math::NotAlmostEquals( ( m_NumberOfSamples[classIndex][0] - 1 ), 0.0 ) )
      {
      tempMeanSq *= ( m_NumberOfSamples[classIndex][0]
                      / ( m_NumberOfSamples[classIndex][0] - 1 ) );
      }
    m_Covariance[classIndex] -= tempMeanSq;

    // Fill the rest of the covairance matrix and make it symmetric
    if ( m_NumberOfSamples[classIndex][0] > 0 )
      {
      unsigned int lastInX = (unsigned int)( VectorDimension - 1 );
      unsigned int upperY = (unsigned int)VectorDimension;
      for ( unsigned int band_x = 0; band_x < lastInX; band_x++ )
        {
        for ( unsigned int band_y = band_x + 1; band_y < upperY; band_y++ )
          {
          m_Covariance[classIndex][band_x][band_y] =
            m_Covariance[classIndex][band_y][band_x];
          } // end band_y loop
        }   // end band_x loop
      }     // end if loop
    }       // end class index loop
}           // end EstimateGaussianModelParameters
}           // namespace itk

#endif
