/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageGaussianModelEstimator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageGaussianModelEstimator_txx
#define _itkImageGaussianModelEstimator_txx

#include "itkImageGaussianModelEstimator.h"
namespace itk
{

template<class TInputImage, 
         class TMembershipFunction,
         class TTrainingImage>
ImageGaussianModelEstimator<TInputImage, TMembershipFunction, TTrainingImage>
::ImageGaussianModelEstimator(void):
  m_Covariance( NULL )
{

}

template<class TInputImage, 
         class TMembershipFunction,
         class TTrainingImage>
ImageGaussianModelEstimator<TInputImage, TMembershipFunction, TTrainingImage>
::~ImageGaussianModelEstimator(void)
{
  if ( m_Covariance )    delete [] m_Covariance;
}

/*
 * PrintSelf
 */
template<class TInputImage, 
         class TMembershipFunction,
         class TTrainingImage>
void
ImageGaussianModelEstimator<TInputImage, TMembershipFunction, TTrainingImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{  

  os << indent << "                   " << std::endl;
  os << indent << "Gaussian Models generated from the training data." << std::endl;
  os << indent << "TrainingImage: " ;
  os << m_TrainingImage.GetPointer() << std::endl;
  os << indent << "Results printed in the superclass " << std::endl;
  os << indent << "                   " << std::endl;

  Superclass::PrintSelf(os,indent);

}// end PrintSelf


/**
 * Generate data (start the model building process)
 */
template<class TInputImage, 
         class TMembershipFunction,
         class TTrainingImage>
void 
ImageGaussianModelEstimator<TInputImage, TMembershipFunction, TTrainingImage>
::GenerateData( )
{
  this->EstimateModels();

}// end Generate data

// Takes a set of training images and returns the means 
// and variance of the various classes defined in the
// training set.

template<class TInputImage, 
         class TMembershipFunction,
         class TTrainingImage>
void 
ImageGaussianModelEstimator<TInputImage, TMembershipFunction, TTrainingImage>
::EstimateModels()
{

  //Do some error checking
  InputImagePointer  inputImage = this->GetInputImage();

  // Check if the training and input image dimensions are same
  if( (int)(TInputImage::ImageDimension) != (int)(TTrainingImage::ImageDimension) )
  {
    throw ExceptionObject(__FILE__, __LINE__);
  }

  InputImageSizeType 
    inputImageSize = inputImage->GetBufferedRegion().GetSize();

  typedef InputImageSizeType TrainingImageSizeType;

  TrainingImagePointer  trainingImage = this->GetTrainingImage();

  TrainingImageSizeType 
    trainingImageSize = trainingImage->GetBufferedRegion().GetSize();  

  // Check if size of the two inputs are same
  for( unsigned int i = 0; i < TInputImage::ImageDimension; i++)
    {
    if( inputImageSize[i] != trainingImageSize[i] ) throw ExceptionObject(__FILE__, __LINE__); 
    }

  //-------------------------------------------------------------------
  // Set up the gaussian membership calculators
  //-------------------------------------------------------------------

  unsigned int numberOfModels = this->GetNumberOfModels();

  //-------------------------------------------------------------------
  // Call local function to estimate mean variances of the various
  // class labels in the training set
  // The statistics class functions have not been used since all the 
  // class statistics are calculated simultaneously here.
  //-------------------------------------------------------------------

  this->EstimateGaussianModelPrameters();

  //-------------------------------------------------------------------
  // Populate the membership functions for all the classes
  //-------------------------------------------------------------------
  MembershipFunctionPointer membershipFunction;

  for (unsigned int classIndex = 0 ; classIndex < numberOfModels ; classIndex++)
    {
   
    membershipFunction = TMembershipFunction::New() ;


    membershipFunction->
      SetNumberOfSamples( m_NumberOfSamples(classIndex, 0) ) ;     

    membershipFunction->
      SetMean(m_Means.get_row( classIndex) ) ;

    membershipFunction->
      SetCovariance( m_Covariance[classIndex] ) ;

    this->AddMembershipFunction( membershipFunction ); 

    }  

}// end train classifier 

template<class TInputImage, 
         class TMembershipFunction,
         class TTrainingImage>
void 
ImageGaussianModelEstimator<TInputImage, TMembershipFunction, TTrainingImage>
::EstimateGaussianModelPrameters()
{

  // Set the iterators and the pixel type definition for the input image
  InputImagePointer  inputImage = this->GetInputImage();
  InputImageIterator inIt( inputImage, inputImage->GetBufferedRegion() );

  //-------------------------------------------------------------------

  //-------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the training image
  TrainingImagePointer  trainingImage = this->GetTrainingImage();

  TrainingImageIterator 
    trainingImageIt( trainingImage, trainingImage->GetBufferedRegion() );

  //-------------------------------------------------------------------

  unsigned int numberOfModels = (this->GetNumberOfModels());

  //-------------------------------------------------------------------
  // Set up the matrices to hold the means and the covariance for the
  // training data

  m_Means.resize(numberOfModels, VectorDimension);
  m_Means.fill(0);

  m_NumberOfSamples.resize(numberOfModels,1);
  m_NumberOfSamples.fill(0);

  // delete previous allocation first
  if ( m_Covariance ) delete [] m_Covariance;
  //Number of covariance matrices are equal to number of classes
  m_Covariance = (MatrixType *) new MatrixType[numberOfModels];  

  for(unsigned int i = 0; i < numberOfModels; i++ )
    {
    m_Covariance[i].resize( VectorDimension, VectorDimension );
    m_Covariance[i].fill( 0 );
    }

  for( inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt, ++trainingImageIt ) 
    {

    unsigned int classIndex = (unsigned int) trainingImageIt.Get();
        
    // Training data assumed =1 band; also the class indices go
    // from 1, 2, ..., n while the corresponding memory goes from
    // 0, 1, ..., n-1. 

    //Ensure that the training data is labelled appropriately 
    if( classIndex > numberOfModels )
      {
      throw ExceptionObject(__FILE__, __LINE__);
      }

    if(classIndex > 0)
      {
      m_NumberOfSamples[classIndex][0] +=1;
      InputImagePixelType inImgVec = inIt.Get();

      for(unsigned int band_x = 0; band_x < VectorDimension; band_x++)
        {
        m_Means[classIndex][band_x] += inImgVec[band_x];
        for(unsigned int band_y = 0; band_y <= band_x; band_y++ )
          {
          m_Covariance[classIndex][band_x][band_y] += inImgVec[band_x] * inImgVec[band_y];
          }
        }
    }
  }// end for 

  //Loop through the classes to calculate the means and
  for( unsigned int classIndex = 0; classIndex < numberOfModels; classIndex++ )
    {
    if( m_NumberOfSamples[classIndex][0] != 0 )
      {
      for( unsigned int i = 0; i < VectorDimension; i++ )
      m_Means[classIndex][i] /= m_NumberOfSamples[classIndex][0];
      }// end if
       
    else 
      {
      for( unsigned int i = 0; i < VectorDimension ; i++ ) 
        m_Means[classIndex][i] = 0;
      }// end else
    
    if( ( m_NumberOfSamples[classIndex][0] - 1 ) != 0 )
      {
      for( unsigned int band_x = 0; band_x < VectorDimension; band_x++ )
        {
        for( unsigned int band_y=0; band_y <= band_x; band_y++ )
          {
          m_Covariance[classIndex][band_x][band_y] 
            /= (m_NumberOfSamples[classIndex][0]-1);
          }// end for band_y loop 
        }// end for band_x loop
      }// end if
        
     else
       {
       for( unsigned int band_x = 0; band_x < VectorDimension; band_x++ )
         for( unsigned int band_y = 0; band_y <= band_x; band_y++ )
           m_Covariance[classIndex][band_x][band_y] = 0;
       }// end else

    MatrixType tempMeanSq;
    tempMeanSq.resize( VectorDimension, VectorDimension );
    tempMeanSq.fill(0);

    for( unsigned int band_x = 0; band_x < VectorDimension; band_x++)
      {
      for(unsigned int band_y=0; band_y<=band_x; band_y++)
        {
        tempMeanSq[band_x][band_y] = 
          m_Means[classIndex][band_x] * m_Means[classIndex][band_y];
        }
      }// end for band_x loop

    if( ( m_NumberOfSamples[classIndex][0] - 1) != 0 )
      {
      tempMeanSq *= ( m_NumberOfSamples[classIndex][0] 
                      / (m_NumberOfSamples[classIndex][0] - 1 ) );
      }
    m_Covariance[classIndex] -=  tempMeanSq;

    // Fill the rest of the covairance matrix and make it symmetric
    if(m_NumberOfSamples[classIndex][0] > 0)
      {
      unsigned int vdimension = VectorDimension - 1;
      for(unsigned int band_x = 0; band_x < (vdimension - 1); band_x++)
        {
        for(unsigned int band_y=band_x+1; band_y < vdimension; band_y++)
          {  
          m_Covariance[classIndex][band_x][band_y] 
            = m_Covariance[classIndex][band_y][band_x];
          }// end band_y loop
        }// end band_x loop
      }// end if loop
    }// end class index loop

}// end EstimateGaussianModelPrameters

} // namespace itk

#endif
