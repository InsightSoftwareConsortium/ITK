/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianSupervisedClassifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGaussianSupervisedClassifier_txx
#define _itkGaussianSupervisedClassifier_txx
#include "itkGaussianSupervisedClassifier.h"

namespace itk
{


template<class TInputImage, class TClassifiedImage>
GaussianSupervisedClassifier<TInputImage,TClassifiedImage>
::GaussianSupervisedClassifier(void)
{
  m_ClassifiedPixelIndex = -1;
  m_Epsilon   = 1e-100;
  m_DoubleMax = 1e+20;
  m_ValidTrainingFlag = false;
 
  m_Covariance = NULL;
  m_InvCovariance = NULL;
}

template<class TInputImage, class TClassifiedImage>
GaussianSupervisedClassifier<TInputImage, TClassifiedImage>
::~GaussianSupervisedClassifier(void)
{
  if ( m_Covariance )    delete [] m_Covariance;
  if ( m_InvCovariance ) delete [] m_InvCovariance;
  if ( m_PixelProbability )  delete [] m_PixelProbability;
}

/**
 * PrintSelf
 */
template <class TInputImage, class TClassifiedImage>
void
GaussianSupervisedClassifier<TInputImage, TClassifiedImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent <<"                                    "<<std::endl;
  os << indent <<"Results of the training algorithms"<<std::endl;
  os << indent <<"===================================="<<std::endl;

  os << indent <<"Number of Samples" << m_NumberOfSamples <<std::endl;

  os << indent <<"Means" << m_Means <<std::endl;

  os << indent <<"Covariance matrix";
  for(unsigned int i = 0; i < m_NumberOfClasses; i++ )
  {
    os << indent <<m_Covariance[i]<<std::endl;
    os << indent <<"===================================="<<std::endl;
  }
  
}// end PrintSelf

// Set the memory for the probability function
template<class TInputImage, class TClassifiedImage>
void
GaussianSupervisedClassifier<TInputImage, TClassifiedImage>
::SetNumberOfClasses( const unsigned int num)
{

  m_NumberOfClasses = num;
  m_PixelProbability = new double[m_NumberOfClasses];

}

// Takes a set of training images and returns the means 
// and variance of the various classes defined in the
// training set.

template<class TInputImage, class TClassifiedImage>
void 
GaussianSupervisedClassifier<TInputImage, TClassifiedImage>
::TrainClassifier()
{
  //-------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the input image
  InputImageType  inputImage = this->GetInputImage();
  InputImageIterator inIt( inputImage, inputImage->GetBufferedRegion() );

  //-------------------------------------------------------------------

  //-------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the training image
  TrainingImageType  trainingImage = this->GetTrainingImage();

  TrainingImageIterator 
          trainingImageIt( trainingImage, trainingImage->GetBufferedRegion() );


  //-------------------------------------------------------------------

  m_NumberOfClasses = (this->GetNumberOfClasses());

  //-------------------------------------------------------------------
  // Set up the matrices to hold the means and the covariance for the
  // training data

  m_Means.resize(m_NumberOfClasses, VectorDimension);
  m_Means.fill(0);

  m_NumberOfSamples.resize(m_NumberOfClasses,1);
  m_NumberOfSamples.fill(0);

  // delete previous allocation first
  if ( m_Covariance ) delete [] m_Covariance;
  //Number of covariance matrices are equal to number of classes
  m_Covariance = (MatrixType *) new MatrixType[m_NumberOfClasses];  

  for(unsigned int i = 0; i < m_NumberOfClasses; i++ )
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
    if( classIndex > m_NumberOfClasses )
      {
      throw ExceptionObject(__FILE__, __LINE__);
      }

    if(classIndex > 0)
      {
      m_NumberOfSamples[classIndex][0] +=1;
      InputImageVectorType inImgVec = inIt.Get();

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

  for( unsigned int classIndex = 0; classIndex < m_NumberOfClasses; classIndex++ )
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
      for(unsigned int band_x = 0; static_cast<int>(band_x) < (VectorDimension - 1); band_x++)
        {
        for(unsigned int band_y=band_x+1; static_cast<int>(band_y) < VectorDimension; band_y++)
          {  
          m_Covariance[classIndex][band_x][band_y] 
            = m_Covariance[classIndex][band_y][band_x];
          }// end band_y loop
        }// end band_x loop
      }// end if loop
    }// end class index loop

  // delete previous allocation first
  if ( m_InvCovariance ) delete [] m_InvCovariance;

  //Calculate the inverse of the covariance matrix 
  //Number of inverse covariance matrices are equal to number of classes
  m_InvCovariance = (MatrixType *) new MatrixType[m_NumberOfClasses];
  
  MatrixType tmpCovMat, inverseMat;

  for(unsigned int classIndex = 0; classIndex < m_NumberOfClasses; classIndex++ ) 
    {
    tmpCovMat = m_Covariance[classIndex];

    // pack the cov matrix from in_model to tmp_cov_mat 
    double cov_sum = 0;
    for(unsigned int band_x = 0; band_x < VectorDimension; band_x++) 
      for(unsigned int band_y = 0; band_y < VectorDimension; band_y++)
        cov_sum += vnl_math_abs(tmpCovMat[band_x][band_y]);
        
    // check if it is a zero covariance, if it is, we make its
    // inverse as an identity matrix with diagonal elements as
    // a very large number; otherwise, inverse it 
    if( cov_sum < m_Epsilon ) 
      {
      inverseMat.resize( VectorDimension, VectorDimension );
      inverseMat.set_identity();
      inverseMat *= m_DoubleMax;
      }
    else 
      {
      // check if num_bands == 1, if it is, we just use 1 to divide it
      if( VectorDimension < 2 ) 
        {
        inverseMat.resize(1,1);
        inverseMat[0][0] = 1.0 / tmpCovMat[0][0];
        }
      else 
        {
        inverseMat = vnl_matrix_inverse<double>(tmpCovMat);
        }
      }// end inverse calculations

    m_InvCovariance[classIndex] = inverseMat;
    }//end Class index looping
  
  //Training completed now set the valid training flag
  m_ValidTrainingFlag = true;  


}// end TrainClassifier


template<class TInputImage, class TClassifiedImage>
void
GaussianSupervisedClassifier<TInputImage, TClassifiedImage>
::ClassifyImage()
{
  
  //First ensure that the classifier has been trained before proceedin
  if( m_ValidTrainingFlag != true) TrainClassifier();

  //--------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the input image
  //-------------------------------------------------------------------
  InputImageType  inputImage = this->GetInputImage();
  InputImageIterator inIt( inputImage, inputImage->GetBufferedRegion() );

  //--------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the classified image
  //--------------------------------------------------------------------
  ClassifiedImageType  classifiedImage = this->GetClassifiedImage();

  TrainingImageIterator 
    classifiedIt( classifiedImage, classifiedImage->GetBufferedRegion() );

  //--------------------------------------------------------------------

  //Set up the vector to store the image  data

  InputImageVectorType      inImgVec;
  ClassifiedImagePixelType  outClassified;

  // support progress methods/callbacks
  unsigned long totalPixels = 
    inputImage->GetBufferedRegion().GetNumberOfPixels();
  unsigned long updateVisits = totalPixels / 10;
  if( updateVisits < 1 ) 
    {
    updateVisits = 1;
    }
  int k = 0;

  m_NumberOfClasses = this->GetNumberOfClasses();
  for ( inIt.GoToBegin(); ! inIt.IsAtEnd(); ++inIt, ++classifiedIt, ++k ) 
    {

    if ( !( k % updateVisits ) )
      {
      this->UpdateProgress((float)k / (float)totalPixels);
      }

    inImgVec = inIt.Get();
    int classifiedIndex = GetPixelClass( inImgVec );
         
    outClassified = ClassifiedImagePixelType ( classifiedIndex );
    classifiedIt.Set( outClassified );
    }// end for (looping throught the dataset

}// end ClassifyImage

template<class TInputImage, class TClassifiedImage>
int 
GaussianSupervisedClassifier<TInputImage, TClassifiedImage>
::GetPixelClass(InputImageVectorType &inPixelVec)
{

  GetPixelDistance( inPixelVec, m_PixelProbability );
  double minDist = m_PixelProbability[0];
  m_ClassifiedPixelIndex = 1;

  //Loop through the probabilities to get the best index
  for(unsigned int classIndex = 1; classIndex < m_NumberOfClasses; classIndex++ )
    {  
    if( m_PixelProbability[classIndex] < minDist ) 
      {
      minDist = m_PixelProbability[classIndex];
      m_ClassifiedPixelIndex = classIndex;
      }
    }// end for

  return m_ClassifiedPixelIndex;
}// end GetPixelClass

template<class TInputImage, class TClassifiedImage>
void
GaussianSupervisedClassifier<TInputImage, TClassifiedImage>
::GetPixelDistance( 
InputImageVectorType &inPixelVec,
double * pixDistance )
{
  double tmp;
  for( unsigned int classIndex = 0; classIndex < m_NumberOfClasses; classIndex++ ) 
    {
    // Compute |y - mean | 
    for ( unsigned int i = 0; i < VectorDimension; i++ )
      m_TmpVec[0][i] = inPixelVec[i] - m_Means[classIndex][i];
        
    // Compute |y - mean | * inverse(cov) 
    m_TmpMat= m_TmpVec * m_InvCovariance[classIndex];

    // Compute |y - mean | * inverse(cov) * |y - mean|^T 
    //tmp = (m_TmpMat * (m_TmpVec.transpose()))[0][0];
    tmp = dot_product( m_TmpMat, m_TmpVec ); 
    
    pixDistance[classIndex] = tmp;
    }

}// end Pixel Distance

} // namespace itk








#endif
