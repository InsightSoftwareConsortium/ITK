/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianSupervisedClassifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkGaussianSupervisedClassifier_txx
#define _itkGaussianSupervisedClassifier_txx

namespace itk
{


template<class TInputImage, class TClassifiedImage>
GaussianSupervisedClassifier<TInputImage,TClassifiedImage>
::GaussianSupervisedClassifier(void)
{
  m_ClassifiedPixelIndex = -1;
  m_VecDim = 1;
  m_Epsilon   = 1e-100;
  m_DoubleMax = 1e+20;
  m_validTrainingFlag = false;
}

template<class TInputImage, class TClassifiedImage>
GaussianSupervisedClassifier<TInputImage, TClassifiedImage>
::~GaussianSupervisedClassifier(void)
{

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

  os << indent << "Gaussian Supervised Classifier / Clusterer" << std::endl;

}// end PrintSelf


template<class TInputImage, class TClassifiedImage>
void 
GaussianSupervisedClassifier<TInputImage, TClassifiedImage>
::PrintResults()
{

  std::cout<<"                                    "<<std::endl;
  std::cout<<"Results of the training algorithms"<<std::endl;
  std::cout<<"===================================="<<std::endl;

  std::cout<<"                                    "<<std::endl;
  std::cout<<"Number of Samples"<<std::endl;
  std::cout<<"++++++++++++++++++++++++++++++++++++"<<std::endl;

  std::cout<< m_NumSamples <<std::endl;

  std::cout<<"                                    "<<std::endl;
  std::cout<<"Means"<<std::endl;
  std::cout<<"++++++++++++++++++++++++++++++++++++"<<std::endl;

  std::cout<< m_Means <<std::endl;
  std::cout<<"                                    "<<std::endl;
  std::cout<<"Covariance matrix"<<std::endl;
  std::cout<<"++++++++++++++++++++++++++++++++++++"<<std::endl;

  for(unsigned int i = 0; i < m_NumClasses; i++ )
  {
    std::cout<<"============     "<< i<<"     ============="<<std::endl;
    std::cout<<m_Covariance[i]<<std::endl;
    std::cout<<"===================================="<<std::endl;
  }
  
  std::cout<<"============  DONE  ================"<<std::endl;

}// End PrintResults

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

  m_NumClasses = (this->GetNumClasses());

  //-------------------------------------------------------------------
  // Set up the matrices to hold the means and the covariance for the
  // training data

  m_VecDim     = InputPixelType::GetVectorDimension();
  m_Means.resize(m_NumClasses, m_VecDim);
  m_Means.fill(NULL);

  m_NumSamples.resize(m_NumClasses,1);
  m_NumSamples.fill(NULL);

  //Number of covariance matrices are equal to number of classes
  m_Covariance = (MatrixType *) new MatrixType[m_NumClasses];  

  for(unsigned int i = 0; i < m_NumClasses; i++ )
  {
    m_Covariance[i].resize( m_VecDim, m_VecDim );
    m_Covariance[i].fill( NULL );
  }

  for ( inIt.GoToBegin(); ! inIt.IsAtEnd(); ++inIt, ++trainingImageIt ) 
  {
    unsigned int classIndex = trainingImageIt.Get();
        
    // Training data assumed =1 band; also the class indices go
    // from 1, 2, ..., n while the corresponding memory goes from
    // 0, 1, ..., n-1. 

    //Ensure that the training data is labelled appropriately 
    if( classIndex > m_NumClasses )
    {
      throw ExceptionObject();
    }

    if(classIndex > 0)
    {
      m_NumSamples[classIndex][0] +=1;
      InputImageVectorType inImgVec = inIt.Get();

      for(unsigned int band_x = 0; band_x < m_VecDim; band_x++)
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

  for( unsigned int classIndex = 0; classIndex < m_NumClasses; classIndex++ )
  {
    if( m_NumSamples[classIndex][0] != 0 )
    {
      for(unsigned int i=0; i<m_VecDim;i++)
      m_Means[classIndex][i] /= m_NumSamples[classIndex][0];
    }// end if
       
    else 
    {
      for(unsigned int i=0; i<m_VecDim;i++) 
        m_Means[classIndex][i] = 0;
    }// end else
    
    if( ( m_NumSamples[classIndex][0] - 1 ) != 0 )
    {
      for( unsigned int band_x = 0; band_x < m_VecDim; band_x++ )
      {
        for( unsigned int band_y=0; band_y <= band_x; band_y++ )
        {
          m_Covariance[classIndex][band_x][band_y] 
            /= (m_NumSamples[classIndex][0]-1);
        }// end for band_y loop 
      }// end for band_x loop
     }// end if
        
     else
     {
       for( unsigned int band_x = 0; band_x < m_VecDim; band_x++ )
         for( unsigned int band_y = 0; band_y <= band_x; band_y++ )
           m_Covariance[classIndex][band_x][band_y] = 0;
     }// end else

    MatrixType tempMeanSq;
    tempMeanSq.resize( m_VecDim, m_VecDim );
    tempMeanSq.fill(NULL);

    for( unsigned int band_x = 0; band_x < m_VecDim; band_x++)
    {
      for(unsigned int band_y=0; band_y<=band_x; band_y++)
      {
        tempMeanSq[band_x][band_y] = 
          m_Means[classIndex][band_x] * m_Means[classIndex][band_y];
      }
    }// end for band_x loop

    if( ( m_NumSamples[classIndex][0] - 1) != 0 )
    {
      tempMeanSq *= ( m_NumSamples[classIndex][0] 
                      / (m_NumSamples[classIndex][0] - 1 ) );
    }
    m_Covariance[classIndex] -=  tempMeanSq;

    // Fill the rest of the covairance matrix and make it symmetric
    if(m_NumSamples[classIndex][0] > 0)
    {
      for(unsigned int band_x = 0; band_x < (m_VecDim-1); band_x++)
      {
        for(unsigned int band_y=band_x+1; band_y< m_VecDim; band_y++)
        {  
          m_Covariance[classIndex][band_x][band_y] 
            = m_Covariance[classIndex][band_y][band_x];
        }// end band_y loop
      }// end band_x loop
    }// end if loop

  }// end class index loop


  //Calculate the inverse of the covariance matrix 
  //Number of inverse covariance matrices are equal to number of classes
  m_InvCovariance = (MatrixType *) new MatrixType[m_NumClasses];
  
  MatrixType tmpCovMat, inverseMat;

  for(unsigned int classIndex = 0; classIndex < m_NumClasses; classIndex++ ) 
  {
    tmpCovMat = m_Covariance[classIndex];

    // pack the cov matrix from in_model to tmp_cov_mat 
    double cov_sum = 0;
    for(unsigned int band_x = 0; band_x < m_VecDim; band_x++) 
      for(unsigned int band_y = 0; band_y < m_VecDim; band_y++)
            cov_sum += vnl_math_abs(tmpCovMat[band_x][band_y]);
        
    // check if it is a zero covariance, if it is, we make its
    // inverse as an identity matrix with diagonal elements as
    // a very large number; otherwise, inverse it 
    if( cov_sum < m_Epsilon ) 
    {
          inverseMat.resize(m_VecDim,m_VecDim);
          inverseMat.set_identity();
          inverseMat *= m_DoubleMax;
    }
    else 
    {
      // check if num_bands == 1, if it is, we just use 1 to divide it
      if( m_VecDim < 2 ) 
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
  m_validTrainingFlag = true;  


}// end TrainClassifier


template<class TInputImage, class TClassifiedImage>
void
GaussianSupervisedClassifier<TInputImage, TClassifiedImage>
::ClassifyImage()
{
  
  //First ensure that the classifier has been trained before proceedin
  if( m_validTrainingFlag != true) TrainClassifier();

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

  m_NumClasses = this->GetNumClasses();
  for ( inIt.GoToBegin(); ! inIt.IsAtEnd(); ++inIt, ++classifiedIt ) 
  {
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

  double *pixProbability = GetPixelDistance( inPixelVec );
  double minDist = pixProbability[0];
  m_ClassifiedPixelIndex = 1;

  //Loop through the probabilities to get the best index
  for(unsigned int classIndex = 1; classIndex < m_NumClasses; classIndex++ )
  {  
    if( pixProbability[classIndex] < minDist ) 
    {
      minDist = pixProbability[classIndex];
      m_ClassifiedPixelIndex = classIndex;
    }
  }// end for

  return m_ClassifiedPixelIndex;
}// end GetPixelClass

template<class TInputImage, class TClassifiedImage>
double *
GaussianSupervisedClassifier<TInputImage, TClassifiedImage>
::GetPixelDistance( InputImageVectorType &inPixelVec )
{

  double *pixDistance = new double[m_NumClasses];

  for( unsigned int classIndex = 0; classIndex < m_NumClasses; classIndex++ ) 
  {
    // Compute |y - mean | 
    MatrixType tmpVec;
    tmpVec.resize( 1, m_VecDim );
    for ( unsigned int i = 0; i < m_VecDim; i++ )
      tmpVec[0][i] = inPixelVec[i] - m_Means[classIndex][i];
        
    // Compute |y - mean | * inverse(cov) 
    MatrixType tmpMat( 1, m_VecDim );
    tmpMat= tmpVec * m_InvCovariance[classIndex];

    // Compute |y - mean | * inverse(cov) * |y - mean|^T 
    MatrixType tmpVecTranspose( m_VecDim, 1 );
    tmpVecTranspose = tmpVec.transpose();
    tmpMat = tmpMat * tmpVecTranspose;
    
    pixDistance[classIndex] = tmpMat[0][0];
  }

  return pixDistance;
}// end Pixel Distance

} // namespace itk








#endif
