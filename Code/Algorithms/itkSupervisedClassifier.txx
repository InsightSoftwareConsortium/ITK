/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSupervisedClassifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSupervisedClassifier_txx
#define _itkSupervisedClassifier_txx

namespace itk
{

template<class TInputImage, class TClassifiedImage>
SupervisedClassifier<TInputImage,TClassifiedImage>
::SupervisedClassifier( void ):
  m_TrainingImage(NULL)
{
}

template<class TInputImage, class TClassifiedImage>
SupervisedClassifier<TInputImage, TClassifiedImage>
::~SupervisedClassifier(void)
{

}

/**
 * PrintSelf
 */
template <class TInputImage, class TClassifiedImage>
void
SupervisedClassifier<TInputImage, TClassifiedImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Supervised Classifier / Clusterer" << std::endl;

}// end PrintSelf

template<class TInputImage, class TClassifiedImage>
void
SupervisedClassifier<TInputImage, TClassifiedImage>
::SetTrainingImage(TrainingImageType image)
{
  m_TrainingImage = image;
  
  //Ensure that the training image has the same size as the
  //input image

  InputImageConstPointer inputImage = this->GetInputImage();
  
  InputImageSizeType 
    inputImageSize = inputImage->GetBufferedRegion().GetSize();

  typedef InputImageSizeType TrainingImageSizeType;

  TrainingImageSizeType 
    trainingImageSize = m_TrainingImage->GetBufferedRegion().GetSize();

  // Check if the training and input image dimensions are same
  if( (int)(TInputImage::ImageDimension) != (int)(TClassifiedImage::ImageDimension) )
  {
    throw ExceptionObject(__FILE__, __LINE__);
  }

  // Check if size of the two inputs are same
  for( int i = 0; i < TInputImage::ImageDimension; i++)
  {
    if( inputImageSize[i] != trainingImageSize[i] ) throw ExceptionObject(__FILE__, __LINE__);   
  } 
   
  this->Allocate();

  //To trigger the pipeline process
  this->Modified();

}

template<class TInputImage, class TClassifiedImage>
void
SupervisedClassifier<TInputImage, TClassifiedImage>
::Allocate()
{
  InputImageConstPointer inputImage = this->GetInputImage();
  
  InputImageSizeType inputImageSize = inputImage->GetBufferedRegion().GetSize();
  
  ClassifiedImageType classifiedImage =  TClassifiedImage::New();   
  this->SetClassifiedImage(classifiedImage);

  typedef typename TClassifiedImage::IndexType myIndex;
  typename TClassifiedImage::IndexType classifiedImageIndex 
    = myIndex::ZeroIndex;

  typename TClassifiedImage::RegionType classifiedImageRegion;

  classifiedImageRegion.SetSize( inputImageSize );
  classifiedImageRegion.SetIndex( classifiedImageIndex );


  classifiedImage->SetLargestPossibleRegion( classifiedImageRegion );
  classifiedImage->SetBufferedRegion( classifiedImageRegion );
  classifiedImage->Allocate();

}


} // namespace itk

#endif
