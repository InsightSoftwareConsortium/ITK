/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSupervisedClassifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
namespace itk
{

template<class TInputImage, class TClassifiedImage>
SupervisedClassifier<TInputImage,TClassifiedImage>
::SupervisedClassifier( void ):
  m_TrainingImage(NULL),
  m_NumClasses(0)
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
::PrintSelf( std::ostream& os, Indent indent )
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

  InputImageType inputImage = this->GetInputImage();
  
  InputImageSizeType 
    inputImageSize = inputImage->GetBufferedRegion().GetSize();

  typedef InputImageSizeType TrainingImageSizeType;

  TrainingImageSizeType 
    trainingImageSize = m_TrainingImage->GetBufferedRegion().GetSize();

  // Check if the training and input image dimensions are same
  if( TInputImage::ImageDimension != TClassifiedImage::ImageDimension)
  {
    throw ExceptionObject();
  }

  // Check if size of the two inputs are same
  for( int i = 0; i < TInputImage::ImageDimension; i++)
  {
    if( inputImageSize[i] != trainingImageSize[i] ) throw ExceptionObject();   
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
  InputImageType inputImage = this->GetInputImage();
  
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















