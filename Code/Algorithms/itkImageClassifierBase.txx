/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageClassifierBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageClassifierBase_txx
#define _itkImageClassifierBase_txx
#include "itkImageClassifierBase.h"

namespace itk
{

template<class TInputImage, 
         class TClassifiedImage>
ImageClassifierBase<TInputImage,TClassifiedImage>
::ImageClassifierBase( void )
{
}

template<class TInputImage, 
         class TClassifiedImage>
ImageClassifierBase<TInputImage, TClassifiedImage>
::~ImageClassifierBase(void)
{

}

/**
 * PrintSelf
 */
template <class TInputImage, 
          class TClassifiedImage>
void
ImageClassifierBase<TInputImage, TClassifiedImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "General Image Classifier / Clusterer" << std::endl;

}// end PrintSelf

/**
 * Generate data (start the classification process)
 */
template <class TInputImage, 
          class TClassifiedImage>
void
ImageClassifierBase<TInputImage, TClassifiedImage>
::GenerateData( )
{
  this->Classify();

}// end Generate data

//------------------------------------------------------------------
// The core function where classification is carried out
//------------------------------------------------------------------
template<class TInputImage, 
         class TClassifiedImage>
void
ImageClassifierBase<TInputImage, TClassifiedImage>
::Classify()
{

  ClassifiedImagePointer  classifiedImage = this->GetClassifiedImage();
  //Check if the an output buffer has been allocated
  if( !classifiedImage )
    {
    this->Allocate();

    //To trigger the pipeline process
    this->Modified();
    }

  //--------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the input image
  //-------------------------------------------------------------------
  InputImagePointer  inputImage = this->GetInputImage();
  InputImageIterator inIt( inputImage, inputImage->GetBufferedRegion() );

  //--------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the classified image
  //--------------------------------------------------------------------
  classifiedImage = this->GetClassifiedImage();

  ClassifiedImageIterator 
    classifiedIt( classifiedImage, classifiedImage->GetBufferedRegion() );

  //--------------------------------------------------------------------
  //Set up the vector to store the image  data

  InputImagePixelType      inputImagePixel;
  ClassifiedImagePixelType  outputClassifiedLabel;

  //Set up the storage containers to record the probability
  //measures for each class.
  unsigned int numberOfClasses = this->GetNumberOfMembershipFunctions();

  std::vector< double > discriminantScores;
  discriminantScores.resize( numberOfClasses );
  unsigned int classLabel;
  unsigned int classIndex;

  // support progress methods/callbacks
  unsigned long totalPixels = 
    inputImage->GetBufferedRegion().GetNumberOfPixels();
  unsigned long updateVisits = totalPixels / 10;
  if( updateVisits < 1 ) 
    {
    updateVisits = 1;
    }
  int k = 0;

  double s1;

  MembershipFunctionPointerVector 
    membershipFunctions = this->GetMembershipFunctions();


  for ( inIt.GoToBegin(); ! inIt.IsAtEnd(); ++inIt, ++classifiedIt, ++k ) 
    {

    if ( !( k % updateVisits ) )
      {
      this->UpdateProgress((float)k / (float)totalPixels);
      }

    //Read the input vector
    inputImagePixel = inIt.Get();
    for (classIndex = 0 ; classIndex < numberOfClasses ; classIndex++)
      {
        discriminantScores[classIndex] = 
          (membershipFunctions[classIndex])->Evaluate(inputImagePixel) ;
        s1 = discriminantScores[classIndex];
      }

    
    classLabel = this->GetDecisionRule()->Evaluate(discriminantScores) ;
  
    outputClassifiedLabel = ClassifiedImagePixelType ( classLabel );
    classifiedIt.Set( outputClassifiedLabel );
    }// end for (looping throught the dataset)

}// end Classify

/**
 * Allocate 
 */
template<class TInputImage, 
         class TClassifiedImage>
void
ImageClassifierBase<TInputImage, TClassifiedImage>
::Allocate()
{
  InputImagePointer inputImage = this->GetInputImage();
  
  InputImageSizeType inputImageSize = inputImage->GetBufferedRegion().GetSize();
  
  ClassifiedImagePointer classifiedImage =  TClassifiedImage::New();   
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
