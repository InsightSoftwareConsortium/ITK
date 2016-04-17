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
#ifndef itkImageClassifierBase_hxx
#define itkImageClassifierBase_hxx
#include "itkImageClassifierBase.h"

namespace itk
{
template< typename TInputImage,
          typename TClassifiedImage >
ImageClassifierBase< TInputImage, TClassifiedImage >
::ImageClassifierBase(void)
{}

template< typename TInputImage,
          typename TClassifiedImage >
ImageClassifierBase< TInputImage, TClassifiedImage >
::~ImageClassifierBase(void)
{}

/**
 * PrintSelf
 */
template< typename TInputImage,
          typename TClassifiedImage >
void
ImageClassifierBase< TInputImage, TClassifiedImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "General Image Classifier / Clusterer" << std::endl;
  os << indent << "ClassifiedImage: ";
  os << m_ClassifiedImage.GetPointer() << std::endl;
  os << indent << "InputImage: ";
  os << m_InputImage.GetPointer() << std::endl;
} // end PrintSelf

/**
 * Generate data (start the classification process)
 */
template< typename TInputImage,
          typename TClassifiedImage >
void
ImageClassifierBase< TInputImage, TClassifiedImage >
::GenerateData()
{
  this->Classify();
} // end Generate data

//------------------------------------------------------------------
// The core function where classification is carried out
//------------------------------------------------------------------
template< typename TInputImage,
          typename TClassifiedImage >
void
ImageClassifierBase< TInputImage, TClassifiedImage >
::Classify()
{
  ClassifiedImagePointer classifiedImage = this->GetClassifiedImage();

  //Check if the an output buffer has been allocated
  if ( !classifiedImage )
    {
    this->Allocate();

    //To trigger the pipeline process
    this->Modified();
    }

  //--------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the input image
  //-------------------------------------------------------------------
  InputImageConstPointer  inputImage = this->GetInputImage();
  InputImageConstIterator inIt( inputImage, inputImage->GetBufferedRegion() );

  //--------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the classified image
  //--------------------------------------------------------------------
  classifiedImage = this->GetClassifiedImage();

  ClassifiedImageIterator
  classifiedIt( classifiedImage, classifiedImage->GetBufferedRegion() );

  //--------------------------------------------------------------------
  //Set up the vector to store the image  data

  InputImagePixelType      inputImagePixel;
  ClassifiedImagePixelType outputClassifiedLabel;

  //Set up the storage containers to record the probability
  //measures for each class.
  unsigned int numberOfClasses = this->GetNumberOfMembershipFunctions();

  std::vector< double > discriminantScores;
  discriminantScores.resize(numberOfClasses);
  unsigned int classLabel;
  unsigned int classIndex;

  // support progress methods/callbacks
  SizeValueType totalPixels =
    inputImage->GetBufferedRegion().GetNumberOfPixels();
  SizeValueType updateVisits = totalPixels / 10;
  if ( updateVisits < 1 )
    {
    updateVisits = 1;
    }
  int k = 0;

  for ( inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt, ++classifiedIt, ++k )
    {
    if ( !( k % updateVisits ) )
      {
      this->UpdateProgress( (float)k / (float)totalPixels );
      }

    //Read the input vector
    inputImagePixel = inIt.Get();
    for ( classIndex = 0; classIndex < numberOfClasses; classIndex++ )
      {
      discriminantScores[classIndex] =
        ( this->GetMembershipFunction(classIndex) )->Evaluate(inputImagePixel);
      }

    classLabel = static_cast<unsigned int>( this->GetDecisionRule()->Evaluate(discriminantScores) );

    outputClassifiedLabel = ClassifiedImagePixelType (classLabel);
    classifiedIt.Set(outputClassifiedLabel);
    } // end for (looping through the dataset)
}     // end Classify

/**
 * Allocate
 */
template< typename TInputImage,
          typename TClassifiedImage >
void
ImageClassifierBase< TInputImage, TClassifiedImage >
::Allocate()
{
  InputImageConstPointer inputImage = this->GetInputImage();

  InputImageSizeType inputImageSize = inputImage->GetBufferedRegion().GetSize();

  ClassifiedImagePointer classifiedImage =  TClassifiedImage::New();

  this->SetClassifiedImage(classifiedImage);

  typename TClassifiedImage::IndexType classifiedImageIndex;
  classifiedImageIndex.Fill(0);

  typename TClassifiedImage::RegionType classifiedImageRegion;

  classifiedImageRegion.SetSize(inputImageSize);
  classifiedImageRegion.SetIndex(classifiedImageIndex);

  classifiedImage->SetLargestPossibleRegion(classifiedImageRegion);
  classifiedImage->SetBufferedRegion(classifiedImageRegion);
  classifiedImage->Allocate();
}

template< typename TInputImage,
          typename TClassifiedImage >
std::vector< double >
ImageClassifierBase< TInputImage, TClassifiedImage >
::GetPixelMembershipValue(const InputImagePixelType inputImagePixel)
{
  unsigned int numberOfClasses = this->GetNumberOfClasses();
  std::vector< double >  pixelMembershipValue(numberOfClasses);

  for ( unsigned int classIndex = 0; classIndex < numberOfClasses; classIndex++ )
    {
    pixelMembershipValue[classIndex] =
      ( this->GetMembershipFunction(classIndex) )->Evaluate(inputImagePixel);
    }

  //Return the membership value of the
  return pixelMembershipValue;
}
} // namespace itk

#endif
