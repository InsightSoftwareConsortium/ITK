/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSupervisedClassifier.txx
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
#ifndef _itkSupervisedClassifier_txx
#define _itkSupervisedClassifier_txx

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

  InputImageType inputImage = this->GetInputImage();
  
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

#endif
