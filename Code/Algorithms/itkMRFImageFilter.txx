/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRFImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMRFImageFilter_txx
#define _itkMRFImageFilter_txx

namespace itk
{

template<class TInputImage, class TClassifiedImage>
MRFImageFilter<TInputImage,TClassifiedImage>
::MRFImageFilter(void):
      m_NumberOfClasses(0),
      m_MaximumNumberOfIterations(50),
      m_LabelStatus(0),
      m_ErrorTolerance(0),
      m_ClassProbability(0),
      m_ClassifierPtr(0),
      m_ErrorCounter(0),
      m_Offset(0),
      m_KernelWidth(3), // Default values of the kernel size of the beta matrix
      m_KernelHeight(3), 
      m_KernelDepth(3)
{
  m_KernelSize = m_KernelWidth * m_KernelHeight * m_KernelDepth;
  m_MRFNeighborhoodWeight.resize(0);
  SetMRFNeighborhoodWeight( 0 );
}

template<class TInputImage, class TClassifiedImage>
MRFImageFilter<TInputImage, TClassifiedImage>
::~MRFImageFilter(void)
{

}

template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << indent <<" MRF Image filter object " << std::endl;
  os << indent <<" Number of classes: " << m_NumberOfClasses << std::endl;
  os << indent <<" Maximum number of iterations: " << m_MaximumNumberOfIterations << std::endl;
  os << indent <<" Error tollerance for convergence: " << m_ErrorTolerance << std::endl;

}// end PrintSelf

/**
 * GenerateInputRequestedRegion method.
 */
template <class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::GenerateInputRequestedRegion()
{

  // this filter requires the all of the input images 
  // to be at the size of the output requested region
  InputImagePointer inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();
  inputPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );

  TrainingImagePointer trainPtr = this->GetTrainingImage();
  if ( trainPtr )
   {
   trainPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
   }
  
}


/**
 * EnlargeOutputRequestedRegion method.
 */
template <class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::EnlargeOutputRequestedRegion(
DataObject *output )
{

  // this filter requires the all of the output image to be in
  // the buffer
  TClassifiedImage *imgData;
  imgData = dynamic_cast<TClassifiedImage*>( output );
  imgData->SetRequestedRegionToLargestPossibleRegion();

}

/**
 * GenerateOutputInformation method.
 */
template <class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::GenerateOutputInformation()
{
  typename TInputImage::Pointer input = this->GetInput();
  typename TClassifiedImage::Pointer output = this->GetOutput();
  output->SetLargestPossibleRegion( input->GetLargestPossibleRegion() );

}

template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::GenerateData()
{
  //First run the Gaussian classifier calculator and
  //generate the Gaussian model for the different classes
  //and then generate the initial labelled dataset.

  InputImagePointer inputImage = this->GetInput();
    
  //Give the input image and training image set to the  
  // classifier
  m_ClassifierPtr->SetInputImage( inputImage );
  m_ClassifierPtr->SetTrainingImage( this->GetTrainingImage() );

  //Run the gaussian classifier algorithm
  m_ClassifierPtr->ClassifyImage();

  //Allocate memory for the labelled images
  this->Allocate();

  ApplyMRFImageFilter();
  //Set the output labelled and allocate the memory
  LabelledImagePointer outputPtr = this->GetOutput();

  //Allocate the output buffer memory 
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  //--------------------------------------------------------------------
  //Copy labelling result to the output buffer
  //--------------------------------------------------------------------
  // Set the iterators to the processed image
  //--------------------------------------------------------------------
  LabelledImageIterator  
    labelledImageIt( m_ClassifierPtr->GetClassifiedImage(), 
                     outputPtr->GetRequestedRegion() );

  //--------------------------------------------------------------------
  // Set the iterators to the output image buffer
  //--------------------------------------------------------------------
  LabelledImageIterator  
    outImageIt( outputPtr, outputPtr->GetRequestedRegion() );

  //--------------------------------------------------------------------

  while ( !outImageIt.IsAtEnd() )
    {
    LabelledImagePixelType labelvalue = 
      ( LabelledImagePixelType ) labelledImageIt.Get();

    outImageIt.Set( labelvalue );
    ++labelledImageIt;
    ++outImageIt;
    }// end while
        
}// end GenerateData

template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::SetClassifier( typename ClassifierType::Pointer ptrToClassifier )
{
  if( ( ptrToClassifier == 0 ) || (m_NumberOfClasses <= 0) )
    throw ExceptionObject(__FILE__, __LINE__);

  m_ClassifierPtr = ptrToClassifier;
  m_ClassifierPtr->SetNumberOfClasses( m_NumberOfClasses );
}//end SetPtrToClassifier

template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::SetTrainingImage( TrainingImagePointer image )
{
  this->ProcessObject::SetNthInput(1, image );
 
}//end SetInputImage

template<class TInputImage, class TClassifiedImage>
MRFImageFilter<TInputImage, TClassifiedImage>
::TrainingImagePointer
MRFImageFilter<TInputImage, TClassifiedImage>
::GetTrainingImage()
{
  if ( this->GetNumberOfInputs() < 2 )
  {
    return NULL;
  }
  return static_cast<TClassifiedImage*>(
    this->ProcessObject::GetInput(1).GetPointer() );

}

template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::Allocate()
{
  if( m_NumberOfClasses <= 0 )
    {
    throw ExceptionObject(__FILE__, __LINE__);
    }

  InputImageSizeType inputImageSize = this->GetInput()->GetBufferedRegion().GetSize();

  //Ensure that the data provided is three dimensional or higher data set
  if(TInputImage::ImageDimension <= 2 )
    {
    throw ExceptionObject(__FILE__, __LINE__);
    }
  
  //---------------------------------------------------------------------
  //Get the image width/height and depth
  //---------------------------------------------------------------------       
  m_ImageWidth  = static_cast<int>(inputImageSize[0]);
  m_ImageHeight = static_cast<int>(inputImageSize[1]);
  m_ImageDepth  = static_cast<int>(inputImageSize[2]);
 
  m_LabelStatus.resize( m_ImageWidth*m_ImageHeight*m_ImageDepth ); 
  
  for( int index = 0; 
       index < ( m_ImageWidth * m_ImageHeight * m_ImageDepth ); 
       index++ ) 
    {
    m_LabelStatus[index]=1;
    }

}// Allocate


template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::SetMRFNeighborhoodWeight( double* )
{

  // Set the beta matrix of a 3x3x3 kernel
  // The index starts from 0 going along the three dimensions
  // in the order of [coloumn], [row], [depth].

  //Allocate memory for the weights of the 3D MRF algorithm
  // and corresponding memory offsets.
  m_WidthOffset.resize(  m_KernelSize );
  m_HeightOffset.resize( m_KernelSize );
  m_DepthOffset.resize(  m_KernelSize );

  m_MRFNeighborhoodWeight.resize( m_KernelSize );


  for( int i = 0; i < 9; i++ ) 
    m_MRFNeighborhoodWeight[i] = 1.3;

  for( int i = 9; i < 18; i++ )
    m_MRFNeighborhoodWeight[i] = 1.7;

  for( int i = 18; i < 27; i++ )
    m_MRFNeighborhoodWeight[i] = 1.3;

  // Change the center weights
  m_MRFNeighborhoodWeight[4]  = 1.5;
  m_MRFNeighborhoodWeight[13] = 0.0;
  m_MRFNeighborhoodWeight[22] = 1.5;

  // k prefix stands for the kernel
        
  int kHalfWidth  = m_KernelWidth/2;
  int kHalfHeight = m_KernelHeight/2;
  int kHalfDepth  = m_KernelDepth/2;

  int l=0; // index for the offset 

  // Now calculate the corresponding offsets
  for( int k = 0; k < m_KernelDepth; k++ )
    {
    for( int j = 0; j < m_KernelHeight; j++ )
      {
      for( int i = 0; i < m_KernelWidth; i++, l++ )
        {
        m_WidthOffset[l]  = ( i - kHalfWidth ); 
        m_HeightOffset[l] = ( j - kHalfHeight );
        m_DepthOffset[l]  = ( k - kHalfDepth );
        }// end for (width loop)
      }// end for (height loop)
    }// end for (depth loop)

}// SetMRFNeighborhoodWeight

template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::SetMRFNeighborhoodWeight( vnl_vector<double> betaMatrix)
{
  m_KernelSize = betaMatrix.size();
  //Allocate memory for the weights of the 3D MRF algorithm
  // and corresponding memory offsets.
  
  m_WidthOffset.resize(  m_KernelSize );
  m_HeightOffset.resize( m_KernelSize );
  m_DepthOffset.resize(  m_KernelSize );
  m_MRFNeighborhoodWeight.resize( m_KernelSize );

  for( unsigned int i = 0; i < betaMatrix.size(); i++ ) 
    m_MRFNeighborhoodWeight[i] = betaMatrix[i];
}// end SetMRFNeighborhoodWeight


template<class TInputImage, class TClassifiedImage>
void 
MRFImageFilter<TInputImage, TClassifiedImage>
::ApplyMRFImageFilter()
{

  int imgSize = m_ImageWidth * m_ImageHeight * m_ImageDepth;
  int maxNumPixelError =  
    (int)(m_ErrorTolerance * imgSize);

  unsigned int numIter = 0;
  do
    {
    itkDebugMacro(<< "Iteration No." << numIter);

    m_ErrorCounter = 0;
    MinimizeFunctional();
    numIter += 1;

    for(int index=0; index < imgSize; index++ )
      {
      if(m_LabelStatus[index] == 1) m_ErrorCounter +=1;
      }
    } 
  while(( numIter < m_MaximumNumberOfIterations ) && 
        ( m_ErrorCounter >maxNumPixelError ) )
    ; 

}// ApplyMRFImageFilter

template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::MinimizeFunctional()
{
  //This implementation uses the ICM algorithm
  ApplyICMLabeller();
}

template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::ApplyICMLabeller()
{
  //--------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the input image
  //-------------------------------------------------------------------
  InputImagePointer inputImage = this->GetInput();
  InputImageIterator  inputImageIt(inputImage, 
                                   inputImage->GetBufferedRegion() );

  //--------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the classified image
  //--------------------------------------------------------------------

  LabelledImagePointer labelledImage = m_ClassifierPtr->GetClassifiedImage();
  LabelledImageIterator  
    labelledImageIt( labelledImage, labelledImage->GetBufferedRegion());

  //---------------------------------------------------------------------
  // Loop through the data set and classify the data
  //---------------------------------------------------------------------

  int offset;
  std::vector<double> neighborInfluence;
  neighborInfluence.resize( m_NumberOfClasses + 1 ); 

  //Varible to store the input pixel vector value
  InputImageVectorType inputPixelVec;

  //Variable to store the labelled pixel vector
  LabelledImagePixelType labelledPixel;

  //Variable to store the output pixel vector label after
  //the MRF classification
  LabelledImagePixelType outLabelledPix;

  //Set a variable to store the offset
  LabelledImageOffsetType offset3D;

  //Set a variable to store the index
  LabelledImageIndexType index3D;

  int imageFrame = m_ImageWidth * m_ImageHeight;

  std::vector<double> dist;
  dist.resize( m_NumberOfClasses );   
 
  for( int d = 0; d < m_ImageDepth; d++ )
    {
    for( int j = 0; j < m_ImageHeight; j++ )
      {
      for( int i = 0; i < m_ImageWidth; i++, ++inputImageIt, ++labelledImageIt )
        {
        int labelStatusPtrOffset = i + j * m_ImageHeight + d * imageFrame;

        //Check if the label == 1 indicates the need for pixel reclassification

        //Uncheck the comment after consulting with Vikram, 
        //Current status returns the best possible result with the
        //flag check suspended

        inputPixelVec = inputImageIt.Get();
        m_ClassifierPtr->GetPixelDistance( inputPixelVec, &(*(dist.begin())) );
                      
        for( unsigned int index = 0; index <= m_NumberOfClasses ;index++ ) 
          neighborInfluence[index]=0;

        for(unsigned int k=0;k<m_KernelSize;k++)
          {
          int widthOffset  = i + m_WidthOffset[k];
          int heightOffset = j + m_HeightOffset[k];
          int depthOffset  = d + m_DepthOffset[k];

          if( ( widthOffset >= 0 ) && ( widthOffset < m_ImageWidth ) &&
              ( heightOffset >= 0 ) && ( heightOffset < m_ImageHeight ) &&
              ( depthOffset >= 0) && ( depthOffset <m_ImageDepth ) )
            {

            //Generate the index offsets
            offset3D[0] = m_WidthOffset[k] ;
            offset3D[1] = m_HeightOffset[k] ;
            offset3D[2] = m_DepthOffset[k]  ;
            index3D = labelledImageIt.GetIndex();
            index3D += offset3D;

            labelledPixel = labelledImage->GetPixel( index3D );
                
            //Assuems that the MRF label is an image with 1 value
            //per pixel and is treated as a vector with 1 entry pervector
            int index = (int) labelledPixel;
    
            //Do the prior probability calculations for each class
            //in the 3x3x3 neighborhood
            neighborInfluence[index] += m_MRFNeighborhoodWeight[k];       

            }// end if

          }// end for 3x3x3 neighborhood processing

        for( unsigned int index = 1; index <= m_NumberOfClasses; index++ )
          dist[index - 1] = neighborInfluence[index] - dist[index - 1] ;

        double maxDist = -1e+20;
        int pixLabel = -1;

        for( unsigned int index = 0; index < m_NumberOfClasses; index++ )
          {
          if ( dist[index] > maxDist )
            {
            maxDist = dist[index];
            pixLabel = index;
            }// if
          }// for
                      
        //Read the current pixel label
        labelledPixel = 
          ( LabelledImagePixelType ) labelledImageIt.Get();

        //Check if the label has changed then set the change flag in all the 
        //neighborhood of the current pixel
        if( pixLabel != ( int ) labelledPixel )
          {
          //While the distance array starts with 0, the actual index starts from 1
          outLabelledPix = pixLabel;
          labelledImageIt.Set( outLabelledPix );

          for( unsigned int k = 0; k < m_KernelSize; k++ )
            {
            int widthOffset  = i + m_WidthOffset[k];
            int heightOffset = j + m_HeightOffset[k];
            int depthOffset  = d + m_DepthOffset[k];

            if( ( widthOffset >= 0) && ( widthOffset < m_ImageWidth ) &&
                ( heightOffset >= 0) && ( heightOffset < m_ImageHeight ) &&
                ( depthOffset >= 0) && ( depthOffset < m_ImageDepth ) )
              {

              offset = m_WidthOffset[k] +
                m_HeightOffset[k] * m_ImageWidth +
                m_DepthOffset[k] * imageFrame;
                  
              m_LabelStatus[labelStatusPtrOffset +offset] = 1;
              }// end if

            }// end for 3x3x3 neighborhood processing
          } //if
        else 
          {
          m_LabelStatus[labelStatusPtrOffset]=0;
          }// else         

        }//end imageWidth
      }// end imageHeight
    }// end imageDepth

}//ApplyICMlabeller

} // namespace itk








#endif
