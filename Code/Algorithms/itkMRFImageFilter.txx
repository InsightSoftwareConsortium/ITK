/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRFImageFilter.txx
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
MRFImageFilter<TInputImage,TClassifiedImage>
::MRFImageFilter(void):
      m_InputImage(0),
      m_TrainingImage(0),
      m_LabelledImage(0),
      m_NumClasses(0),
      m_LabelStatus(0),
      m_ErrorCounter(0),
      m_MaxNumIter(50),
      m_ClassifierPtr(0),
      m_kWidth(3), // Default values of the kernel size of the beta matrix
      m_kHeight(3), 
      m_kDepth(3) 
{
  m_KernelSize = m_kWidth * m_kHeight * m_kDepth;
  SetBeta( 0 );
}

template<class TInputImage, class TClassifiedImage>
MRFImageFilter<TInputImage, TClassifiedImage>
::~MRFImageFilter(void)
{
        
}

/**
 * GenerateInputRequestedRegion method.
 */
template <class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::GenerateInputRequestedRegion()
{

  // this filter requires the all of the input image to be in
  // the buffer
  InputImageType inputPtr = this->GetInput();
  inputPtr->SetRequestedRegionToLargestPossibleRegion();
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

  m_InputImage = this->GetInput();
    
  //Give the input image and training image set to the  
  // classifier
  m_ClassifierPtr->SetInputImage( m_InputImage );
  m_ClassifierPtr->SetTrainingImage( m_TrainingImage );

  //Run the gaussian classifier algorithm
  m_ClassifierPtr->ClassifyImage();

  SetLabelledImage( m_ClassifierPtr->GetClassifiedImage() );

  ApplyMRFImageFilter();
  //Set the output labelled and allocate the memory
  LabelledImageType outputPtr = this->GetOutput();

  //Allocate the output buffer memory 
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  //--------------------------------------------------------------------
  //Copy labelling result to the output buffer
  //--------------------------------------------------------------------
  // Set the iterators to the processed image
  //--------------------------------------------------------------------
  LabelledImageIterator  
    labelledImageIt( m_LabelledImage, m_LabelledImage->GetBufferedRegion() );

  labelledImageIt.Begin();

  //--------------------------------------------------------------------
  // Set the iterators to the output image buffer
  //--------------------------------------------------------------------
  LabelledImageIterator  
    outImageIt( outputPtr, outputPtr->GetBufferedRegion() );

  outImageIt.Begin();

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
  if( ( ptrToClassifier == 0 ) || (m_NumClasses <= 0) )
    throw ExceptionObject();

  m_ClassifierPtr = ptrToClassifier;
  m_ClassifierPtr->SetNumClasses( m_NumClasses );
}//end SetPtrToClassifier

template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::SetTrainingImage( TrainingImageType image )
{
  m_TrainingImage = image;
}//end SetInputImage



template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::SetLabelledImage(LabelledImageType image)
{
  m_LabelledImage = image;
  this->Allocate();
}// Set the LabelledImage

template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::PrintSelf( std::ostream& os, Indent indent )
{
  Superclass::PrintSelf(os,indent);
  os << indent << "MRF labeller" << std::endl;
}// end PrintSelf


template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::Allocate()
{
  if( m_NumClasses <= 0 )
  {
    throw ExceptionObject();
  }

  InputImageSizeType inputImageSize = m_InputImage->GetBufferedRegion().GetSize();

  //Ensure that the data provided is three dimensional data set
  if(TInputImage::ImageDimension <= 2 )
  {
    throw ExceptionObject();
  }
  
  //---------------------------------------------------------------------
  //Get the image width/height and depth
  //---------------------------------------------------------------------       
  m_imgWidth  = inputImageSize[0];
  m_imgHeight = inputImageSize[1];
  m_imgDepth  = inputImageSize[2];
 
  m_LabelStatus = (unsigned int *) new unsigned int[m_imgWidth*m_imgHeight*m_imgDepth]; 
  for( int index = 0; 
       index <= ( m_imgWidth * m_imgHeight * m_imgDepth ); 
       index++ ) 
  {
    m_LabelStatus[index]=1;
  }

}// Allocate


template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::SetBeta( double* )
{

  // Set the beta matrix of a 3x3x3 kernel
  // The index starts from 0 going along the three dimensions
  // in the order of [coloumn], [row], [depth].

  //Allocate memory for the weights of the 3D MRF algorithm
  // and corresponding memory offsets.
  m_WidthOffset     = (int *)   new int   [m_KernelSize];
  m_HeightOffset    = (int *)   new int   [m_KernelSize];
  m_DepthOffset     = (int *)   new int   [m_KernelSize];
  m_Beta3x3x3       = (double *)new double[m_KernelSize];


  for( int i = 0; i < 9; i++ ) 
    m_Beta3x3x3[i] = 1.3;

  for( int i = 9; i < 18; i++ )
    m_Beta3x3x3[i] = 1.7;

  for( int i = 18; i < 27; i++ )
    m_Beta3x3x3[i] = 1.3;

  // Change the center weights
  m_Beta3x3x3[4]  = 1.5;
  m_Beta3x3x3[13] = 0.0;
  m_Beta3x3x3[22] = 1.5;

  // k prefix stands for the kernel
        
  int kHalfWidth  = m_kWidth/2;
  int kHalfHeight = m_kHeight/2;
  int kHalfDepth  = m_kDepth/2;

  int l=0; // index for the offset 

  // Now calculate the corresponding offsets
  for( int k = 0; k < m_kDepth; k++ )
  {
    for( int j = 0; j < m_kHeight; j++ )
    {
      for( int i = 0; i < m_kWidth; i++, l++ )
      {
        m_WidthOffset[l]  = ( i - kHalfWidth ); 
        m_HeightOffset[l] = ( j - kHalfHeight );
        m_DepthOffset[l]  = ( k - kHalfDepth );
      }// end for (width loop)
    }// end for (height loop)
  }// end for (depth loop)

}// SetBeta


template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::SetBeta( double *betaMatrix, unsigned int kernelSize )
{
  m_KernelSize = kernelSize;
  //Allocate memory for the weights of the 3D MRF algorithm
  // and corresponding memory offsets.
  m_WidthOffset     = (int *)   new int   [m_KernelSize];
  m_HeightOffset    = (int *)   new int   [m_KernelSize];
  m_DepthOffset     = (int *)   new int   [m_KernelSize];
  m_Beta3x3x3       = (double *)new double[m_KernelSize];

  for( int i = 0; i < m_KernelSize; i++ ) 
    m_Beta3x3x3[i] = *betaMatrix++;
}// end SetBeta

template<class TInputImage, class TClassifiedImage>
void
MRFImageFilter<TInputImage, TClassifiedImage>
::SetBeta( vnl_vector<double> betaMatrix)
{
  m_KernelSize = betaMatrix.size();
  //Allocate memory for the weights of the 3D MRF algorithm
  // and corresponding memory offsets.
  m_WidthOffset     = (int *)   new int   [m_KernelSize];
  m_HeightOffset    = (int *)   new int   [m_KernelSize];
  m_DepthOffset     = (int *)   new int   [m_KernelSize];
  m_Beta3x3x3       = (double *)new double[m_KernelSize];

  for( int i = 0; i < betaMatrix.size(); i++ ) 
    m_Beta3x3x3[i] = betaMatrix[i];
}// end SetBeta


template<class TInputImage, class TClassifiedImage>
void 
MRFImageFilter<TInputImage, TClassifiedImage>
::ApplyMRFImageFilter()
{

  int maxNumPixelError =  
    m_ErrorTollerance * m_imgWidth * m_imgHeight * m_imgDepth;

  int numIter = 0;
  do
  {
    std::cout << "Iteration No." << numIter << std::endl;

    m_ErrorCounter = 0;
    MinimizeFunctional();
    numIter += 1;

    for(int index=0; 
        index<( m_imgWidth * m_imgHeight * m_imgDepth ); index++ )
    {
      if(m_LabelStatus[index] ==1) m_ErrorCounter +=1;
    }
  } 
  while(( numIter < m_MaxNumIter ) && ( m_ErrorCounter >maxNumPixelError ) ); 

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
  InputImageIterator  inputImageIt(m_InputImage, 
                                   m_InputImage->GetBufferedRegion() );

  inputImageIt.Begin();
 
  //--------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the classified image
  //--------------------------------------------------------------------
  LabelledImageIterator  
    labelledImageIt(m_LabelledImage, m_LabelledImage->GetBufferedRegion());

  labelledImageIt.Begin();
 
  //---------------------------------------------------------------------
  // Loop through the data set and classify the data
  //---------------------------------------------------------------------

  int offset;
  double *neighborInfluence = (double *) new double[m_NumClasses + 1];

  //Varible to store the input pixel vector value
  InputImageVectorType inputPixelVec;

  //Variable to store the labelled pixel vector
  LabelledImagePixelType labelledPixel;

  //Variable to store the output pixel vector label after
  //the MRF classification
  LabelledImagePixelType outLabelledPix;

  //Set a variable to store the offset index
  LabelledImageIndexType offsetIndex3D = { 0, 0, 0};

  int imageFrame = m_imgWidth * m_imgHeight;

  for(int d = 0; d < m_imgDepth; d++ )
  {
    for( int j = 0; j < m_imgHeight; j++ )
    {
      for( int i = 0; i < m_imgWidth; i++, ++inputImageIt, ++labelledImageIt )
      {
        int labelStatusPtrOffset = i + j * m_imgHeight + d * imageFrame;

        //Check if the label == 1 indicates the need for pixel reclassification

        //Uncheck the comment after consulting with Vikram, 
        //Current status returns the best possible result with the
        //flag check suspended

        inputPixelVec = inputImageIt.Get();
        double *dist = m_ClassifierPtr->GetPixelDistance( inputPixelVec );
                      
        for( int index = 0; index <= m_NumClasses ;index++ ) 
          neighborInfluence[index]=0;

        for(int k=0;k<m_KernelSize;k++)
        {
          int widthOffset  = i + m_WidthOffset[k];
          int heightOffset = j + m_HeightOffset[k];
          int depthOffset  = d + m_DepthOffset[k];

          if( ( widthOffset >= 0 ) && ( widthOffset < m_imgWidth ) &&
              ( heightOffset >= 0 ) && ( heightOffset < m_imgHeight ) &&
              ( depthOffset >= 0) && ( depthOffset <m_imgDepth ) )
          {
            //More elegant solution would be to use neighborhood
            //operators but it is currently unstable for VC++

            offsetIndex3D[0] = m_WidthOffset[k] ;
            offsetIndex3D[1] = m_HeightOffset[k] ;
            offsetIndex3D[2] = m_DepthOffset[k]  ;
            offsetIndex3D += labelledImageIt.GetIndex();

            labelledPixel = m_LabelledImage->GetPixel( offsetIndex3D );
                
            //Assuems that the MRF label is an image with 1 value
            //per pixel and is treated as a vector with 1 entry pervector
            int index = (int) labelledPixel;
    
            //Do the prior probability calculations for each class
            //in the 3x3x3 neighborhood
            neighborInfluence[index] += m_Beta3x3x3[k];       

          }// end if

        }// end for 3x3x3 neighborhood processing

        for( int index = 1; index <= m_NumClasses; index++ )
          dist[index - 1] = neighborInfluence[index] - dist[index - 1] ;

        double maxDist = -1e+20;
        int pixLabel = -1;

        for( int index = 0; index < m_NumClasses; index++ )
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

          for( int k = 0; k < m_KernelSize; k++ )
          {
            int widthOffset  = i + m_WidthOffset[k];
            int heightOffset = j + m_HeightOffset[k];
            int depthOffset  = d + m_DepthOffset[k];

            if( ( widthOffset >= 0) && ( widthOffset < m_imgWidth ) &&
                ( heightOffset >= 0) && ( heightOffset < m_imgHeight ) &&
                ( depthOffset >= 0) && ( depthOffset < m_imgDepth ) )
            {

                  offset = m_WidthOffset[k] +
                           m_HeightOffset[k] * m_imgWidth +
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

  delete[] neighborInfluence;
}//ApplyICMlabeller

} // namespace itk







