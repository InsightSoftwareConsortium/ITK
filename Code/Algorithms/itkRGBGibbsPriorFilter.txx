/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBGibbsPriorFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkRGBGibbsPriorFilter_txx
#define _itkRGBGibbsPriorFilter_txx


#include "itkRGBGibbsPriorFilter.h"
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

//typedef itk::Mesh<int>  Mesh;

namespace itk
{
  
/**
 * set intial value of some parameters in the constructor
 */

template <typename TInputImage, typename TClassifiedImage>
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::RGBGibbsPriorFilter()
{
// Modify superclass default values, set default values,
// can be overridden by subclasses
  m_BoundaryGradient = 7;
  m_GibbsNeighborsThreshold = 1;
  m_BoundaryWt = 1;
  m_GibbsPriorWt = 1;
  m_StartPoint[0] = 128;
  m_StartPoint[1] = 128;
  m_StartPoint[2] = 0;
  m_StartRadius = 10;
  m_RecursiveNum = 0;
}

/**
 * Set the labelled image.
 */
template<typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::SetLabelledImage(LabelledImageType image)
{
  m_LabelledImage = image;
  this->Allocate();
}// Set the LabelledImage

/**
 * GenerateInputRequestedRegion method.
 */
template <class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
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
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
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
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GenerateOutputInformation()
{

  typename TInputImage::Pointer input = this->GetInput();
  typename TClassifiedImage::Pointer output = this->GetOutput();
  output->SetLargestPossibleRegion( input->GetLargestPossibleRegion() );

}

/**
 * GenerateMediumImage method.
 */
template <class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GenerateMediumImage()
{

  typename TInputImage::Pointer input = this->GetInput();
  m_MediumImage = TInputImage::New() ;
  m_MediumImage->SetLargestPossibleRegion( input->GetLargestPossibleRegion() );
  m_MediumImage->SetRequestedRegionToLargestPossibleRegion();
  m_MediumImage->SetBufferedRegion( m_MediumImage->GetRequestedRegion() );
  m_MediumImage->Allocate();

/*
  VecImageType::IndexType index = VecImageType::IndexType::ZeroIndex;
  VecImageType::RegionType region;

  region.SetSize( vecImgSize );
  region.SetIndex( index );

  vecImage->SetLargestPossibleRegion( region );
  vecImage->SetBufferedRegion( region );
  vecImage->Allocate();
*/
}

/**
 * allocate the memeory for classified image.
 */
template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::Allocate()
{
  if( m_NumClasses <= 0 )
  {
    throw ExceptionObject(__FILE__, __LINE__);
  }

  InputImageSizeType inputImageSize = m_InputImage->GetBufferedRegion().GetSize();

  //Ensure that the data provided is three dimensional data set
  if(TInputImage::ImageDimension <= 2 )
  {
    throw ExceptionObject(__FILE__, __LINE__);
  }
  
  //---------------------------------------------------------------------
  //Get the image width/height and depth
  //---------------------------------------------------------------------       
  m_imgWidth  = inputImageSize[0];
  m_imgHeight = inputImageSize[1];
  m_imgDepth  = inputImageSize[2];
 
  m_LabelStatus = (unsigned int *) new unsigned int[m_imgWidth*m_imgHeight*m_imgDepth]; 
  for( int index = 0; 
       index < ( m_imgWidth * m_imgHeight * m_imgDepth ); 
       index++ ) 
  {
    m_LabelStatus[index]=1;
  }

}// Allocate

template <typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::SetStartPoint (int x, int y, int z) 
{ 
  m_StartPoint[0] = x; 
  m_StartPoint[1] = y; 
  m_StartPoint[2] = z; 
} 

template <typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GreyScalarBoundary(LabelledImageIndexType Index3D)
{
  int i, change, signs[4], x, numx, rgb;
//  int lowpoint;
  int origin, neighbors[4];

  for (rgb = 0; rgb < m_VecDim; rgb++) {
	origin = (int) m_InputImage->GetPixel( Index3D )[rgb];
	int j = 0;
	for(int i = 0; i < ImageDimension-1; i++) {

	  Index3D[i]--;
	  neighbors[j] = (int) m_InputImage->GetPixel( Index3D )[rgb];
	  Index3D[i]++;
	  j++;

      Index3D[i]++;
      neighbors[j] = (int) m_InputImage->GetPixel( Index3D )[rgb];
      Index3D[i]--;
	  j++;
	}

	for (unsigned int ii=0; ii<4; ii++) 
  {
    signs[ii] = 0;	
  }

// calculate the minimum points of piecewise smoothness  
	m_LowPoint[rgb] = origin;
	change = 1;
	x = origin;
	numx = 1;
	while ( change > 0 ) 
  {
    unsigned int i;
	  change = 0;
	  for (i=0; i<4; i++) 
    {
      if (signs[i] == 0) 
      {
        if (abs(m_LowPoint[rgb] - neighbors[i]) < m_BoundaryGradient) {
        numx++;
        x += neighbors[i];
        signs[i]++;
        change++;
        }
      }
	  }

	  m_LowPoint[rgb] = x/numx;

	  for (i=0; i<4; i++) 
    {
      if (signs[i] == 1) 
      {
        if (abs(m_LowPoint[rgb] - neighbors[i]) > m_BoundaryGradient) 
        {
          numx--;
          x -= neighbors[i];
          signs[i]--;
          change++;
        }
      }
	  }

	  m_LowPoint[rgb] = x/numx;
	}
  }
}

template<typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::SetClassifier( typename ClassifierType::Pointer ptrToClassifier )
{
  if( ( ptrToClassifier == 0 ) || (m_NumClasses <= 0) )
    throw ExceptionObject(__FILE__, __LINE__);

  m_ClassifierPtr = ptrToClassifier;
  m_ClassifierPtr->SetNumClasses( m_NumClasses );
}//end SetClassifier

/*	
template <typename TInputImage, typename TClassifiedImage>
float
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GradientEnergy(InputImageVectorType fi, int i)
{
  float energy, x1, x2;
  int j, f[4], di, dif;
	
  InputImageIterator  inputImageIt(m_InputImage, 
                                   m_InputImage->GetBufferedRegion() );

  LabelledImageIndexType offsetIndex3D = { 0, 0, 0};

  int size = m_imgWidth * m_imgHeight * m_imgDepth;
  int frame = m_imgWidth * m_imgHeight;
  int rowsize = m_imgWidth;

  offsetIndex3D[2] = i / frame;
  offsetIndex3D[1] = (i % frame) / m_imgHeight;
  offsetIndex3D[0] = (i % frame) % m_imgHeight;

  di = (int) m_InputImage->GetPixel( offsetIndex3D )[0];

  if ((i > rowsize - 1)&&((i%rowsize) != rowsize - 1)&&(i < size - rowsize)&&((i%rowsize) != 0)) {

    offsetIndex3D[1]--;
	f[0] = (int) m_InputImage->GetPixel( offsetIndex3D )[0];

    offsetIndex3D[0]--;
    offsetIndex3D[1]++;
    f[1] = (int) m_InputImage->GetPixel( offsetIndex3D )[0];

    offsetIndex3D[0]++;
    offsetIndex3D[0]++;
    f[2] = (int) m_InputImage->GetPixel( offsetIndex3D )[0];

    offsetIndex3D[0]--;
    offsetIndex3D[1]++;
    f[3] = (int) m_InputImage->GetPixel( offsetIndex3D )[0];
  }

  dif = 0;
  energy = 0.0;
  x1 = 0;
  for (j = 0; j < 4; j++) {
	if ( abs(f[j]-fi[0]) > m_BoundaryGradient ) dif = 1;
    x1 = x1 + (fi[0] - f[j])*(fi[0] - f[j])*(1 - dif) + dif*m_BoundaryGradient;
  }

  x1 = m_BoundaryWt*x1;
  x2 = (fi[0] - di)*(fi[0] - di);

  energy = x1 + x2;

  return energy; 
}
*/

template<typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::SetTrainingImage( TrainingImageType image )
{
  m_TrainingImage = image;
}//end SetTrainingImage

template<typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::SetBoundaryGradient( int a )
{
  m_BoundaryGradient = a;
}

template <typename TInputImage, typename TClassifiedImage>
int
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::Sim(int a, int b)
{
  if (a == b) return 1;
  return 0;
}

/**
 * GibbsTotalEnergy method minimizes the local characteristic item
 * in the energy function.
 */
template <typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GibbsTotalEnergy(int i)
{
  LabelledImageIndexType offsetIndex3D = {0, 0, 0};

  int size = m_imgWidth * m_imgHeight * m_imgDepth;
  int frame = m_imgWidth * m_imgHeight;
  int rowsize = m_imgWidth;

  float *energy;
  energy = (float *) malloc(sizeof(float)*m_NumClasses); 
  float maxenergy;
  int label, originlabel;
  maxenergy = 1e+20;

  offsetIndex3D[2] = i / frame;
  offsetIndex3D[1] = (i % frame) / m_imgHeight;
  offsetIndex3D[0] = (i % frame) % m_imgHeight;

  for(int j = 1; j < m_NumClasses; j++) {
    energy[j-1] += GibbsEnergy(i,				0, j);
	energy[j-1] += GibbsEnergy(i + rowsize + 1, 1, j);
	energy[j-1] += GibbsEnergy(i + rowsize,		2, j);
	energy[j-1] += GibbsEnergy(i + rowsize - 1, 3, j);
	energy[j-1] += GibbsEnergy(i - 1,			4, j);
	energy[j-1] += GibbsEnergy(i - rowsize - 1, 5, j);
	energy[j-1] += GibbsEnergy(i - rowsize,		6, j);
	energy[j-1] += GibbsEnergy(i - rowsize + 1, 7, j);
	energy[j-1] += GibbsEnergy(i + 1,			8, j);	
  }

  for(int j = 1; j < m_NumClasses; j++) {
	  if (energy[j-1] < maxenergy) {
		maxenergy = energy[j-1];
		label = j;
	  }
  }

  originlabel = m_LabelledImage->GetPixel(offsetIndex3D);
  if (originlabel != label) {
    m_ErrorCounter++;
	m_LabelledImage->SetPixel(offsetIndex3D, label);
  }
}

template <typename TInputImage, typename TClassifiedImage>
float
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GibbsEnergy(int i, int k, int k1)
{
  LabelledImageIterator  
    labelledImageIt(m_LabelledImage, m_LabelledImage->GetBufferedRegion());

  int f[8];
  int j, neighborcount = 0, simnum = 0, difnum = 0, changenum = 0;
  int changeflag;

  LabelledImageIndexType offsetIndex3D = { 0, 0, 0};
  LabelledImagePixelType labelledPixel;

  int size = m_imgWidth * m_imgHeight * m_imgDepth;
  int frame = m_imgWidth * m_imgHeight;
  int rowsize = m_imgWidth;

  offsetIndex3D[2] = i / frame;
  offsetIndex3D[1] = (i % frame) / m_imgHeight;
  offsetIndex3D[0] = (i % frame) % m_imgHeight;
  
  if (k != 0) labelledPixel = 
	  ( LabelledImagePixelType ) m_LabelledImage->GetPixel( offsetIndex3D );

  if ((i > rowsize - 1)&&((i%rowsize) != rowsize - 1)&&
	  (i < size - rowsize)&&((i%rowsize) != 0)) {

	offsetIndex3D[0]--;
	offsetIndex3D[1]--;
	f[neighborcount++] = (int)m_LabelledImage->GetPixel( offsetIndex3D );
	
	offsetIndex3D[0]++;
	f[neighborcount++] = (int)m_LabelledImage->GetPixel( offsetIndex3D );
  
	offsetIndex3D[0]++;
	f[neighborcount++] = (int)m_LabelledImage->GetPixel( offsetIndex3D );
  
	offsetIndex3D[1]++;  
	f[neighborcount++] = (int)m_LabelledImage->GetPixel( offsetIndex3D );
  
	offsetIndex3D[1]++;
	f[neighborcount++] = (int)m_LabelledImage->GetPixel( offsetIndex3D );
  
	offsetIndex3D[0]--;
	f[neighborcount++] = (int)m_LabelledImage->GetPixel( offsetIndex3D );
  
	offsetIndex3D[0]--;
	f[neighborcount++] = (int)m_LabelledImage->GetPixel( offsetIndex3D );
  
	offsetIndex3D[1]--;
	f[neighborcount++] = (int)m_LabelledImage->GetPixel( offsetIndex3D );

  }

// pixels at the edge of image will be dropped 
  if (neighborcount != 8) return 0.0; 

  if (k != 0) f[k-1] = k1;
  else labelledPixel = k1;
  
  changeflag = (f[0] == labelledPixel)?1:0;

// assuming we are segmenting objects with smooth boundaries, we give 
// weight to such kind of local characteristics
  for(j=0;j<8;j++) {
	if (Sim(f[j], labelledPixel) != changeflag) {
      changenum++;
	  changeflag = 1 - changeflag;
	}
      
    if (changeflag) simnum++;
    else difnum++;
  }
  
  if (changenum < 3) {
    if (/*(simnum==4)||*/(simnum==5)&&(f[0] != f[4])&&(f[2] != f[6])) return -5.0;
  }
  
  if ((difnum==7)||(difnum==8)||(difnum==6)) return 5.0;

  if (simnum == 8) return -5.0;

  return 0;
}

template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::Advance()
{
  this->GenerateData();
}

template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GenerateData()
{
  //First run the Gaussian classifier calculator and
  //generate the Gaussian model for the different classes
  //and then generate the initial labelled dataset.

  m_InputImage = this->GetInput();
  m_VecDim     = InputPixelType::GetVectorDimension();  

  GenerateMediumImage();
 
  //Give the input image and training image set to the  
  //classifier, for the first iteration, use the original image
  //in the following loops, use the result provided by the 
  //deformable model
  if (m_RecursiveNum == 0) {
	m_ClassifierPtr->SetInputImage( m_InputImage );
	// create the training image using the original image
	m_ClassifierPtr->SetTrainingImage( m_TrainingImage );
  } else {
    m_ClassifierPtr->SetInputImage( m_InputImage );
	// create the training image using deformable model
	m_ClassifierPtr->SetTrainingImage( m_TrainingImage );
  }

  //Run the gaussian classifier algorithm
  m_ClassifierPtr->ClassifyImage();

  SetLabelledImage( m_ClassifierPtr->GetClassifiedImage() );

  m_ClassifierPtr->Delete();

  ApplyGPImageFilter();
  //Set the output labelled and allocate the memory
  LabelledImageType outputPtr = this->GetOutput();

  if (m_RecursiveNum == 0) {
	outputPtr->SetLargestPossibleRegion( m_InputImage->GetLargestPossibleRegion() );
	outputPtr->SetBufferedRegion( m_InputImage->GetLargestPossibleRegion() );
//	outputPtr->Allocate();
  }

  //Allocate the output buffer memory 
//  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  //--------------------------------------------------------------------
  //Copy labelling result to the output buffer
  //--------------------------------------------------------------------
  // Set the iterators to the processed image
  //--------------------------------------------------------------------
  LabelledImageIterator  
  labelledImageIt( m_LabelledImage, m_LabelledImage->GetBufferedRegion() );

  //--------------------------------------------------------------------
  // Set the iterators to the output image buffer
  //--------------------------------------------------------------------
  LabelledImageIterator  
    outImageIt( outputPtr, outputPtr->GetBufferedRegion() );

  //--------------------------------------------------------------------

  while ( !outImageIt.IsAtEnd() )
  {
    LabelledImagePixelType labelvalue = 
      ( LabelledImagePixelType ) labelledImageIt.Get();
    outImageIt.Set( labelvalue );
    ++labelledImageIt;
    ++outImageIt;
  }// end while

  m_LabelledImage->Delete();

  m_RecursiveNum++;
        
}// end GenerateData

template<class TInputImage, class TClassifiedImage>
void 
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::ApplyGPImageFilter()
{

  int maxNumPixelError =  
    m_ErrorTollerance * m_imgWidth * m_imgHeight * m_imgDepth;

  int size = m_imgWidth * m_imgHeight * m_imgDepth;
  int rowsize = m_imgWidth;

  int numIter = 0;
  do
  {
    std::cout << "Iteration No." << numIter << std::endl;

    m_ErrorCounter = 0;
    MinimizeFunctional(); // minimize f_1 and f_3;
    numIter += 1;
	std::cout << "Error Num:" << m_ErrorCounter << std::endl;
  } 
  while(( numIter < m_MaxNumIter ) && ( m_ErrorCounter >maxNumPixelError ) ); 

  RegionEraser();
/*
  m_ErrorCounter = 0;
  srand ((unsigned)time(NULL));   
  for (int i = 0; i < size; i++ ) {
    int randomPixel = (int) size*rand()/32768;
	
	if ((randomPixel > (rowsize - 1)) && (randomPixel < (size - rowsize)) 
		&& (randomPixel%rowsize != 0) && (randomPixel%rowsize != rowsize-1)) {
	  GibbsTotalEnergy(randomPixel); // minimized f_2;
	}
  }
*/
//  std::cout << "Gibbs Error Num:" << m_ErrorCounter << std::endl;

// erase noise regions in the image
//  RegionEraser();
}// ApplyMRFImageFilter

template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::MinimizeFunctional()
{
  //This implementation uses the ICM algorithm
  ApplyGibbsLabeller();
}

template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::ApplyGibbsLabeller()
{
  //--------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the input image
  //-------------------------------------------------------------------
  InputImageIterator  inputImageIt(m_InputImage, 
                                   m_InputImage->GetBufferedRegion() );

  InputImageIterator  mediumImageIt(m_MediumImage, 
                                   m_MediumImage->GetBufferedRegion() );

  //--------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the classified image
  //--------------------------------------------------------------------
  LabelledImageIterator  
    labelledImageIt(m_LabelledImage, m_LabelledImage->GetBufferedRegion());

  //Varible to store the origin pixel vector value
  InputImageVectorType OriginPixelVec;

  //Varible to store the origin pixel vector value
  InputImageVectorType ChangedPixelVec;

  //Variable to store the labelled pixel vector
  LabelledImagePixelType labelledPixel;

  //Variable to store the output pixel vector label after
  //the MRF classification
  LabelledImagePixelType outLabelledPix;

  //Set a variable to store the offset index
  LabelledImageIndexType offsetIndex3D = { 0, 0, 0};

  int size = m_imgWidth * m_imgHeight * m_imgDepth;
  int frame = m_imgWidth * m_imgHeight;
  int rowsize = m_imgWidth;

//  srand ((unsigned)time(NULL));   
//  for (int i = 0; i < size; i++ ) {
//    int randomPixel = (int) size*rand()/32768;

  int i = 0;
  while ( !inputImageIt.IsAtEnd() ) {

	offsetIndex3D[2] = i / frame;
	offsetIndex3D[1] = (i % frame) / m_imgHeight;
	offsetIndex3D[0] = (i % frame) % m_imgHeight;

	if ((i > (rowsize - 1)) && (i < (size - rowsize)) 
		&& (i%rowsize != 0) && (i%rowsize != rowsize-1)) {
      OriginPixelVec = inputImageIt.Get();
      GreyScalarBoundary(offsetIndex3D);
	  for (int rgb = 0; rgb < m_VecDim; rgb++) {
		ChangedPixelVec[rgb] = m_LowPoint[rgb];
	  }
//	  if (OriginPixelVec != ChangedPixelVec) {
		mediumImageIt.Set(ChangedPixelVec);
//		m_ErrorCounter++;
//	  }
		double *dist = m_ClassifierPtr->GetPixelDistance( ChangedPixelVec );
		double minDist = 1e+20;
        int pixLabel = -1;

        for( int index = 0; index < m_NumClasses; index++ )
        {
          if ( dist[index] < minDist )
          {
                  minDist = dist[index];
                  pixLabel = index;
          }// if
        }// for
/*
		if (pixLabel == 1) {
		if ( dist[1] > (double)(2.5*m_BoundaryGradient) ) { 
		  pixLabel = 2;
		}
		}
*/
        labelledPixel = 
          ( LabelledImagePixelType ) labelledImageIt.Get();

        //Check if the label has changed then set the change flag in all the 
        //neighborhood of the current pixel
        if( pixLabel != ( int ) labelledPixel )
        {
		  outLabelledPix = pixLabel;
          labelledImageIt.Set( outLabelledPix );
		  m_ErrorCounter++;
		}		
//	  } 
		delete dist;

    }
	i++;
	++labelledImageIt;
	++inputImageIt;
	++mediumImageIt;
  }

  inputImageIt.GoToBegin();
  mediumImageIt.GoToBegin();

  while ( !mediumImageIt.IsAtEnd() )
    {
    ChangedPixelVec = mediumImageIt.Get();
    inputImageIt.Set(ChangedPixelVec);
    ++inputImageIt;
    ++mediumImageIt;
    }

}//ApplyGibbslabeller

template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::RegionEraser()
{
  int i, j, size = m_imgWidth * m_imgHeight * m_imgDepth, count=0;
  m_Region = (unsigned short*) malloc(sizeof(unsigned short)*size);
  m_RegionCount = (unsigned short*) malloc(sizeof(unsigned short)*size);

  LabelledImageIndexType offsetIndex3D = { 0, 0, 0};

  int frame = m_imgWidth * m_imgHeight;
  int rowsize = m_imgWidth;

  LabelledImageIterator  
    labelledImageIt(m_LabelledImage, m_LabelledImage->GetBufferedRegion());

  for ( i=0; i<size; i++ ) {
	m_Region[i] = 0;
	m_RegionCount[i] = 1;
  }

  i = 0;
  int l = 0;
  int label;
  while ( !labelledImageIt.IsAtEnd() ) {
    label = labelledImageIt.Get();
	if (( m_Region[i] == 0 ) && (label != m_ObjectLabel)) {

	  if (LabelRegion(i, ++l, label) < m_ClusterSize) {
		for (j = 0; j < size; j++) {
		  offsetIndex3D[2] = j / frame;
		  offsetIndex3D[1] = (j % frame) / m_imgHeight;
		  offsetIndex3D[0] = (j % frame) % m_imgHeight;
		  if (m_Region[j] == l) {
			m_LabelledImage->SetPixel(offsetIndex3D, m_ObjectLabel);
			m_Region[j] = 0;
		  }
		}
		l--;
	  }

/*
} else {
	    if (LabelRegion(i, ++l, 2) < m_ClusterSize) {
		  for (j = 0; j < size; j++) {
			offsetIndex3D[2] = j / frame;
			offsetIndex3D[1] = (j % frame) / m_imgHeight;
			offsetIndex3D[0] = (j % frame) % m_imgHeight;
			if (m_Region[j] == l) {
			  m_LabelledImage->SetPixel(offsetIndex3D, 1);
			  m_Region[j] = 0;
		    }
		  }
		  l--;
	    }
	  }
*/
	}
	i++;
	++labelledImageIt;
  }
}

template<class TInputImage, class TClassifiedImage>
int
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::LabelRegion(int i, int l, int change)
{
  int count = 1, m;
  int size = m_imgWidth * m_imgHeight * m_imgDepth;
  int frame = m_imgWidth * m_imgHeight;
  int rowsize = m_imgWidth;

  LabelledImageIndexType offsetIndex3D = { 0, 0, 0};

  m_Region[i] = l;

  offsetIndex3D[2] = i / frame;
  offsetIndex3D[1] = (i % frame) / m_imgHeight;
  offsetIndex3D[0] = (i % frame) % m_imgHeight;
  
	
  if (offsetIndex3D[0] > 0) {
    offsetIndex3D[0]--;
	m = m_LabelledImage->GetPixel(offsetIndex3D);
    if ((m==change)&&(m_Region[i-1]==0))
	  count += LabelRegion(i-1, l, change);
	offsetIndex3D[0]++;
  }

  if (offsetIndex3D[0] < m_imgWidth-1) {
    offsetIndex3D[0]++;
	m = m_LabelledImage->GetPixel(offsetIndex3D);
    if ((m==change)&&(m_Region[i+1]==0))
	  count += LabelRegion(i+1, l, change);
	offsetIndex3D[0]--;
  }

  if (offsetIndex3D[1] > 0) {
    offsetIndex3D[1]--;
	m = m_LabelledImage->GetPixel(offsetIndex3D);
    if ((m==change)&&(m_Region[i-rowsize]==0))
	  count += LabelRegion(i-rowsize, l, change);
	offsetIndex3D[1]++;
  }

  if (offsetIndex3D[1] < m_imgHeight-1) {
    offsetIndex3D[1]++;
	m = m_LabelledImage->GetPixel(offsetIndex3D);
    if ((m==change)&&(m_Region[i+rowsize]==0))
	  count += LabelRegion(i+rowsize, l, change);
	offsetIndex3D[1]--;
  }

  return count;
	    
}

} // end namespace itk

#endif
