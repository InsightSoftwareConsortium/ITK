/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBGibbsPriorFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRGBGibbsPriorFilter_txx
#define _itkRGBGibbsPriorFilter_txx

#include "itkRGBGibbsPriorFilter.h"
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

namespace itk
{
/** Set intial value of some parameters in the constructor */
template <typename TInputImage, typename TClassifiedImage>
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::RGBGibbsPriorFilter(void):
    m_InputImage(0),
    m_TrainingImage(0),
    m_LabelledImage(0),
    m_NumberOfClasses(0),
    m_MaximumNumberOfIterations(10),
    m_ClassifierPtr(0),
    m_BoundaryGradient(7),
    m_BoundaryWeight(1),
    m_GibbsPriorWeight(1),
    m_StartRadius(10),
    m_RecursiveNum(0),
    m_LabelStatus(0),
    m_MediumImage(0),
    m_Temp(0),
    m_ImageWidth(0),
    m_ImageHeight(0),
    m_ImageDepth(0),
    m_ClusterSize(10),
    m_ObjectLabel(1),
    m_VecDim(0),
    m_LowPoint(),
    m_Region(NULL),
    m_RegionCount(NULL),
    m_CliqueWeight_1(0.0),
    m_CliqueWeight_2(-1.2),
    m_CliqueWeight_3(0.0),
    m_CliqueWeight_4(0.0)
{
  m_StartPoint[0] = m_StartPoint[1] = m_StartPoint[2] = 0;
}

template <typename TInputImage, typename TClassifiedImage>
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::~RGBGibbsPriorFilter()
{
  if (m_Region)
    {
    delete [] m_Region;
    }
  if (m_RegionCount)
    {
    delete [] m_RegionCount;
    }
  if (m_LabelStatus)
    {
    delete [] m_LabelStatus;
    }
}
/** Set the labelled image. */
template<typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::SetLabelledImage(LabelledImageType image)
{
  m_LabelledImage = image;
  this->Allocate();
}

/** GenerateMediumImage method. */
template <class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GenerateMediumImage()
{
  InputImageType input = this->GetInput();
  m_MediumImage = TInputImage::New() ;
  m_MediumImage->SetLargestPossibleRegion( input->GetLargestPossibleRegion() );
  m_MediumImage->SetRequestedRegionToLargestPossibleRegion();
  m_MediumImage->SetBufferedRegion( m_MediumImage->GetRequestedRegion() );
  m_MediumImage->Allocate();
}

/** Allocate the memeory for classified image. */
template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::Allocate()
{
  /** Get the image width/height and depth. */  
  InputImageSizeType inputImageSize = m_InputImage->GetBufferedRegion().GetSize();
  m_ImageWidth  = inputImageSize[0];
  m_ImageHeight = inputImageSize[1];
  m_ImageDepth  = inputImageSize[2];
 
  m_LabelStatus = (unsigned int *) new unsigned int[m_ImageWidth*m_ImageHeight*m_ImageDepth]; 
  for( int index = 0; 
      index < ( m_ImageWidth * m_ImageHeight * m_ImageDepth ); 
      index++ ) 
  {
    m_LabelStatus[index]=1;
  }

}

/** Smooth the image in piecewise fashion. */
template <typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GreyScalarBoundary(LabelledImageIndexType Index3D)
{
  int change, signs[4], x, numx, rgb;
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

    for (unsigned int ii=0; ii<4; ii++) signs[ii] = 0;  

    /** Compute the minimum points of piecewise smoothness */ 
    m_LowPoint[rgb] = origin;
    change = 1;
    x = origin;
    numx = 1;

    while ( change > 0 ) {
      unsigned int i;
      change = 0;
      
      for (i=0; i<4; i++) {
        if (signs[i] == 0) {
          if (abs(m_LowPoint[rgb] - neighbors[i]) < m_BoundaryGradient) {
            numx++;
            x += neighbors[i];
            signs[i]++;
            change++;
          }
        }
      }

      m_LowPoint[rgb] = x/numx;

      for (i=0; i<4; i++) {
        if (signs[i] == 1) {
          if (abs(m_LowPoint[rgb] - neighbors[i]) > m_BoundaryGradient) {
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

/** Set the classifier. */
template<typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::SetClassifier( typename ClassifierType::Pointer ptrToClassifier )
{
  m_ClassifierPtr = ptrToClassifier;
  m_ClassifierPtr->SetNumberOfClasses( m_NumberOfClasses );
}

/** Set the training image. */
template<typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::SetTrainingImage( TrainingImageType image )
{
  m_TrainingImage = image;
}

/** Check if 2 number are identical. */
template <typename TInputImage, typename TClassifiedImage>
int
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::Sim(int a, int b)
{
  if (a == b) return 1;
  return 0;
}

/** GibbsTotalEnergy method that minimizes the local characteristic(f_2) term
 * in the energy function. */
template <typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GibbsTotalEnergy(int i)
{
  LabelledImageIndexType offsetIndex3D = LabelledImageIndexType::ZeroIndex;

  int size = m_ImageWidth * m_ImageHeight * m_ImageDepth;
  int frame = m_ImageWidth * m_ImageHeight;
  int rowsize = m_ImageWidth;

  double energy[2];
  double difenergy;
  int label, originlabel, f[8], j, k, neighborcount=0;

  offsetIndex3D[2] = i / frame;
  offsetIndex3D[1] = (i % frame) / m_ImageHeight;
  offsetIndex3D[0] = (i % frame) % m_ImageHeight;

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

  k = 0;
  for(j=0;j<8;j++) {
    if (f[j] == m_ObjectLabel) k++;
  }

  bool changeflag = (k > 3);

  for(int j = 0; j < 2; j++) {
    energy[j] = 0;
    energy[j] += GibbsEnergy(i,       0, j);
    energy[j] += GibbsEnergy(i+rowsize+1, 1, j);
    energy[j] += GibbsEnergy(i+rowsize,   2, j);
    energy[j] += GibbsEnergy(i+rowsize-1, 3, j);
    energy[j] += GibbsEnergy(i-1,     4, j);
    energy[j] += GibbsEnergy(i-rowsize-1, 5, j);
    energy[j] += GibbsEnergy(i-rowsize,   6, j);
    energy[j] += GibbsEnergy(i-rowsize+1, 7, j);
    energy[j] += GibbsEnergy(i+1,     8, j);
    if ( m_LabelStatus[i] == j ) energy[j] += -3;
    else energy[j] += 3;  
  }

  if (energy[0] < energy[1]) label = 0;
  else label = 1;

  originlabel = m_LabelledImage->GetPixel(offsetIndex3D);
  if (originlabel != label) {
    m_LabelledImage->SetPixel(offsetIndex3D, label);
  }
  

  else {
    if (changeflag) {
      difenergy = energy[label]-energy[1-label];
      double rand_num = (double) (rand()/32768.0);
      double energy_num = (double) (exp((double) (difenergy*0.5*size/(2*size-m_Temp))));
      if ( rand_num < energy_num ) m_LabelledImage->SetPixel(offsetIndex3D, 1-label);
    }
  }
}

template <typename TInputImage, typename TClassifiedImage>
double
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GibbsEnergy(int i, int k, int k1)
{
  LabelledImageRegionIterator  
    labelledImageIt(m_LabelledImage, m_LabelledImage->GetBufferedRegion());

  int f[8];
  int j, neighborcount = 0, simnum = 0, difnum = 0, changenum = 0;
  bool changeflag;
  double res = 0.0;

  LabelledImageIndexType offsetIndex3D = LabelledImageIndexType::ZeroIndex;
  LabelledImagePixelType labelledPixel;

  int size = m_ImageWidth * m_ImageHeight * m_ImageDepth;
  int frame = m_ImageWidth * m_ImageHeight;
  int rowsize = m_ImageWidth;

  offsetIndex3D[2] = i / frame;
  offsetIndex3D[1] = (i % frame) / m_ImageHeight;
  offsetIndex3D[0] = (i % frame) % m_ImageHeight;
  
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

  /** Pixels at the edge of image will be dropped. */
  if (neighborcount != 8) return 0.0; 

  if (k != 0) f[k-1] = k1;
  else labelledPixel = k1;
  
  changeflag = (f[0] == labelledPixel);

  /** Assuming we are segmenting objects with smooth boundaries, we give 
    * weight to local characteristics */
  for(j=0;j<8;j++) {
    if ((f[j] == labelledPixel) != changeflag) {
      changenum++;
      changeflag = !changeflag;
    }

    if (changeflag) {
      if (j%2==0) res -= 0.7;
      else res -= 1.0;
      simnum++;
    } else difnum++;
  }
   
  if (changenum < 3) 
    {
    if ( (simnum==4)||(simnum==5) ) return res -= m_CliqueWeight_2;
    }

  return res;
}

template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GenerateData()
{
  /** First run the Gaussian classifier calculator and
   *  generate the Gaussian model for the different classes.
   *  Then generate the initial labelled dataset.*/

  m_InputImage = this->GetInput();
  m_VecDim     = InputPixelType::GetVectorDimension();  

  GenerateMediumImage();
 
  /** Pass the input image and training image set to the  
   *  classifier. For the first iteration, use the original image.
   *  In the following loops, you can use the result provided by a segmentation 
   *  method such as the deformable model. */
  m_ClassifierPtr->SetInputImage( m_InputImage );
  /** Create the training image using the original image or the output 
   *  of a segmentation method such as the deformable model. */
  m_ClassifierPtr->SetTrainingImage( m_TrainingImage );

  /** Run the Gaussian classifier algorithm. */
  m_ClassifierPtr->ClassifyImage();

  std::cout<<"Classify finished!"<<std::endl;

  SetLabelledImage( m_ClassifierPtr->GetClassifiedImage() );

  ApplyGPImageFilter();
  /** Set the output labelled image and allocate the memory. */
  LabelledImageType outputPtr = this->GetOutput();

  if (m_RecursiveNum == 0) {
    outputPtr->SetLargestPossibleRegion( m_InputImage->GetLargestPossibleRegion() );
    outputPtr->SetBufferedRegion( m_InputImage->GetLargestPossibleRegion() );
  }

  /** Allocate the output buffer memory. */
  outputPtr->Allocate();

  /** Copy labelled result to the Output buffer and set the iterators of 
    * the processed image.   */
  LabelledImageRegionIterator  
    labelledImageIt( m_LabelledImage, m_LabelledImage->GetBufferedRegion() );

  /** Set the iterators of the output image buffer. */
  LabelledImageRegionIterator  
    outImageIt( outputPtr, outputPtr->GetBufferedRegion() );

  while ( !outImageIt.IsAtEnd() )
    {
    LabelledImagePixelType labelvalue = 
      ( LabelledImagePixelType ) labelledImageIt.Get();
    outImageIt.Set( labelvalue );
    ++labelledImageIt;
    ++outImageIt;
  }

  m_RecursiveNum++;

}

template<class TInputImage, class TClassifiedImage>
void 
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::ApplyGPImageFilter()
{
 /** Minimize f_1 and f_3. */
  MinimizeFunctional(); 
}

template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::MinimizeFunctional()
{
  /** This implementation uses the SA algorithm. */

  ApplyGibbsLabeller();

  RegionEraser();

  std::cout<<"Region eraser finished! " <<std::endl; 
  int size = m_ImageWidth * m_ImageHeight * m_ImageDepth;
  int rowsize = m_ImageWidth;

  m_Temp = 0;
  srand ((unsigned)time(NULL));

  while ( m_Temp < 2*m_ImageWidth*m_ImageDepth*m_ImageHeight ) {
    int randomPixel = (int) size*rand()/32768;
    if ((randomPixel > (rowsize - 1)) && (randomPixel < (size - rowsize)) 
        && (randomPixel%rowsize != 0) && (randomPixel%rowsize != rowsize-1)) {
      GibbsTotalEnergy(randomPixel); /** minimized f_2; */
    }
    m_Temp++;
  }
}

template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::ApplyGibbsLabeller()
{
  /** Set the iterators and the pixel type definition for the input image. */
  InputImageRegionIterator  inputImageIt(m_InputImage, 
                                   m_InputImage->GetBufferedRegion() );

  InputImageRegionIterator  mediumImageIt(m_MediumImage, 
                                    m_MediumImage->GetBufferedRegion() );

  /** Set the iterators and the pixel type definition for the classified image. */
  LabelledImageRegionIterator  
    labelledImageIt(m_LabelledImage, m_LabelledImage->GetBufferedRegion());

  /** Variable to store the origin pixel vector value. */
  InputImagePixelType OriginPixelVec;

  /** Variable to store the modified pixel vector value. */
  InputImagePixelType ChangedPixelVec;

  /** Variable to store the output pixel vector label after
   *  the MRF classification. */
  LabelledImagePixelType outLabelledPix;

  /** Set a variable to store the offset index. */
  LabelledImageIndexType offsetIndex3D; offsetIndex3D.Fill(0);

  double * dist = new double[m_NumberOfClasses];

  int size = m_ImageWidth * m_ImageHeight * m_ImageDepth;
  int frame = m_ImageWidth * m_ImageHeight;
  int rowsize = m_ImageWidth;

  inputImageIt.GoToBegin();
  mediumImageIt.GoToBegin();

  int i = 0;
  while ( !inputImageIt.IsAtEnd() ) {

    offsetIndex3D[2] = i / frame;
    offsetIndex3D[1] = (i % frame) / m_ImageHeight;
    offsetIndex3D[0] = (i % frame) % m_ImageHeight;

    if ((i > (rowsize - 1)) && (i < (size - rowsize)) 
        && (i%rowsize != 0) && (i%rowsize != rowsize-1)) {
      OriginPixelVec = inputImageIt.Get();
      GreyScalarBoundary(offsetIndex3D);
      for (int rgb = 0; rgb < m_VecDim; rgb++) {
        ChangedPixelVec[rgb] = m_LowPoint[rgb];
      }
    /** mediumImageIt.Set(ChangedPixelVec); */
    } 
    else ChangedPixelVec = inputImageIt.Get();
    m_ClassifierPtr->GetPixelDistance( ChangedPixelVec, dist );
    double minDist = 1e+20;
    int pixLabel = -1;

    for( int index = 0; index < m_NumberOfClasses; index++ ) {
      if ( dist[index] < minDist ) {
        minDist = dist[index];
        pixLabel = index;
      }
    }

    /** Check if the label has changed then set the change flag in all the 
     *  neighborhood of the current pixel. */
    if( pixLabel != m_ObjectLabel ) {
      outLabelledPix = 1-m_ObjectLabel;
      labelledImageIt.Set( outLabelledPix );
      m_LabelStatus[i] = outLabelledPix;
    } else {
      labelledImageIt.Set( m_ObjectLabel );
      m_LabelStatus[i] = m_ObjectLabel; 
    }

    i++;
    ++labelledImageIt;
    ++inputImageIt;
    ++mediumImageIt;
  }

  delete dist;

}

/** Remove the tiny bias inside the object region. */
template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::RegionEraser()
{
  int i, j, k, size = m_ImageWidth * m_ImageHeight * m_ImageDepth;
  if (m_Region)
    {
    delete [] m_Region;
    }
  m_Region = new unsigned short[size];

  if (m_RegionCount)
    {
    delete [] m_RegionCount;
    }
  m_RegionCount = new unsigned short[size];

  unsigned short *valid_region_counter = new unsigned short[size/100];

  LabelledImageRegionIterator  
    labelledImageIt(m_LabelledImage, m_LabelledImage->GetBufferedRegion());

  for ( i=0; i<size; i++ ) {
    m_Region[i] = 0;
    m_RegionCount[i] = 1;
  }

  for ( i=0; i<size/100; i++ ) {
    valid_region_counter[i] = 0;
  }
  i = 0;
  k = 0;
  int l = 0;
  int label;

  while ( !labelledImageIt.IsAtEnd() ) {
    if ( m_Region[i] == 0 ) {
      label = labelledImageIt.Get();
      if (LabelRegion(i, ++l, label) > m_ClusterSize) {
        valid_region_counter[k] = l;
        k++;
      }
    }

    i++;
    ++labelledImageIt;
  }

  i = 0; 
  labelledImageIt.GoToBegin();

  while ( !labelledImageIt.IsAtEnd() ) 
    {
    j = 0;
    while ( (m_Region[i] != valid_region_counter[j]) && (j < k) ) {
      j++;
    }

    if (j == k) {
      label = labelledImageIt.Get();
      labelledImageIt.Set(1-label);
    }
    i++;
    ++labelledImageIt;
  }

  delete []valid_region_counter;

}

template<class TInputImage, class TClassifiedImage>
int
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::LabelRegion(int i, int l, int change)
{
  int count = 1, m;
  int frame = m_ImageWidth * m_ImageHeight;
  int rowsize = m_ImageWidth;

  LabelledImageIndexType offsetIndex3D; offsetIndex3D.Fill(0);
  m_Region[i] = l;

  offsetIndex3D[2] = i / frame;
  offsetIndex3D[1] = (i % frame) / m_ImageHeight;
  offsetIndex3D[0] = (i % frame) % m_ImageHeight;

  if (offsetIndex3D[0] > 0) {
    offsetIndex3D[0]--;
    m = m_LabelledImage->GetPixel(offsetIndex3D);
    if ((m==change)&&(m_Region[i-1]==0))
    count += LabelRegion(i-1, l, change);
    offsetIndex3D[0]++;
  }

  if (offsetIndex3D[0] < m_ImageWidth-1) 
    {
    offsetIndex3D[0]++;
    m = m_LabelledImage->GetPixel(offsetIndex3D);
    if ((m==change)&&(m_Region[i+1]==0))
    count += LabelRegion(i+1, l, change);
    offsetIndex3D[0]--;
  }

  if (offsetIndex3D[1] > 0) 
    {
    offsetIndex3D[1]--;
    m = m_LabelledImage->GetPixel(offsetIndex3D);
    if ((m==change)&&(m_Region[i-rowsize]==0))
    count += LabelRegion(i-rowsize, l, change);
    offsetIndex3D[1]++;
  }

  if (offsetIndex3D[1] < m_ImageHeight-1) 
    {
    offsetIndex3D[1]++;
    m = m_LabelledImage->GetPixel(offsetIndex3D);
    if ((m==change)&&(m_Region[i+rowsize]==0))
    count += LabelRegion(i+rowsize, l, change);
    offsetIndex3D[1]--;
  }

  return count;
      
}

template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "NumberOfClasses: " 
     << m_NumberOfClasses << std::endl;
  os << indent << "MaximumNumberOfIterations: " 
     << m_MaximumNumberOfIterations << std::endl;
  os << indent << "BoundaryGradient: "
     << m_BoundaryGradient << std::endl;
  os << indent << "CliqueWeight_1: "
     << m_CliqueWeight_1 << std::endl;
  os << indent << "CliqueWeight_2: "
     << m_CliqueWeight_2 << std::endl;
  os << indent << "CliqueWeight_3: "
     << m_CliqueWeight_3 << std::endl;
  os << indent << "CliqueWeight_4: "
     << m_CliqueWeight_4 << std::endl;
  os << indent << "ClusterSize: " 
     << m_ClusterSize << std::endl;
  os << indent << "ObjectLabel: " 
     << m_ObjectLabel << std::endl;
  os << indent << "StartPoint: "
     << m_StartPoint << std::endl;
  if (m_TrainingImage)
    {
    os << "TraingImage: " << m_TrainingImage;
    }
  if (m_LabelledImage)
    {
    os << "TrainingImage: " << m_TrainingImage;
    }
  if (m_ClassifierPtr)
    {
    os << "ClassifierPtr: " << m_ClassifierPtr;
    }
}

} /** end namespace itk. */

#endif
