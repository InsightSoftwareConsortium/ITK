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

#define RGBGibbsPriorFilterNeedsDebugging 1

namespace itk
{
/* Set intial value of some parameters in the constructor */
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
  m_RecursiveNumber(0),
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
  m_CliqueWeight_2(0.0),
  m_CliqueWeight_3(0.0),
  m_CliqueWeight_4(0.0),
  m_CliqueWeight_5(0.0),
  m_CliqueWeight_6(0.0),
  m_ObjectThreshold(5.0)
{
  m_StartPoint.Fill(0);
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
/* Set the labelled image. */
template<typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::SetLabelledImage(LabelledImageType image)
{
  m_LabelledImage = image;
  this->Allocate();
}

/* GenerateMediumImage method. */
template <class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GenerateMediumImage()
{
  InputImageConstPointer input = this->GetInput();
  m_MediumImage = TInputImage::New() ;
  m_MediumImage->SetLargestPossibleRegion( input->GetLargestPossibleRegion() );
  m_MediumImage->SetRequestedRegionToLargestPossibleRegion();
  m_MediumImage->SetBufferedRegion( m_MediumImage->GetRequestedRegion() );
  m_MediumImage->Allocate();
}

/* Allocate the memeory for classified image. */
template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::Allocate()
{
  /* Get the image width/height and depth. */  
  InputImageSizeType inputImageSize = m_InputImage->GetBufferedRegion().GetSize();
  m_ImageWidth  = inputImageSize[0];
  m_ImageHeight = inputImageSize[1];
  m_ImageDepth  = inputImageSize[2];
 
  const unsigned int numberOfPixels = m_ImageWidth*m_ImageHeight*m_ImageDepth; 
  m_LabelStatus = new LabelType[ numberOfPixels ];
  for( unsigned int index = 0; index < numberOfPixels; index++ ) 
    {
    m_LabelStatus[index]=1;
    }

}

/* Smooth the image in piecewise fashion. */
template <typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GreyScalarBoundary(LabelledImageIndexType Index3D)
{
  int change;
  int signs[4];
  int x;
  int numx;
  LabelType origin;
  LabelType neighbors[4];

  for (unsigned int rgb = 0; rgb < m_VecDim; rgb++) 
    {
    origin = static_cast<LabelType>( m_InputImage->GetPixel( Index3D )[rgb] );
    unsigned int j = 0;

    for(unsigned int i = 0; i < ImageDimension-1; i++) 
      {
      Index3D[i]--;
      neighbors[j] = static_cast<LabelType>( m_InputImage->GetPixel( Index3D )[rgb] );
      Index3D[i]++;
      j++;

      Index3D[i]++;
      neighbors[j] = static_cast<LabelType>( m_InputImage->GetPixel( Index3D )[rgb] );
      Index3D[i]--;
      j++;
      }

    for (unsigned int ii=0; ii<4; ii++) 
      {
      signs[ii] = 0;  
      }

    /* Compute the minimum points of piecewise smoothness */ 
    m_LowPoint[rgb] = origin;
    change = 1;
    x = origin;
    numx = 1;

    while ( change > 0 ) 
      {
      change = 0;
      
      for (unsigned int i=0; i<4; i++) 
        {
        if (signs[i] == 0) 
          {
          const LabelType difference = 
            static_cast< LabelType > ( vnl_math_abs(m_LowPoint[rgb] - neighbors[i]) );
          if ( difference < m_BoundaryGradient ) 
            {
            numx++;
            x += neighbors[i];
            signs[i]++;
            change++;
            }
          }
        }

      m_LowPoint[rgb] = x/numx;

      for (unsigned int i=0; i<4; i++) 
        {
        if (signs[i] == 1) 
          {
          const LabelType difference = 
            static_cast< LabelType > ( vnl_math_abs(m_LowPoint[rgb] - neighbors[i]) );
          if ( difference > m_BoundaryGradient ) 
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

/* Set the classifier. */
template<typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::SetClassifier( typename ClassifierType::Pointer ptrToClassifier )
{
  m_ClassifierPtr = ptrToClassifier;
  m_ClassifierPtr->SetNumberOfClasses( m_NumberOfClasses );
}

/* Set the training image. */
template<typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::SetTrainingImage( TrainingImageType image )
{
  m_TrainingImage = image;
}

/* Check if 2 number are identical. */
template <typename TInputImage, typename TClassifiedImage>
int
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::Sim(int a, int b)
{
  if (a == b) return 1;
  return 0;
}

/* GibbsTotalEnergy method that minimizes the local characteristic(f_2) term
 * in the energy function. */
template <typename TInputImage, typename TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GibbsTotalEnergy(int i)
{
  LabelledImageIndexType offsetIndex3D;
  offsetIndex3D.Fill(0);

  int size = m_ImageWidth * m_ImageHeight * m_ImageDepth;
  int frame = m_ImageWidth * m_ImageHeight;
  int rowsize = m_ImageWidth;

  double energy[2];
  double difenergy;
  LabelType label;
  LabelType originlabel;
  LabelType f[8];
  unsigned int j;
  unsigned int k;
  unsigned int neighborcount=0;

  offsetIndex3D[2] = i / frame;
  offsetIndex3D[1] = (i % frame) / rowsize;
  offsetIndex3D[0] = (i % frame) % rowsize;

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
  f[neighborcount] = (int)m_LabelledImage->GetPixel( offsetIndex3D );
  }

  k = 0;
  for( j=0; j<8; j++ ) 
    {
    if (f[j] == m_ObjectLabel ) 
      {
      k++;
      }
    }

  bool changeflag = (k > 3);

  for(unsigned int j = 0; j < 2; j++) 
    {
    energy[j] = 0;
    energy[j] += GibbsEnergy(i,           0, j);
    energy[j] += GibbsEnergy(i+rowsize+1, 1, j);
    energy[j] += GibbsEnergy(i+rowsize,   2, j);
    energy[j] += GibbsEnergy(i+rowsize-1, 3, j);
    energy[j] += GibbsEnergy(i-1,         4, j);
    energy[j] += GibbsEnergy(i-rowsize-1, 5, j);
    energy[j] += GibbsEnergy(i-rowsize,   6, j);
    energy[j] += GibbsEnergy(i-rowsize+1, 7, j);
    energy[j] += GibbsEnergy(i+1,         8, j);
    if ( m_LabelStatus[i] == j ) 
      {
      energy[j] += -3;
      }
    else 
      {
      energy[j] += 3;  
      }
    }

  if ( energy[0] < energy[1] ) 
    {
    label = 0;
    }
  else 
    {
    label = 1;
    }

  originlabel = m_LabelledImage->GetPixel(offsetIndex3D);
  if (originlabel != label) 
    {
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
::GibbsEnergy(unsigned int i, unsigned int k, unsigned int k1)
{
  LabelledImageRegionIterator  
    labelledImageIt(m_LabelledImage, m_LabelledImage->GetBufferedRegion());

  LabelType f[8];
  int j;
  unsigned int neighborcount = 0;
  int simnum    = 0;
  int changenum = 0;
  bool changeflag;
  double res = 0.0;

  LabelledImageIndexType offsetIndex3D;
  offsetIndex3D.Fill(0);

  LabelledImagePixelType labelledPixel = 0;

  const unsigned int size     = m_ImageWidth * m_ImageHeight * m_ImageDepth;
  const unsigned int frame    = m_ImageWidth * m_ImageHeight;
  const unsigned int rowsize  = m_ImageWidth;

  
  offsetIndex3D[1] = (i % frame) / rowsize;
  offsetIndex3D[0] = (i % frame) % rowsize;
  
  if (k != 0) 
    {
    labelledPixel = 
      ( LabelledImagePixelType ) m_LabelledImage->GetPixel( offsetIndex3D );
    }

  if (     (      i       > rowsize - 1    )
           && ( (i%rowsize) != rowsize - 1    )
           && (      i       < size - rowsize )
           && ( (i%rowsize) != 0              )  ) 
    {

    offsetIndex3D[0]--;
    offsetIndex3D[1]--;
    f[neighborcount++] = static_cast< LabelType > ( m_LabelledImage->GetPixel( offsetIndex3D ) );
  
    offsetIndex3D[0]++;
    f[neighborcount++] = static_cast< LabelType > ( m_LabelledImage->GetPixel( offsetIndex3D ) );
  
    offsetIndex3D[0]++;
    f[neighborcount++] = static_cast< LabelType > ( m_LabelledImage->GetPixel( offsetIndex3D ) );
  
    offsetIndex3D[1]++;  
    f[neighborcount++] = static_cast< LabelType > ( m_LabelledImage->GetPixel( offsetIndex3D ) );
  
    offsetIndex3D[1]++;
    f[neighborcount++] = static_cast< LabelType > ( m_LabelledImage->GetPixel( offsetIndex3D ) );
  
    offsetIndex3D[0]--;
    f[neighborcount++] = static_cast< LabelType > ( m_LabelledImage->GetPixel( offsetIndex3D ) );
  
    offsetIndex3D[0]--;
    f[neighborcount++] = static_cast< LabelType > ( m_LabelledImage->GetPixel( offsetIndex3D ) );

    offsetIndex3D[1]--;
    f[neighborcount++] = static_cast< LabelType > ( m_LabelledImage->GetPixel( offsetIndex3D ) );
    }

  /* Pixels at the edge of image will be dropped. */
  if (neighborcount != 8) 
    {
    return 0.0; 
    }

  if (k != 0) 
    {
    f[k-1] = k1;
    }
  else 
    {
    labelledPixel = k1;
    }
  
  changeflag = (f[0] == labelledPixel);

  /* Assuming we are segmenting objects with smooth boundaries, we give 
    * weight to local characteristics */
  for( j=0; j<8 ;j++ ) 
    {
    if ( (f[j] == labelledPixel) != changeflag ) 
      {
      changenum++;
      changeflag = !changeflag;
      }

    if (changeflag) 
      {
      if (j%2==0) 
        {
        res -= 0.7;
        }
      else 
        {
        res -= 1.0;
        }
      simnum++;
      } 
    }
   
  if (changenum < 3) 
    {
    if ( (simnum==4)||(simnum==5) )
      {
      return res -= m_CliqueWeight_2;
      }
    }

  if ( simnum==8 )
    {
    return res -= m_CliqueWeight_4;
    }
  else return res -=m_CliqueWeight_6; 

}

template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::GenerateData()
{
  /* First run the Gaussian classifier calculator and
   *  generate the Gaussian model for the different classes.
   *  Then generate the initial labelled dataset.*/

  m_InputImage = this->GetInput();
  m_VecDim     = InputPixelType::GetVectorDimension();  

  GenerateMediumImage();
 
  /* Pass the input image and training image set to the  
   *  classifier. For the first iteration, use the original image.
   *  In the following loops, you can use the result provided by a segmentation 
   *  method such as the deformable model. */
  m_ClassifierPtr->SetInputImage( m_InputImage );
  /* Create the training image using the original image or the output 
   *  of a segmentation method such as the deformable model. */
//  m_ClassifierPtr->SetTrainingImage( m_TrainingImage );

  /* Run the Gaussian classifier algorithm. */
  m_ClassifierPtr->Update();

  std::cout<<"Classify finished!"<<std::endl;

  SetLabelledImage( m_ClassifierPtr->GetClassifiedImage() );

  ApplyGPImageFilter();
  /* Set the output labelled image and allocate the memory. */
  LabelledImageType outputPtr = this->GetOutput();

  if (m_RecursiveNumber == 0) {
  outputPtr->SetLargestPossibleRegion( m_InputImage->GetLargestPossibleRegion() );
  outputPtr->SetBufferedRegion( m_InputImage->GetLargestPossibleRegion() );
  }

  /* Allocate the output buffer memory. */
  outputPtr->Allocate();

  /* Copy labelled result to the Output buffer and set the iterators of 
    * the processed image.   */
  LabelledImageRegionIterator  
    labelledImageIt( m_LabelledImage, m_LabelledImage->GetBufferedRegion() );

  /* Set the iterators of the output image buffer. */
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

  m_RecursiveNumber++;

}

template<class TInputImage, class TClassifiedImage>
void 
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::ApplyGPImageFilter()
{
  /* Minimize f_1 and f_3. */
  MinimizeFunctional(); 
}

template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::MinimizeFunctional()
{
  /* This implementation uses the SA algorithm. */

  ApplyGibbsLabeller();

  RegionEraser();

  std::cout<<"Region eraser finished! " <<std::endl; 

#ifndef RGBGibbsPriorFilterNeedsDebugging 
  const unsigned int size = m_ImageWidth * m_ImageHeight * m_ImageDepth;
  const unsigned int rowsize = m_ImageWidth;

  m_Temp = 0;
  srand( static_cast<unsigned int>(time(NULL)) );

  while ( m_Temp < 2 * size ) 
    {
    unsigned int randomPixel = static_cast<unsigned int>( size * rand() / RAND_MAX );
    if ((randomPixel > (rowsize - 1)) && (randomPixel < (size - rowsize)) 
        && (randomPixel%rowsize != 0) && (randomPixel%rowsize != rowsize-1)) 
      {
      GibbsTotalEnergy(randomPixel); // minimized f_2; 
      }
    m_Temp++;
    }
#endif

}

template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::ApplyGibbsLabeller()
{
  /* Set the iterators and the pixel type definition for the input image. */
  InputImageRegionConstIterator  inputImageIt(m_InputImage, 
                                              m_InputImage->GetBufferedRegion() );

  InputImageRegionIterator  mediumImageIt(m_MediumImage, 
                                          m_MediumImage->GetBufferedRegion() );

  /* Set the iterators and the pixel type definition for the classified image. */
  LabelledImageRegionIterator  
    labelledImageIt(m_LabelledImage, m_LabelledImage->GetBufferedRegion());

  /* Variable to store the origin pixel vector value. */
  InputImagePixelType OriginPixelVec;

  /* Variable to store the modified pixel vector value. */
  InputImagePixelType ChangedPixelVec;

  /* Set a variable to store the offset index. */
  LabelledImageIndexType offsetIndex3D; offsetIndex3D.Fill(0);

  double * dist = new double[m_NumberOfClasses];

  const unsigned int size    = m_ImageWidth * m_ImageHeight * m_ImageDepth;
  const unsigned int frame   = m_ImageWidth * m_ImageHeight;
  const unsigned int rowsize = m_ImageWidth;

  inputImageIt.GoToBegin();
  mediumImageIt.GoToBegin();
  labelledImageIt.GoToBegin();

  unsigned int i = 0;
  while ( !inputImageIt.IsAtEnd() ) 
    {

    offsetIndex3D[2] =  i / frame;
    offsetIndex3D[1] = (i % frame) / rowsize;
    offsetIndex3D[0] = (i % frame) % rowsize;

    if ((i > (rowsize - 1)) && (i < (size - rowsize)) 
        && (i%rowsize != 0) && (i%rowsize != rowsize-1)) 
      {
      OriginPixelVec = inputImageIt.Get();
      GreyScalarBoundary(offsetIndex3D);
      for (unsigned int rgb = 0; rgb < m_VecDim; rgb++) 
        {
        ChangedPixelVec[rgb] = m_LowPoint[rgb];
        }
      /* mediumImageIt.Set(ChangedPixelVec); */
      } 
    else 
      {
      ChangedPixelVec = inputImageIt.Get();
      }

    const std::vector<double> & dist =
      m_ClassifierPtr->GetPixelMembershipValue( ChangedPixelVec );

    LabelType pixLabel;
    if (dist[1] > m_ObjectThreshold) pixLabel = 0;
    else pixLabel = 1;
    labelledImageIt.Set( pixLabel );
    
    i++;
    ++labelledImageIt;
    ++inputImageIt;
    ++mediumImageIt;
    }

  delete dist;

}

/* Remove the tiny bias inside the object region. */
template<class TInputImage, class TClassifiedImage>
void
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::RegionEraser()
{

  const unsigned int size = m_ImageWidth * m_ImageHeight * m_ImageDepth;
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

  unsigned short *valid_region_counter = new unsigned short[size];

  LabelledImageRegionIterator  
    labelledImageIt(m_LabelledImage, m_LabelledImage->GetBufferedRegion());

  for ( unsigned int i=0; i<size; i++ ) 
    {
    m_Region[i] = 0;
    m_RegionCount[i] = 1;
    valid_region_counter[i] = 0;
    }

  LabelType i = NumericTraits< LabelType >::Zero;
  LabelType k = NumericTraits< LabelType >::Zero;
  LabelType l = NumericTraits< LabelType >::Zero;
  LabelType label;

  while ( !labelledImageIt.IsAtEnd() ) 
    {
    if ( m_Region[i] == 0 ) 
      {
      label = labelledImageIt.Get();
      if ( LabelRegion(i, ++l, label) > m_ClusterSize ) 
        {
        valid_region_counter[k] = l;
        k++;
        }
      }

    i++;
    ++labelledImageIt;
    }

  i = 0;
  unsigned int j; 
  labelledImageIt.GoToBegin();

  while ( !labelledImageIt.IsAtEnd() ) 
    {
    j = 0;
    while ( (m_Region[i] != valid_region_counter[j]) && (j < k) ) 
      {
      j++;
      }

    if (j == k) 
      {
      label = labelledImageIt.Get();
      labelledImageIt.Set(1-label);
      }
    i++;
    ++labelledImageIt;
    }

  delete []valid_region_counter;

}



template<class TInputImage, class TClassifiedImage>
unsigned int
RGBGibbsPriorFilter<TInputImage, TClassifiedImage>
::LabelRegion(int i, int l, int change)
{
  unsigned int count = 1; 
  int m;
  const unsigned int frame   = m_ImageWidth * m_ImageHeight;
  const unsigned int rowsize = m_ImageWidth;


  LabelledImageIndexType offsetIndex3D; 
  offsetIndex3D.Fill(0);

  m_Region[i] = l;

  offsetIndex3D[2] = i / frame;
  offsetIndex3D[1] = (i % frame) / rowsize;
  offsetIndex3D[0] = (i % frame) % rowsize;

  if (offsetIndex3D[0] > 0) 
    {
    offsetIndex3D[0]--;
    m = m_LabelledImage->GetPixel(offsetIndex3D);
    if ((m==change)&&(m_Region[i-1]==0))
      {
      count += LabelRegion(i-1, l, change);
      }
    offsetIndex3D[0]++;
    }

  if (offsetIndex3D[0] < static_cast<IndexValueType>( m_ImageWidth-1 ) ) 
    {
    offsetIndex3D[0]++;
    m = m_LabelledImage->GetPixel(offsetIndex3D);
    if ((m==change)&&(m_Region[i+1]==0))
      {
      count += LabelRegion(i+1, l, change);
      }
    offsetIndex3D[0]--;
    }

  if (offsetIndex3D[1] > 0) 
    {
    offsetIndex3D[1]--;
    m = m_LabelledImage->GetPixel(offsetIndex3D);
    if ((m==change)&&(m_Region[i-rowsize]==0))
      {
      count += LabelRegion(i-rowsize, l, change);
      }
    offsetIndex3D[1]++;
    }

  if (offsetIndex3D[1] < static_cast<IndexValueType>( m_ImageHeight-1 ) ) 
    {
    offsetIndex3D[1]++;
    m = m_LabelledImage->GetPixel(offsetIndex3D);
    if ((m==change)&&(m_Region[i+rowsize]==0))
      {
      count += LabelRegion(i+rowsize, l, change);
      }
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
  os << indent << "ObjectThreshold: " 
     << m_ObjectThreshold << std::endl;
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
  os << indent << "CliqueWeight_5: "
     << m_CliqueWeight_5 << std::endl;
  os << indent << "CliqueWeight_6: "
     << m_CliqueWeight_6 << std::endl;
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

} /* end namespace itk. */

#endif
