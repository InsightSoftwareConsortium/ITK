/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHoughTransform2DLinesImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHoughTransform2DLinesImageFilter_txx
#define __itkHoughTransform2DLinesImageFilter_txx

#include "itkHoughTransform2DLinesImageFilter.h"
#include <itkImageRegionConstIteratorWithIndex.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkDiscreteGaussianImageFilter.h>
#include <itkMinimumMaximumImageCalculator.h>

#ifndef PI 
  #define PI 3.1415926535897932384626433832795
#endif


namespace itk
{

/** Constructor */
template<typename TInputPixelType, typename TOutputPixelType>
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType>
::HoughTransform2DLinesImageFilter()
{
  m_Threshold = 0; // by default
  m_AngleResolution = 500;
  m_AngleAxisSize = 500;
  m_NumberOfLines = 1;
  m_DiscRadius = 10;
  m_Variance = 10;
  m_OldModifiedTime = 0;
  m_OldNumberOfLines = 0;
  m_SimplifyAccumulator = NULL;
}


/** Generate the accumulator image */
template<typename TInputPixelType, typename TOutputPixelType>
void
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType>
::GenerateData()
{
  itkDebugMacro(<<"HoughTransform2DLinesImageFilter called");
 
  // Get the input and output pointers
  InputImageConstPointer  m_InputImage  = this->GetInput(0);
  OutputImagePointer m_OutputImage = this->GetOutput(0);

  // Allocate the output
  typename InputImageType::RegionType region;
  Size<2> size;
  size[0]= (long unsigned int)(sqrt(m_AngleAxisSize*m_AngleAxisSize+m_InputImage->GetLargestPossibleRegion().GetSize()[0]*m_InputImage->GetLargestPossibleRegion().GetSize()[0]));
  size[1]= (long unsigned int)m_AngleAxisSize;
  region.SetSize(size);
  region.SetIndex(m_InputImage->GetLargestPossibleRegion().GetIndex());
  m_OutputImage->SetLargestPossibleRegion( region );
  m_OutputImage->SetBufferedRegion( region );
  m_OutputImage->SetRequestedRegion( region );
  
  m_OutputImage->SetOrigin(m_InputImage->GetOrigin());
  m_OutputImage->SetSpacing(m_InputImage->GetSpacing());
  
  m_OutputImage->Allocate();

  ImageRegionConstIteratorWithIndex< InputImageType >  image_it( m_InputImage,  m_InputImage->GetRequestedRegion() );
  image_it.Begin();

  Index<2> m_Index;

  while( !image_it.IsAtEnd() )
  {
    if(image_it.Get()>m_Threshold)
    { 
      for(double angle=-PI;angle<PI;angle+=PI/m_AngleResolution)
      {  
        m_Index[0]=(long unsigned int)(image_it.GetIndex()[0]*cos(angle)+image_it.GetIndex()[1]*sin(angle)); // m_R
        m_Index[1]= (long unsigned int)((m_AngleAxisSize/2)+m_AngleAxisSize*angle/(2*PI)); // m_Theta
  
        if ( (m_Index[0]>0) && (m_Index[0]<=(long)size[0]))
        {
          m_OutputImage->SetPixel(m_Index, m_OutputImage->GetPixel(m_Index)+1);  
        }
      } 
    }
    ++image_it;
  }
}




/** Simplify the accumulator 
 * Do the same iteration process as the Update() method but find the maximum 
 * along the curve and then remove the curve */
template<typename TInputPixelType, typename TOutputPixelType>
void
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType>
::Simplify(void)
{
  // Get the input and output pointers
  InputImageConstPointer  m_InputImage = this->GetInput(0);
  OutputImagePointer m_OutputImage = this->GetOutput(0);

  Size<2> size;
  /** Allocate the simplify accumulator */
  typename InputImageType::RegionType region;
  m_SimplifyAccumulator = OutputImageType::New();
  size[0]= (long unsigned int)(sqrt(m_AngleAxisSize*m_AngleAxisSize+m_InputImage->GetLargestPossibleRegion().GetSize()[0]*m_InputImage->GetLargestPossibleRegion().GetSize()[0]));
  size[1]= (long unsigned int)m_AngleAxisSize;
  region.SetSize(size);
  region.SetIndex(m_InputImage->GetLargestPossibleRegion().GetIndex());

  m_SimplifyAccumulator->SetRegions( region );
  
  m_SimplifyAccumulator->SetOrigin(m_OutputImage->GetOrigin());
  m_SimplifyAccumulator->SetSpacing(m_OutputImage->GetSpacing());
  
  m_SimplifyAccumulator->Allocate();

  Index<2> m_Index;
  Index<2> m_MaxIndex;
  typename OutputImageType::PixelType value;
  typename OutputImageType::PixelType valuemax;
  
  ImageRegionConstIteratorWithIndex< InputImageType >  image_it( m_InputImage,  m_InputImage->GetRequestedRegion() );
  image_it.GoToBegin();


  while( !image_it.IsAtEnd() )
  {
    if(image_it.Get()>m_Threshold)
    { 
      // Look for maximum along the curve and remove the curve at the same time 
      valuemax = -1;
      m_MaxIndex[0]=0;
      m_MaxIndex[1]=0;
      for(double angle=-PI;angle<PI;angle+=PI/m_AngleResolution)
      {  
        m_Index[0]= (long int)(image_it.GetIndex()[0]*cos(angle)+image_it.GetIndex()[1]*sin(angle)); // m_R
        m_Index[1]= (long int)((m_AngleAxisSize/2)+m_AngleAxisSize*angle/(2*PI)); // m_Theta
  
        if ( (m_Index[0]>0) && (m_Index[0]<size[0]) && (m_Index[1]>0) && (m_Index[1]<size[1]))
        {
          value = m_OutputImage->GetPixel(m_Index);
          if( value > valuemax)
          {
            valuemax = value;
            m_MaxIndex = m_Index;
          }
        }
      } 
      m_SimplifyAccumulator->SetPixel(m_MaxIndex,m_SimplifyAccumulator->GetPixel(m_MaxIndex)+1);
    }
    ++image_it;
  }
  
  ImageRegionConstIteratorWithIndex< OutputImageType >  accusimple_it( m_SimplifyAccumulator,  m_SimplifyAccumulator->GetRequestedRegion() );
  ImageRegionIteratorWithIndex< OutputImageType >       accu_it( m_OutputImage,  m_OutputImage->GetRequestedRegion() );

  
  accusimple_it.GoToBegin();
  accu_it.GoToBegin();

  while( !accusimple_it.IsAtEnd() )
  {
    accu_it.Set(accusimple_it.Get());
    ++accu_it;
    ++accusimple_it;
  }

}


/** Get the list of lines. This recomputes the lines */
template<typename TInputPixelType, typename TOutputPixelType>
typename HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType>::LinesListType & 
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType>
::GetLines(unsigned int n)
{
  if((this->GetMTime() == m_OldModifiedTime) && (n == m_OldNumberOfLines)) // if the filter has not been updated
  {
    return m_LinesList;
  }

  m_LinesList.clear();

  /** Blur the accumulator in order to find the maximum */
  typedef Image<float,2> InternalImageType;
  
  OutputImagePointer  outputImage = OutputImageType::New();

  typename OutputImageType::RegionType region;
  region.SetSize(this->GetOutput(0)->GetLargestPossibleRegion().GetSize());
  region.SetIndex(this->GetOutput(0)->GetLargestPossibleRegion().GetIndex());
  outputImage->SetRegions( region );
  outputImage->SetOrigin(this->GetOutput(0)->GetOrigin());
  outputImage->SetSpacing(this->GetOutput(0)->GetSpacing());
  outputImage->Allocate();
  outputImage->FillBuffer(0);

  ImageRegionConstIteratorWithIndex< OutputImageType >  image_it( this->GetOutput(0),  this->GetOutput(0)->GetRequestedRegion() );
  image_it.GoToBegin();

  ImageRegionIterator< InternalImageType >  it( outputImage,  outputImage->GetRequestedRegion() );
 
  while( !image_it.IsAtEnd() )
  {
    it.Set(image_it.Get());
    ++image_it;
    ++it;
  }


  typedef itk::DiscreteGaussianImageFilter<OutputImageType,InternalImageType> GaussianFilterType;
  typename GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();

  gaussianFilter->SetInput(outputImage); // the output is the accumulator image
  double variance[2];
  variance[0] = m_Variance;
  variance[1] = m_Variance;
  gaussianFilter->SetVariance(variance);
  gaussianFilter->Update();
  InternalImageType::Pointer m_PostProcessImage = gaussianFilter->GetOutput();

  typedef itk::MinimumMaximumImageCalculator<InternalImageType> MinMaxCalculatorType;
  typename MinMaxCalculatorType::Pointer minMaxCalculator = MinMaxCalculatorType::New();
  itk::ImageRegionIterator<InternalImageType> it_input(m_PostProcessImage,m_PostProcessImage->GetLargestPossibleRegion());

  itk::Index<2> m_index;

  unsigned int lines=0;
  bool found;

  // Find maxima
  do
  {
    minMaxCalculator->SetImage(m_PostProcessImage);
    minMaxCalculator->ComputeMaximum();
    InternalImageType::PixelType  max = minMaxCalculator->GetMaximum();

    //std::cout << "max =  " << max << std::endl;
    
    found = false;
    for(it_input.GoToBegin();!it_input.IsAtEnd();++it_input)
    {
      if(it_input.Get() == max) 
      { 
        // Create the line
        LineType::PointListType list; // insert two points per line

        double radius = it_input.GetIndex()[0]; 
        double teta   = ((it_input.GetIndex()[1])*2*PI/this->GetAngleResolution())-PI ;
        double Vx = radius*cos( teta );
        double Vy = radius*sin( teta );
        double norm = sqrt(Vx*Vx+Vy*Vy);
        double VxNorm = Vx/norm;
        double VyNorm = Vy/norm;

        if((teta<=0) || (teta >= PI/2) )
        {
          if(teta >= PI/2)
          {
            VyNorm = - VyNorm;
            VxNorm = - VxNorm;
          }

          LinePointType p;
          p.SetPosition(Vx,Vy);
          list.push_back(p);
          p.SetPosition(Vx-VyNorm*5,Vy+VxNorm*5);
          list.push_back(p);
        }
        else // if teta>0
        {
          LinePointType p;
          p.SetPosition(Vx,Vy);
          list.push_back(p);
          p.SetPosition(Vx-VyNorm*5,Vy+VxNorm*5);
          list.push_back(p);
        } // end if(teta>0)
        
       
        // Create a Line Spatial Object
        LinePointer Line = LineType::New();
        Line->SetId(lines);
        Line->SetPoints(list);
        Line->ComputeBoundingBox();

        m_LinesList.push_back(Line);
       
        // Remove a black disc from the hough space domain
        for(double angle = 0; angle <= 2*PI ; angle += PI/1000)
        {     
          for(double lenght = 0; lenght < m_DiscRadius;lenght += 1)
          {
            m_index[0] = (long int)(it_input.GetIndex()[0] + lenght * cos(angle));
            m_index[1] = (long int)(it_input.GetIndex()[1] + lenght * sin(angle));
            if( ((m_index[0]<=(long)m_PostProcessImage->GetLargestPossibleRegion().GetSize()[0]) 
                && (m_index[0]>=0)
                && (m_index[1]<=(long)m_PostProcessImage->GetLargestPossibleRegion().GetSize()[1]) 
                && (m_index[1]>=0)
              )
            )
            {
              m_PostProcessImage->SetPixel(m_index,0);
            }
          } 
        }
        minMaxCalculator->SetImage(m_PostProcessImage);
        minMaxCalculator->ComputeMaximum();
        max = minMaxCalculator->GetMaximum();
      
        lines++;
        found = true;
        if(lines == m_NumberOfLines) break;  
      }
    }
   } while((lines<m_NumberOfLines) && (found));
  
  m_OldModifiedTime = this->GetMTime();
  m_OldNumberOfLines = m_LinesList.size();
  return m_LinesList;
}

/** Print Self information */
template<typename TInputPixelType, typename TOutputPixelType>
void
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  std::cout << "Threshold: "
            << m_Threshold << std::endl;
  std::cout << "Angle Resolution: "
            << m_AngleResolution << std::endl;
  std::cout << "Angle Axis size: "
            << m_AngleAxisSize << std::endl;
  std::cout << "Number Of Lines: " 
            << m_NumberOfLines << std::endl;
  std::cout << "Disc Radius: "
            << m_DiscRadius << std::endl;
  std::cout << "Accumulator blur variance: "
            << m_Variance << std::endl;
  std::cout << "Simplify Accumulator" 
            << m_SimplifyAccumulator << std::endl;

}


} // end namespace

#endif
