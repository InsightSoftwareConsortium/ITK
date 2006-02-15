/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHoughTransform2DLinesImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
#include <itkCastImageFilter.h>

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
  m_NumberOfLines = 1;
  m_DiscRadius = 10;
  m_Variance = 5;
  m_OldModifiedTime = 0;
  m_OldNumberOfLines = 0;
  m_SimplifyAccumulator = NULL;
}

template<typename TInputPixelType, typename TOutputPixelType>
void 
HoughTransform2DLinesImageFilter<TInputPixelType,TOutputPixelType>
::EnlargeOutputRequestedRegion(DataObject *output)
{
  // call the superclass' implementation of this method
  Superclass::EnlargeOutputRequestedRegion(output);

  output->SetRequestedRegionToLargestPossibleRegion();
}


template<typename TInputPixelType, typename TOutputPixelType>
void 
HoughTransform2DLinesImageFilter<TInputPixelType,TOutputPixelType>
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  InputImageConstPointer  input  = this->GetInput();
  OutputImagePointer      output = this->GetOutput();

  if ( !input || !output )
    {
    return;
    }

  // Compute the size of the output image
  typename InputImageType::RegionType region;
  Size<2> size;
  size[0]= (long unsigned int)(sqrt(m_AngleResolution*m_AngleResolution+input->GetLargestPossibleRegion().GetSize()[0]*input->GetLargestPossibleRegion().GetSize()[0]));
  size[1]= (long unsigned int)m_AngleResolution;
  region.SetSize(size);
  region.SetIndex(input->GetLargestPossibleRegion().GetIndex());

  output->SetLargestPossibleRegion( region );
}


template<typename TInputPixelType, typename TOutputPixelType>
void 
HoughTransform2DLinesImageFilter<TInputPixelType,TOutputPixelType>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
    {
    InputImagePointer image = 
      const_cast< InputImageType * >( this->GetInput() );
    image->SetRequestedRegionToLargestPossibleRegion();
    }
}


/** Generate the accumulator image */
template<typename TInputPixelType, typename TOutputPixelType>
void
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType>
::GenerateData()
{
  itkDebugMacro(<<"HoughTransform2DLinesImageFilter called");

  // Get the input and output pointers
  InputImageConstPointer  inputImage  = this->GetInput(0);
  OutputImagePointer outputImage = this->GetOutput(0);

  // Allocate the output
  this->AllocateOutputs();
  outputImage->FillBuffer(0);

  ImageRegionConstIteratorWithIndex< InputImageType >  image_it( inputImage,  inputImage->GetRequestedRegion() );
  image_it.Begin();

  Index<2> index;

  while( !image_it.IsAtEnd() )
    {
    if(image_it.Get()>m_Threshold)
      { 
      for(double angle=-PI;angle<PI;angle+=PI/m_AngleResolution)
        {  
        index[0]=(long unsigned int)(image_it.GetIndex()[0]*cos(angle)+image_it.GetIndex()[1]*sin(angle)); // m_R
        index[1]= (long unsigned int)((m_AngleResolution/2)+m_AngleResolution*angle/(2*PI)); // m_Theta
  
        if ( (index[0]>0) && (index[0]<=(long)outputImage->GetBufferedRegion().GetSize()[0])) // the preceeding "if" should be replacable with "if ( outputImage->GetBufferedRegion().IsInside(index) )" but the algorithm fails if it is
          {
          outputImage->SetPixel(index, outputImage->GetPixel(index)+1);  
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
  InputImageConstPointer  inputImage = this->GetInput(0);
  OutputImagePointer outputImage = this->GetOutput(0);

  if(!inputImage || !outputImage)
    {
    itkExceptionMacro("Update() must be called before Simplify().");
    } 

  /** Allocate the simplify accumulator */
  m_SimplifyAccumulator = OutputImageType::New();
  m_SimplifyAccumulator->SetRegions( outputImage->GetLargestPossibleRegion() );
  m_SimplifyAccumulator->SetOrigin(inputImage->GetOrigin());
  m_SimplifyAccumulator->SetSpacing(inputImage->GetSpacing());
  m_SimplifyAccumulator->Allocate();
  m_SimplifyAccumulator->FillBuffer(0);

  Index<2> index;
  Index<2> maxIndex;
  typename OutputImageType::PixelType value;
  typename OutputImageType::PixelType valuemax;
  
  ImageRegionConstIteratorWithIndex< InputImageType >  image_it( inputImage,  inputImage->GetRequestedRegion() );
  image_it.GoToBegin();


  while( !image_it.IsAtEnd() )
    {
    if(image_it.Get()>m_Threshold)
      { 
      // Look for maximum along the curve and remove the curve at the same time 
      valuemax = -1;
      maxIndex[0]=0;
      maxIndex[1]=0;
      for(double angle=-PI;angle<PI;angle+=PI/m_AngleResolution)
        {  
        index[0]= (long int)(image_it.GetIndex()[0]*cos(angle)+image_it.GetIndex()[1]*sin(angle)); // m_R
        index[1]= (long int)((m_AngleResolution/2)+m_AngleResolution*angle/(2*PI)); // m_Theta
  
        if ( outputImage->GetBufferedRegion().IsInside(index) )
          {
          value = outputImage->GetPixel(index);
          if( value > valuemax)
            {
            valuemax = value;
            maxIndex = index;
            }
          }
        } 
      m_SimplifyAccumulator->SetPixel(maxIndex,m_SimplifyAccumulator->GetPixel(maxIndex)+1);
      }
    ++image_it;
    }
  
  ImageRegionConstIteratorWithIndex< OutputImageType >  accusimple_it( m_SimplifyAccumulator,  m_SimplifyAccumulator->GetRequestedRegion() );
  ImageRegionIteratorWithIndex< OutputImageType >       accu_it( outputImage,  outputImage->GetRequestedRegion() );

  
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
  typedef float          InternalImagePixelType;
  typedef Image< InternalImagePixelType,2 > InternalImageType;

  OutputImagePointer outputImage = this->GetOutput(0); 

  if( !outputImage )
    {
    itkExceptionMacro("Update() must be called before GetLines().");
    } 

  /** Convert the accumulator output image type to internal image type*/
  typedef CastImageFilter< OutputImageType, InternalImageType> CastImageFilterType;

  typename CastImageFilterType::Pointer castImageFilter = CastImageFilterType::New();
  castImageFilter->SetInput(outputImage);
  
  typedef DiscreteGaussianImageFilter<InternalImageType,InternalImageType> GaussianFilterType;
  typename GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();

  gaussianFilter->SetInput(castImageFilter->GetOutput()); // the output is the accumulator image
  double variance[2];
  variance[0] = m_Variance;
  variance[1] = m_Variance;
  gaussianFilter->SetVariance(variance);
  gaussianFilter->Update();
  InternalImageType::Pointer postProcessImage = gaussianFilter->GetOutput();

  typedef MinimumMaximumImageCalculator<InternalImageType> MinMaxCalculatorType;
  typename MinMaxCalculatorType::Pointer minMaxCalculator = MinMaxCalculatorType::New();
  itk::ImageRegionIterator<InternalImageType> 
                     it_input(postProcessImage,postProcessImage->GetLargestPossibleRegion());

  itk::Index<2> index;

  unsigned int lines=0;
  bool found;

  // Find maxima
  do
    {
    minMaxCalculator->SetImage(postProcessImage);
    minMaxCalculator->ComputeMaximum();
    InternalImageType::PixelType  max = minMaxCalculator->GetMaximum();

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
          for(double length = 0; length < m_DiscRadius;length += 1)
            {
            index[0] = (long int)(it_input.GetIndex()[0] + length * cos(angle));
            index[1] = (long int)(it_input.GetIndex()[1] + length * sin(angle));
            if( postProcessImage->GetBufferedRegion().IsInside(index) )
              {
              postProcessImage->SetPixel(index,0);
              }
            } 
          }
        minMaxCalculator->SetImage(postProcessImage);
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

  os << "Threshold: " << m_Threshold << std::endl;
  os << "Angle Resolution: " << m_AngleResolution << std::endl;
  os << "Number Of Lines: " << m_NumberOfLines << std::endl;
  os << "Disc Radius: " << m_DiscRadius << std::endl;
  os << "Accumulator blur variance: " << m_Variance << std::endl;
  os << "Simplify Accumulator" << m_SimplifyAccumulator << std::endl;
}


} // end namespace

#endif
