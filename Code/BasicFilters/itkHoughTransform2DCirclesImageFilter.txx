/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHoughTransform2DCirclesImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkHoughTransform2DCirclesImageFilter_txx
#define __itkHoughTransform2DCirclesImageFilter_txx

#include "itkHoughTransform2DCirclesImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkGaussianDerivativeImageFunction.h"
#include "itkMinimumMaximumImageCalculator.h"


#ifndef PI 
#define PI 3.1415926535897932384626433832795
#endif


namespace itk
{

template<typename TInputPixelType, typename TOutputPixelType>
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType>
::HoughTransform2DCirclesImageFilter()
{
  m_Threshold = 0;
  m_MinimumRadius = 0; // by default
  m_MaximumRadius = 10; // by default
  m_SigmaGradient = 1; // Scale of the DoG filter
  m_DiscRadiusRatio = 1;
  m_Variance = 10;
  m_OldModifiedTime = 0;
  m_OldNumberOfCircles = 0;
  m_SweepAngle = 0.0;
}

template<typename TInputPixelType, typename TOutputPixelType>
void 
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType>
::SetRadius(double radius)
{
  m_MinimumRadius = radius;
  m_MaximumRadius = radius;
}

template<typename TInputPixelType, typename TOutputPixelType>
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType>
::GenerateData()
{

  // Get the input and output pointers
  InputImageConstPointer  m_InputImage = this->GetInput(0);
  OutputImagePointer m_OutputImage = this->GetOutput(0);

  // Allocate the output
  typename InputImageType::RegionType region;
  Size<2> size;
  size[0]= m_InputImage->GetLargestPossibleRegion().GetSize()[0];
  size[1]= m_InputImage->GetLargestPossibleRegion().GetSize()[1];
  region.SetSize(size);
  region.SetIndex(m_InputImage->GetLargestPossibleRegion().GetIndex());
  m_OutputImage->SetLargestPossibleRegion( region );
  m_OutputImage->SetBufferedRegion( region );
  m_OutputImage->SetRequestedRegion( region );
  
  m_OutputImage->SetOrigin(m_InputImage->GetOrigin());
  m_OutputImage->SetSpacing(m_InputImage->GetSpacing());
  
  m_OutputImage->Allocate();
  m_OutputImage->FillBuffer(0);

  typedef GaussianDerivativeImageFunction<InputImageType> DoGFunctionType;
  typename DoGFunctionType::Pointer DoGFunction = DoGFunctionType::New();
  DoGFunction->SetInputImage(m_InputImage);
  DoGFunction->SetSigma(m_SigmaGradient);

  m_RadiusImage = OutputImageType::New();

  m_RadiusImage->SetLargestPossibleRegion( region );
  m_RadiusImage->SetBufferedRegion( region );
  m_RadiusImage->SetRequestedRegion( region );
  
  m_RadiusImage->SetOrigin(m_InputImage->GetOrigin());
  m_RadiusImage->SetSpacing(m_InputImage->GetSpacing());
  
  m_RadiusImage->Allocate();
  m_RadiusImage->FillBuffer(0);

  ImageRegionConstIteratorWithIndex< InputImageType >  image_it( m_InputImage,  m_InputImage->GetRequestedRegion() );
  image_it.Begin();

  Index<2> index;
  Point<float,2> point;

  while( !image_it.IsAtEnd() )
    {
    if(image_it.Get()>m_Threshold)
      {   
      point[0] = image_it.GetIndex()[0];
      point[1] = image_it.GetIndex()[1];
      typename DoGFunctionType::VectorType grad = DoGFunction->EvaluateAtIndex(image_it.GetIndex());

      double Vx = grad[0];
      double Vy = grad[1];

      if( (fabs(Vx)>1) || (fabs(Vy)>1) ) // if the gradient is not flat
        {
        double norm = sqrt(Vx*Vx+Vy*Vy);
        Vx /= norm;
        Vy /= norm;
        
        for(double angle = -m_SweepAngle;angle<=m_SweepAngle;angle+=0.05)
          {
        double i = m_MinimumRadius;
        double distance;

        do{


          
        index[0] = (long int)(point[0]-i*(Vx*cos(angle)+Vy*sin(angle)));
        index[1] = (long int)(point[1]-i*(Vx*sin(angle)+Vy*cos(angle)));

        distance = sqrt( (index[1]-point[1])*(index[1]-point[1])
                         +(index[0]-point[0])*(index[0]-point[0])
          );


        if((index[1]>0) &&(index[1]<static_cast<long
                           int>(size[1]))  && (index[0]>0)
           &&(index[0]<static_cast<long int>(size[0])))
          {
          m_OutputImage->SetPixel(index, m_OutputImage->GetPixel(index)+1);
          m_RadiusImage->SetPixel(index, (m_RadiusImage->GetPixel(index)+distance)/2);       
          }
          
        i=i+1;

        } while( (index[0]>0) &&
                 (index[0]<static_cast<typename Index<2>::IndexValueType>(size[0])) &&
                 (index[1]>0) &&
                 (index[1]<static_cast<typename Index<2>::IndexValueType>(size[1])) 
                 && (distance < m_MaximumRadius)
         );
        }
       
        }

      }
    ++image_it;
    }

}


/** Get the list of circles. This recomputes the circles */
template<typename TInputPixelType, typename TOutputPixelType>
typename HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType>::CirclesListType & 
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType>
::GetCircles(unsigned int n)
{
  if((this->GetMTime() == m_OldModifiedTime) && (n == m_OldNumberOfCircles)) // if the filter has not been updated
    {
    return m_CirclesList;
    }

  m_CirclesList.clear();

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


  typedef DiscreteGaussianImageFilter<OutputImageType,InternalImageType> GaussianFilterType;
  typename GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();

  gaussianFilter->SetInput(outputImage); // the output is the accumulator image
  double variance[2];
  variance[0] = m_Variance;
  variance[1] = m_Variance;
  gaussianFilter->SetVariance(variance);
  gaussianFilter->Update();
  typename InternalImageType::Pointer m_PostProcessImage = gaussianFilter->GetOutput();

  typedef MinimumMaximumImageCalculator<InternalImageType> MinMaxCalculatorType;
  typename MinMaxCalculatorType::Pointer minMaxCalculator = MinMaxCalculatorType::New();
  ImageRegionIterator<InternalImageType> it_input(m_PostProcessImage,m_PostProcessImage->GetLargestPossibleRegion());

  Index<2> m_index;

  unsigned int circles=0;
  bool found;

  // Find maxima
  do
    {
    minMaxCalculator->SetImage(m_PostProcessImage);
    minMaxCalculator->ComputeMaximum();
    InternalImageType::PixelType  max = minMaxCalculator->GetMaximum();
    
    found = false;
    for(it_input.GoToBegin();!it_input.IsAtEnd();++it_input)
      {
      if(it_input.Get() == max) 
        { 
        // Create a Line Spatial Object
        CirclePointer Circle = CircleType::New();
        Circle->SetId(circles);
        Circle->SetRadius(m_RadiusImage->GetPixel(it_input.GetIndex()));

        CircleType::VectorType center;
        center[0] = it_input.GetIndex()[0];
        center[1] = it_input.GetIndex()[1];
        Circle->GetObjectToParentTransform()->SetOffset(center);
        Circle->ComputeBoundingBox();

        m_CirclesList.push_back(Circle);
       
        // Remove a black disc from the hough space domain
        for(double angle = 0; angle <= 2*PI ; angle += PI/1000)
          {     
          for(double lenght = 0; lenght < m_DiscRadiusRatio*Circle->GetRadius()[0];lenght += 1)
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
      
        circles++;
        found = true;
        if(circles == m_NumberOfCircles) break;  
        }
      }
    } while((circles<m_NumberOfCircles) && (found));
  
  m_OldModifiedTime = this->GetMTime();
  m_OldNumberOfCircles = m_CirclesList.size();
  return m_CirclesList;
}


/** Print Self information */
template<typename TInputPixelType, typename TOutputPixelType>
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  std::cout << "Threshold: "
            << m_Threshold << std::endl;

  std::cout << "Minimum Radius:  "
            << m_MinimumRadius << std::endl;

  std::cout << "Maximum Radius: "
            << m_MaximumRadius << std::endl;

  std::cout << "Derivative Scale : "
            << m_SigmaGradient << std::endl;

  std::cout << "Radius Image Information : " 
            << m_RadiusImage << std::endl;

  std::cout << "Number Of Circles: " 
            << m_NumberOfCircles << std::endl;

  std::cout << "Disc Radius: "
            << m_DiscRadiusRatio << std::endl;

  std::cout << "Accumulator blur variance: "
            << m_Variance << std::endl;
  
  std::cout << "Sweep angle : "
             << m_SweepAngle << std::endl;
  
  }
} // end namespace

#endif
