/*=========================================================================

  Program:   itkUNC
  Module:    itkHoughTransform2DCirclesImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 CADDLab @ UNC. All rights reserved.
  See itkUNCCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHoughTransform2DCirclesImageFilter_txx
#define __itkHoughTransform2DCirclesImageFilter_txx

#include "itkHoughTransform2DCirclesImageFilter.h"
#include <itkImageRegionIteratorWithIndex.h>
#include "itkGaussianDerivativeImageFunction.h"



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
  m_MaximumRadius = 5; // by default
  m_SigmaGradient = 1; // Scale of the DoG filter
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

  typedef GaussianDerivativeImageFunction<InputImageType> DoGFunctionType;
  typename DoGFunctionType::Pointer DoGFunction = DoGFunctionType::New();
  DoGFunction->SetInputImage(m_InputImage.GetPointer());
  DoGFunction->SetSigma(m_SigmaGradient);

  m_RadiusImage = OutputImageType::New();

  m_RadiusImage->SetLargestPossibleRegion( region );
  m_RadiusImage->SetBufferedRegion( region );
  m_RadiusImage->SetRequestedRegion( region );
  
  m_RadiusImage->SetOrigin(m_InputImage->GetOrigin());
  m_RadiusImage->SetSpacing(m_InputImage->GetSpacing());
  
  m_RadiusImage->Allocate();

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
        
        double i = m_MinimumRadius;
        double distance;

        do{

            index[0] = (long int)(point[0]+i*Vx);
            index[1] = (long int)(point[1]+i*Vy);

            distance = sqrt( (index[1]-point[1])*(index[1]-point[1])
                             +(index[0]-point[0])*(index[0]-point[0])
                           );


            if((index[1]>0) &&(index[1]<size[1])  && (index[0]>0) &&(index[0]<size[0]))
            {
              m_OutputImage->SetPixel(index, m_OutputImage->GetPixel(index)+1);
              m_RadiusImage->SetPixel(index, (m_RadiusImage->GetPixel(index)+distance)/2);       
            }
          
            i=i+1;

          } while( (index[0]>0) && (index[0]<size[0]) && (index[1]>0) && (index[1]<size[1]) 
                    && (distance < m_MaximumRadius)
                  );
      }

    }
    ++image_it;
  }

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
}


} // end namespace

#endif
