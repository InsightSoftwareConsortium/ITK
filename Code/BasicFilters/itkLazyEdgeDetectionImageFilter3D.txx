/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLazyEdgeDetectionImageFilter3D.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLazyEdgeDetectionImageFilter3D_txx
#define _itkLazyEdgeDetectionImageFilter3D_txx

#include "itkLazyEdgeDetectionImageFilter3D.h"
#include "itkCannyEdgeDetectionImageFilter.h"
#include "itkImageRegion.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage >
LazyEdgeDetectionImageFilter3D<TInputImage,TOutputImage >
::LazyEdgeDetectionImageFilter3D()
{
  this->SetNumberOfRequiredInputs( 1 );

  edgeDetectorThreshold =0.03;
  edgeDetectorOutsideValue =0;
  edgeDetectorVariance =3;
  edgeDetectorMaximumError =0.01 ;
  m_Direction = 0;
}


/**
 * GenerateData Performs the reflection
 */
template <class TInputImage, class TOutputImage >
void
LazyEdgeDetectionImageFilter3D<TInputImage,TOutputImage>
::GenerateData( void )
{


  typename Superclass::InputImageConstPointer  inputPtr = this->GetInput();
  typedef Image<float, 2> floatImage2Dtype;
  typedef ExtractImageFilter<TInputImage, floatImage2Dtype> extractorType;
  typedef ImageRegionIterator<TOutputImage> outputIterType;
  typedef ImageRegionIterator<floatImage2Dtype> sliceIterType;
  typedef CannyEdgeDetectionImageFilter<floatImage2Dtype, floatImage2Dtype> edgeDetectorType;

  typename floatImage2Dtype::Pointer float2Dimage;
  typename extractorType::Pointer extractor=extractorType::New();
  typename extractorType::InputImageRegionType extractionRegion;
  typename extractorType::InputImageRegionType::SizeType extractionRegionSize;
  typename extractorType::InputImageRegionType::IndexType extractionRegionIndex;
  typename TOutputImage::Pointer outputImage=TOutputImage::New();
  typename InputImageRegionType::SizeType size;
  typename floatImage2Dtype::RegionType sliceRegion;
  typename floatImage2Dtype::SizeType sliceSize;
  typename edgeDetectorType::Pointer edgeDetector=edgeDetectorType::New();

  size=inputPtr->GetLargestPossibleRegion().GetSize();
  outputImage->SetRegions(inputPtr->GetLargestPossibleRegion());
  outputImage->SetOrigin(inputPtr->GetOrigin());
  outputImage->SetSpacing(inputPtr->GetSpacing());
  outputImage->Allocate();

  outputIterType outputIter(outputImage, outputImage->GetLargestPossibleRegion());
  outputIter.GoToBegin();

  sliceSize[0]=size[0];
  sliceSize[1]=size[1];
  sliceRegion.SetSize(sliceSize);

  extractionRegionSize[0]=size[0];
  extractionRegionSize[1]=size[1];
  extractionRegionSize[2]=0;

  extractionRegion.SetSize(extractionRegionSize);
  extractionRegionIndex[0]=extractionRegionIndex[1]=0;

  edgeDetector->SetThreshold(edgeDetectorThreshold);
  edgeDetector->SetOutsideValue(edgeDetectorOutsideValue);
  edgeDetector->SetVariance(edgeDetectorVariance);
  edgeDetector->SetMaximumError(edgeDetectorMaximumError);

  extractor->SetInput(inputPtr);
  edgeDetector->SetInput(extractor->GetOutput());


  for(int z=0; z<size[2]; z++)
  {
    extractionRegionIndex[2]=z;
    extractionRegion.SetIndex(extractionRegionIndex);
    extractor->SetExtractionRegion(extractionRegion);
    edgeDetector->Update();
    float2Dimage=edgeDetector->GetOutput();
    float2Dimage->SetRegions(sliceRegion);
    sliceIterType sliceIter(float2Dimage, float2Dimage->GetLargestPossibleRegion());
    sliceIter.GoToBegin();
    while(!sliceIter.IsAtEnd())
    {
      outputIter.Set(sliceIter.Value());
      ++outputIter;
      ++sliceIter;
    }
  }
this->GraftOutput(outputImage);


/*  #define YAW_ANGLE 0
  #define ROLL_ANGLE 1
  #define pi 3.1415926535897932384626433832795
  #define FINAL_WIDTH 64
  #define FINAL_HEIGHT 64
  #define edgeDetectorThreshold 0.03
  #define edgeDetectorOutsideValue 0
  #define edgeDetectorVariance 3
  #define edgeDetectorMaximumError 0.01
  
  typedef AffineTransform<double, TInputImage::ImageDimension> correctingTransformType;
  typedef ResampleImageFilter<TInputImage, TOutputImage> transformerType;
  typedef LinearInterpolateImageFunction<TInputImage, double> linearInterpolatorType;
  typedef ShrinkImageFilter<TInputImage, TInputImage> shrinkerType;
  typedef CannyEdgeDetectionImageFilter<TInputImage, TInputImage> edgeDetectorType;
    
  TInputImage::Pointer tmpImage= TInputImage::New();
  correctingTransformType::Pointer correctingTransform=correctingTransformType::New();
  transformerType::Pointer transformer=transformerType::New();
  shrinkerType::Pointer shrinker=shrinkerType::New();
  edgeDetectorType::Pointer edgeDetector=edgeDetectorType::New();
  correctingTransformType::OutputVectorType axes;
  linearInterpolatorType::Pointer linearInterpolator=linearInterpolatorType::New();
    typename Superclass::InputImageConstPointer  inputPtr = this->GetInput();
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput(0);
  float yawAngle=0, rollAngle=0, shift=0;
  unsigned int  shrinkFactors[TInputImage::ImageDimension];
  TInputImage::SizeType inputImageSize;
  const double *defaultSpacing;
  double *defaultOrigin;
  double *centeredOrigin;
  defaultSpacing=new double[TInputImage::ImageDimension];
  defaultOrigin=new double[TInputImage::ImageDimension];
  centeredOrigin=new double[TInputImage::ImageDimension];
  
  defaultSpacing=inputPtr->GetSpacing();
//  defaultOrigin=inputPtr->GetOrigin();
  defaultOrigin[0]=0;
  defaultOrigin[1]=0;
  defaultOrigin[2]=0;

  outputPtr->SetRegions(inputPtr->GetLargestPossibleRegion());
  outputPtr->SetSpacing(defaultSpacing);
  outputPtr->SetOrigin(defaultOrigin);
  outputPtr->Allocate();
  
  inputImageSize=inputPtr->GetLargestPossibleRegion().GetSize();
  shrinkFactors[0]=floor(inputImageSize[0]/FINAL_WIDTH);
  shrinkFactors[1]=floor(inputImageSize[0]/FINAL_HEIGHT);
  shrinkFactors[2]=1;
  centeredOrigin[0]=floor(inputImageSize[0]/shrinkFactors[0]/2);
  centeredOrigin[1]=floor(inputImageSize[1]/shrinkFactors[1]/2);
  centeredOrigin[2]=floor(inputImageSize[2]/shrinkFactors[2]/2);

  edgeDetector->SetThreshold(edgeDetectorThreshold);
  edgeDetector->SetOutsideValue(edgeDetectorOutsideValue);
  edgeDetector->SetVariance(edgeDetectorVariance);
  edgeDetector->SetMaximumError(edgeDetectorMaximumError);
  edgeDetector->SetInput(inputPtr);

  shrinker->SetShrinkFactors(shrinkFactors);
  shrinker->SetInput(edgeDetector->GetOutput());
//  shrinker->SetInput(inputPtr);
  shrinker->Update();
  tmpImage=shrinker->GetOutput();
//  tmpImage->SetOrigin(centeredOrigin);
  tmpImage->SetOrigin(defaultOrigin);
  tmpImage->SetSpacing(defaultSpacing);

  yawAngle=FindAngle((typename Superclass::InputImageConstPointer)tmpImage, YAW_ANGLE);
  transformer->SetInterpolator((InterpolateImageFunction<TInputImage, double>::Pointer) linearInterpolator);
  transformer->SetSize(tmpImage->GetLargestPossibleRegion().GetSize());
  transformer->SetOutputSpacing(tmpImage->GetSpacing());
  transformer->SetOutputOrigin(tmpImage->GetOrigin());

  axes[0]=1;
  axes[1]=0;
  axes[2]=0;
  correctingTransform->SetIdentity();
  correctingTransform->Rotate3D(axes, -pi*yawAngle/180);
  transformer->SetTransform((Transform<double,TInputImage::ImageDimension, TInputImage::ImageDimension>::Pointer) correctingTransform);
  transformer->SetInput(tmpImage);
  transformer->Update();
  tmpImage=transformer->GetOutput();

  rollAngle=FindAngle((typename Superclass::InputImageConstPointer)tmpImage, ROLL_ANGLE);
  transformer=transformerType::New();
  transformer->SetInterpolator((InterpolateImageFunction<TInputImage, double>::Pointer) linearInterpolator);
  transformer->SetSize(tmpImage->GetLargestPossibleRegion().GetSize());
  transformer->SetOutputSpacing(defaultSpacing);
  transformer->SetOutputOrigin(defaultOrigin);
  axes[0]=0;
  axes[1]=1;
  axes[2]=0;
  correctingTransform->Rotate3D(axes, -pi*rollAngle/180);
  transformer->SetTransform((Transform<double,TInputImage::ImageDimension, TInputImage::ImageDimension>::Pointer) correctingTransform);
  transformer->SetInput(tmpImage);
  transformer->Update();
  tmpImage=transformer->GetOutput();

  transformer=transformerType::New();
  transformer->SetInterpolator((InterpolateImageFunction<TInputImage, double>::Pointer)linearInterpolator);
  transformer->SetSize(inputPtr->GetLargestPossibleRegion().GetSize());
  transformer->SetOutputSpacing(defaultSpacing);
  transformer->SetOutputOrigin(defaultOrigin);
  axes[0]=shrinkFactors[0]*FindShift((typename Superclass::InputImageConstPointer)tmpImage);
  axes[1]=0;
  axes[2]=0;
  correctingTransform->Translate(axes);
  transformer->SetTransform((Transform<double,TInputImage::ImageDimension, TInputImage::ImageDimension>::Pointer) correctingTransform);
  transformer->SetInput(inputPtr);
  transformer->Update();
  this->GraftOutput(transformer->GetOutput());*/
}

template <class TInputImage, class TOutputImage >
void
LazyEdgeDetectionImageFilter3D<TInputImage,TOutputImage>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Direction: " << m_Direction << std::endl;
}



//} // end namespace itk


} // end namespace itk


#endif
