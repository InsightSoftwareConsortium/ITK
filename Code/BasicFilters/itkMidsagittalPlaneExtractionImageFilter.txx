/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMidsagittalPlaneExtractionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMidsagittalPlaneExtractionImageFilter_txx
#define _itkMidsagittalPlaneExtractionImageFilter_txx


#include "itkMidsagittalPlaneExtractionImageFilter.h"
#include "itkImage.h"
#include "itkGetAverageSliceImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkThresholdImageFilter.h"
#include "itkCannyEdgeDetectionImageFilter.h"
#include "itkFlipImageFilter.h"
#include "itkResampleImageFilter.h"
#include "itkAffineTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkNormalizedCorrelationImageToImageMetric.h"
#include "MSP_utils.h"
#include "itkLazyEdgeDetectionImageFiilter3D.h"

namespace itk
{

/**
 * Constructor
 */

 template <class TInputImage, class TOutputImage >
MidsagittalPlaneExtractionImageFilter<TInputImage,TOutputImage >
::MidsagittalPlaneExtractionImageFilter()
{
  this->SetNumberOfRequiredInputs( 1 );
  m_Direction = 0;
}


/**
 * GenerateData Performs the reflection
 */
template <class TInputImage, class TOutputImage >
void
MidsagittalPlaneExtractionImageFilter<TInputImage,TOutputImage>
::GenerateData( void )
{
  #define YAW_ANGLE 0
  #define ROLL_ANGLE 1
  #define pi 3.1415926535897932384626433832795
  #define FINAL_WIDTH 64
  #define FINAL_HEIGHT 64
  #define edgeDetectorThreshold 0.03
  #define edgeDetectorOutsideValue 0
  #define edgeDetectorVariance 3
  #define edgeDetectorMaximumError 0.01
  
  typedef itk::AffineTransform<double, TInputImage::ImageDimension> correctingTransformType;
  typedef itk::ResampleImageFilter<TInputImage, TOutputImage> transformerType;
  typedef itk::LinearInterpolateImageFunction<TInputImage, double> linearInterpolatorType;
  typedef itk::ShrinkImageFilter<TInputImage, TInputImage> shrinkerType;
  typedef itk::CannyEdgeDetectionImageFilter<TInputImage, TInputImage> edgeDetectorType;
    typedef itk::LazyEdgeDetectionImageFilter3D<TInputImage, TInputImage> lazyEdgeDetector3DType; 
  typedef itk::Image<double, TInputImage::ImageDimension> averagedImageType;
  typedef itk::GetAverageSliceImageFilter<TInputImage, averagedImageType> averagerType;

  itk::Point<double, TInputImage::ImageDimension> centerOfRotation, transformedPoint;
  TInputImage::Pointer tmpImage= TInputImage::New();
  TInputImage::Pointer zeroYawImage= TInputImage::New();
  TInputImage::Pointer zeroRollImage= TInputImage::New();
  correctingTransformType::Pointer correctingTransform=correctingTransformType::New();
  correctingTransformType::Pointer affineTransform=correctingTransformType::New();
  transformerType::Pointer transformer=transformerType::New();
  shrinkerType::Pointer shrinker=shrinkerType::New();
  edgeDetectorType::Pointer edgeDetector=edgeDetectorType::New();
  correctingTransformType::OutputVectorType axes, translations;
  linearInterpolatorType::Pointer linearInterpolator=linearInterpolatorType::New();
    typename Superclass::InputImageConstPointer  inputPtr = this->GetInput();
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput(0);
  float yawAngle=0, rollAngle=0, shift=0;
  unsigned int  shrinkFactors[TInputImage::ImageDimension];
  TInputImage::SizeType inputImageSize;
  const double *defaultSpacing;
  double *defaultOrigin;
  double *centeredOrigin;
  double *yShifts;
  defaultSpacing=new double[TInputImage::ImageDimension];
  defaultOrigin=new double[TInputImage::ImageDimension];
  centeredOrigin=new double[TInputImage::ImageDimension];
  lazyEdgeDetector3DType::Pointer lazyEdgeDetector3D=lazyEdgeDetector3DType::New();
  averagerType::Pointer averager=averagerType::New();
  estimateType *yawEstimate=NULL, *rollEstimate=NULL;
  averagedImageType::Pointer averagedImage;
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
//  edgeDetector->SetVariance(edgeDetectorVariance);
//  edgeDetector->SetMaximumError(edgeDetectorMaximumError);
//  edgeDetector->SetInput(inputPtr);
//  averager->SetInput(inputPtr);
//  averager->Update();
//  exit(3);

//  lazyEdgeDetector3D->SetInput(inputPtr);
//  averager->SetInput(lazyEdgeDetector3D->GetOutput());

  shrinker->SetShrinkFactors(shrinkFactors);
//  shrinker->SetInput(lazyEdgeDetector3D->GetOutput());
//  shrinker->SetInput(edgeDetector->GetOutput());
  shrinker->SetInput(inputPtr);
  lazyEdgeDetector3D->SetInput(shrinker->GetOutput());

//  shrinker->Update();
  affineTransform->SetIdentity();
  averager->SetAveragedOutDimension(2);
//  averager->SetInput(shrinker->GetOutput());
  averager->SetInput(lazyEdgeDetector3D->GetOutput());
  averager->Update();
  tmpImage=lazyEdgeDetector3D->GetOutput();
//  lazyEdgeDetector3D=NULL;
  averagedImage=averager->GetOutput();
  averagedImage->SetSpacing(defaultSpacing);
  averagedImage->SetPhysicalToIndexTransform(affineTransform);
  averagedImage->SetIndexToPhysicalTransform(affineTransform);
//  edgeDetector->SetInput(averager->GetOutput());
//  edgeDetector->Update();
//  printVolumeTo(tmpImage, 2, "c:\\tmp", "ax");
//  exit(1);
//  tmpImage->SetOrigin(centeredOrigin);
//  tmpImage->SetOrigin(defaultOrigin);
//  tmpImage->SetSpacing(defaultSpacing);
//  printVolumeTo(averager->GetOutput(), 2, "c:\\tmp", "aaver0");


  yawEstimate=estimateAngle((averagedImageType::ConstPointer)averagedImage);
/*  estimate=new estimateType;
  estimate->shiftsY=new double[130];
  for(int i=0; i<=129; i++)
  {
    estimate->shiftsY[i]=-6;
  }
  estimate->shiftX=6;
  estimate->angleEstimate=-20;
  estimate->n=120;
  estimate->center=0;
  estimate->stepY=3;
  estimate->flippedCenterOfMass=new double[3];
  estimate->flippedCenterOfMass[0]=30.906;
  estimate->flippedCenterOfMass[1]=24.2944;
  estimate->flippedCenterOfMass[2]=0; */

  yawAngle=yawEstimate->angleEstimate/2;

  tmpImage->SetOrigin(defaultOrigin);
  tmpImage->SetSpacing(defaultSpacing);
  axes[0]=0;
  axes[1]=0;
  axes[2]=1;
  correctingTransform->SetIdentity();
  correctingTransform->Rotate3D(axes, -pi*yawAngle/180);
  for(int i=0; i<TInputImage::ImageDimension; i++)
  {
    translations[i]=0;
    centerOfRotation[i]=(yawEstimate->flippedCenterOfMass[i]);
  }
  centerOfRotation[0]=inputImageSize[0]/shrinkFactors[0]-centerOfRotation[0];
  transformedPoint=correctingTransform->TransformPoint(centerOfRotation);
  for(int i=0; i<TInputImage::ImageDimension; i++)
  {
    translations[i]=centerOfRotation[i]-transformedPoint[i];
  }
  correctingTransform->Translate(translations, 1);
  transformer->SetTransform((itk::Transform<double,TInputImage::ImageDimension, TInputImage::ImageDimension>::Pointer) correctingTransform);
  transformer->SetInput(tmpImage);
  transformer->SetOutputOrigin(defaultOrigin);
  transformer->SetOutputSpacing(defaultSpacing);
  transformer->SetSize(tmpImage->GetLargestPossibleRegion().GetSize());
  transformer->Update();
  zeroYawImage=transformer->GetOutput();
//  printVolumeTo(zeroYawImage, 0, "c:\\tmp", "zeroYaw");
  
//  yawAngle=FindAngle((typename Superclass::InputImageConstPointer)tmpImage, YAW_ANGLE, 90, 1, estimate);
/*  delete estimate->shiftsY;
  delete estimate;
  transformer->SetInterpolator((itk::InterpolateImageFunction<TInputImage, double>::Pointer) linearInterpolator);
  transformer->SetSize(tmpImage->GetLargestPossibleRegion().GetSize());
  transformer->SetOutputSpacing(tmpImage->GetSpacing());
  transformer->SetOutputOrigin(tmpImage->GetOrigin());


*/
   averager->SetAveragedOutDimension(1);
  averager->Update(); 
  averagedImage=averager->GetOutput();
  averagedImage->SetSpacing(defaultSpacing);
  averagedImage->SetPhysicalToIndexTransform(affineTransform);
  averagedImage->SetIndexToPhysicalTransform(affineTransform);

/*  estimate=new estimateType;
  estimate->shiftsY=new double[130];
  for(int i=0; i<=129; i++)
  {
    estimate->shiftsY[i]=0;
  }
  estimate->shiftX=-6;
  estimate->angleEstimate=-3;
  estimate->n=120;
  estimate->center=0;
  estimate->stepY=3;
  estimate->flippedCenterOfMass=new double[3];
  estimate->flippedCenterOfMass[0]=30.906;
  estimate->flippedCenterOfMass[1]=24.2944;
  estimate->flippedCenterOfMass[2]=64.22322; */

  rollEstimate=estimateAngle((averagedImageType::ConstPointer)averagedImage);
  rollAngle=rollEstimate->angleEstimate/2;
  rollEstimate->flippedCenterOfMass[1]=yawEstimate->flippedCenterOfMass[1];
//  rollAngle=FindAngle((typename Superclass::InputImageConstPointer)tmpImage, ROLL_ANGLE, 90, 1, estimate);

//  rollAngle=FindAngle((typename Superclass::InputImageConstPointer)tmpImage, ROLL_ANGLE);
  transformer=transformerType::New();
  axes[0]=0;
  axes[1]=0;
  axes[2]=1;
  correctingTransform->SetIdentity();
  correctingTransform->Rotate3D(axes, -pi*yawAngle/180);
  axes[0]=0;
  axes[1]=1;
  axes[2]=0;
  correctingTransform->Rotate3D(axes, -pi*rollAngle/180);
  for(int i=0; i<TInputImage::ImageDimension; i++)
  {
    translations[i]=0;
    centerOfRotation[i]=(rollEstimate->flippedCenterOfMass[i])*shrinkFactors[i];
  }
  centerOfRotation[0]=inputImageSize[0]-centerOfRotation[0];
  transformedPoint=correctingTransform->TransformPoint(centerOfRotation);
  for(int i=0; i<TInputImage::ImageDimension; i++)
  {
    translations[i]=centerOfRotation[i]-transformedPoint[i];
  }
  correctingTransform->Translate(translations, 1);
  transformer->SetTransform((itk::Transform<double,TInputImage::ImageDimension, TInputImage::ImageDimension>::Pointer) correctingTransform);
  transformer->SetInput(inputPtr);
  transformer->SetOutputOrigin(defaultOrigin);
  transformer->SetOutputSpacing(defaultSpacing);
  transformer->SetSize(inputImageSize);
  transformer->Update();
  zeroRollImage=transformer->GetOutput();
//  printVolumeTo(zeroRollImage, 0, "c:\\tmp", "zeroYaw");
//  printVolumeTo(zeroRollImage, 1, "c:\\tmp", "zeroRoll");

    lazyEdgeDetector3D=lazyEdgeDetector3DType::New();

  averager->SetAveragedOutDimension(2);
  lazyEdgeDetector3D->SetInput(zeroRollImage);
  averager->SetInput(lazyEdgeDetector3D->GetOutput());
  averager->Update();
  averager->GetOutput()->SetOrigin(defaultOrigin);
//  printVolumeTo(averager->GetOutput(), 0, "c:\\tmp", "averageZeroRoll");

  shift=estimateShift(averager->GetOutput());
//  shift = 0.36;

  transformer=transformerType::New();
  transformer->SetInterpolator((itk::InterpolateImageFunction<TInputImage, double>::Pointer)linearInterpolator);
  transformer->SetSize(inputPtr->GetLargestPossibleRegion().GetSize());
  transformer->SetOutputSpacing(defaultSpacing);
  transformer->SetOutputOrigin(defaultOrigin);
  transformer->SetInput(inputPtr);
  transformer->SetSize(inputImageSize);
  for(int i=0; i<TInputImage::ImageDimension; i++)
  {
    translations[i]=0;
  }
  translations[0]=-shift/2;
  correctingTransform->Translate(translations, 1);
  transformer->SetTransform((itk::Transform<double,TInputImage::ImageDimension, TInputImage::ImageDimension>::Pointer) correctingTransform);
  transformer->Update();
  printVolumeTo(transformer->GetOutput(), 0, "c:\\tmp", "finalYaw");
  printVolumeTo(transformer->GetOutput(), 1, "c:\\tmp", "finalRoll");

  exit(0);

  averager->SetInput(tmpImage);
  averager->SetAveragedOutDimension(0);
  averager->Update();
  shift=estimateShift(averager->GetOutput());
  shift=FindShift((typename Superclass::InputImageConstPointer)tmpImage, 90, 1, shift);
//  axes[0]=shrinkFactors[0]*FindShift((typename Superclass::InputImageConstPointer)tmpImage);
  axes[0]=0;
  axes[1]=0;
  axes[2]=0;
  correctingTransform->Translate(axes);
  transformer->SetTransform((itk::Transform<double,TInputImage::ImageDimension, TInputImage::ImageDimension>::Pointer) correctingTransform);
  transformer->SetInput(inputPtr);
  transformer->Update();
  this->GraftOutput(transformer->GetOutput());
}

template <class TInputImage, class TOutputImage >
void
MidsagittalPlaneExtractionImageFilter<TInputImage,TOutputImage>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Direction: " << m_Direction << std::endl;
}

template <class TInputImage, class TOutputImage>
double
MidsagittalPlaneExtractionImageFilter<TInputImage, TOutputImage>::
FindAngle(typename Superclass::InputImageConstPointer image, int angleChoice, int numberOfPoints, double step, estimateType* estimate)
{
  typedef itk::FlipImageFilter<TInputImage> flipperType;
  typedef itk::AffineTransform<double, TInputImage::ImageDimension> rotationTransformType;
  typedef itk::NormalizedCorrelationImageToImageMetric<TInputImage, TInputImage> correlatorType;
  typedef itk::LinearInterpolateImageFunction<TInputImage, double> linearInterpolatorType;
  typedef itk::ResampleImageFilter<TInputImage, TInputImage> transformerType;
  int junk;

  linearInterpolatorType::Pointer linearInterpolator=linearInterpolatorType::New();  
  TInputImage::Pointer flippedImage=TInputImage::New();
  flipperType::FlipAxesArrayType flipAxes;
  flipperType::Pointer flipper=flipperType::New();
  rotationTransformType::Pointer rotationTransform=rotationTransformType::New();
  correlatorType::Pointer correlator=correlatorType::New();
  rotationTransformType::OutputVectorType rotationAxes, translations;
  transformerType::Pointer transformer=transformerType::New();
  itk::Point <double , TInputImage::ImageDimension> aPoint, transformedPoint;  
  TInputImage::RegionType::SizeType flippedSize;
  correlatorType::MeasureType currentScore=0, maxScore=0;
  double correctAngle=0;
  double index, yShift;

//  translations[0]=-32;
//  translations[1]=-32;
//  translations[2]=-5;
  rotationTransform->SetIdentity();
  flipAxes[0]=flipAxes[1]=flipAxes[2]=0;
  rotationAxes[0]=rotationAxes[1]=rotationAxes[2]=0;
  if(angleChoice!=0 && angleChoice!=1)
  {
    fprintf(stderr, "\nFindAngle(...): Illegal Angle choice! Quitting\n");
    exit(111);
  }

  flipAxes[angleChoice]=1;
  if(angleChoice==0) rotationAxes[2]=1;
  else rotationAxes[1]=1;
  flipper->SetFlipAxes(flipAxes);
  flipper->SetInput(image);
  flipper->Update();
  flippedImage=flipper->GetOutput();
  flippedImage->SetSpacing(image->GetSpacing());
  flippedImage->SetOrigin(image->GetOrigin());
  rotationTransform->SetIdentity();
  flippedImage->SetIndexToPhysicalTransform(rotationTransform);
  flippedImage->SetPhysicalToIndexTransform(rotationTransform);
  linearInterpolator->SetInputImage(flippedImage);  
  correlator->SetInterpolator(linearInterpolator);
  correlator->SetFixedImage(image);
//  correlator->SetFixedImage(flippedImage);
  correlator->SetMovingImage(flippedImage);
//  correlator->SetMovingImage(image);
//  correlator->SetFixedImageRegion(image->GetLargestPossibleRegion());
  correlator->SetFixedImageRegion(image->GetLargestPossibleRegion());
//  correlator->SetScaleGradient(1.0);
//  correlator->SetDebug(1);
//  correlator->SetGlobalWarningDisplay(1); 
//  printVolumeTo(flippedImage, "c:\\tmp", "lazy_flipped");
  flippedSize=flippedImage->GetLargestPossibleRegion().GetSize();
//  aPoint[0]=ceil(flippedSize[0]/2);
//  aPoint[1]=ceil(flippedSize[1]/2);
//  aPoint[2]=ceil(flippedSize[2]/2);
  aPoint=estimate->flippedCenterOfMass;
  transformer->SetInterpolator((itk::InterpolateImageFunction<TInputImage, double>::Pointer) linearInterpolator);
  transformer->SetSize(flippedImage->GetLargestPossibleRegion().GetSize());
  transformer->SetOutputSpacing(flippedImage->GetSpacing());
  transformer->SetOutputOrigin(flippedImage->GetOrigin());
  for (float angle=estimate->angleEstimate-floor(numberOfPoints/2)*step; angle<=estimate->angleEstimate+floor(numberOfPoints/2)*step; angle+=step)
  {
    rotationTransform->SetIdentity();
    rotationTransform->Rotate3D(rotationAxes, pi*angle/180);
    transformedPoint=rotationTransform->TransformPoint(aPoint);
    translations[0]=-transformedPoint[0]+aPoint[0]+estimate->shiftX;
    index=(angle-estimate->angleEstimate+floor(estimate->n/2)*estimate->stepY)/estimate->stepY;
    if(index>=estimate->n) 
    {
      fprintf(stderr, "FinfMSPImageFilter::FindAngle():\nEstimate is inconsistent. Quitting\n");
      exit(6);
    }
    yShift=(estimate->shiftsY[(int)floor(index)]+estimate->shiftsY[(int)ceil(index)])/2;
    translations[1]=-transformedPoint[1]+aPoint[1]+yShift;
    translations[2]=-transformedPoint[2]+aPoint[2];
    rotationTransform->Translate(translations, 1);
    correlator->SetTransform(rotationTransform);
    correlator->SetTransformParameters(rotationTransform->GetParameters());
    currentScore=correlator->GetValue(rotationTransform->GetParameters());
    cout<<endl<<"For angle "<<angle<<" correlation is "<<currentScore<<endl;
    if(abs(currentScore)>maxScore)
    {
      maxScore=abs(currentScore);
      correctAngle=angle/2;
    }
//    transformer->SetTransform((itk::Transform<double,TInputImage::ImageDimension, TInputImage::ImageDimension>::Pointer) rotationTransform);
//    transformer->SetInput(flippedImage);
//    transformer->Update();
//    printVolumeTo(transformer->GetOutput(), "c:\\tmp", "transformed_flipped");
  }

//  scanf("%d", &junk);
  return correctAngle;
}

//} // end namespace itk

template <class TInputImage, class TOutputImage>
double
MidsagittalPlaneExtractionImageFilter<TInputImage, TOutputImage>::
FindShift(typename Superclass::InputImageConstPointer volume, double numberOfPoints, double step, double shiftEstimate)
{
  typedef itk::FlipImageFilter<TInputImage> flipperType;
  typedef itk::LinearInterpolateImageFunction<TInputImage, double> linearInterpolatorType;
  typedef itk::AffineTransform<double, TInputImage::ImageDimension> affineTransformType;
  typedef itk::NormalizedCorrelationImageToImageMetric<TInputImage, TInputImage> correlatorType;

  flipperType::Pointer flipper=flipperType::New();
  correlatorType::Pointer correlator=correlatorType::New();
  affineTransformType::Pointer affineTransform=affineTransformType::New();
  linearInterpolatorType::Pointer linearInterpolator=linearInterpolatorType::New();
  TInputImage::Pointer flippedImage;
  flipperType::FlipAxesArrayType flipAxes;
  affineTransformType::OutputVectorType rotationAxes, translations, verticalShift, horizontalShift;
  TInputImage::RegionType::SizeType flippedSize, inputSize;
  correlatorType::MeasureType currentScore=10, bestScore=10;
  TInputImage::RegionType::IndexType dimensionsOfInterest;
  itk::Point <double , TInputImage::ImageDimension> aPoint, transformedPoint, *centerOfMass=NULL, flippedCenterOfMass;  
  double middleValue=0;
  double shiftX, bestShiftX=0, shift;
  double theOrigin[TInputImage::ImageDimension];
  int j=0;

  inputSize=volume->GetLargestPossibleRegion().GetSize();
  for(int i=0; i<TInputImage::ImageDimension; i++)
  {
    flipAxes[i]=0;
    translations[i]=0;
    horizontalShift[i]=0;
    theOrigin[i]=0;
  }

  flipAxes[0]=1;
  middleValue=shiftEstimate;

  flipper->SetFlipAxes(flipAxes);
  flipper->SetInput(volume);
  flipper->Update();
  flippedImage=flipper->GetOutput();
  flippedImage->SetOrigin(theOrigin);

  linearInterpolator->SetInputImage(flippedImage);
  correlator->SetInterpolator(linearInterpolator);
  correlator->SetFixedImage(volume);
  correlator->SetMovingImage(flippedImage);
  affineTransform->SetIdentity();
  
  translations[0]=middleValue-floor(numberOfPoints/2)*step;
  affineTransform->Translate(translations);
  translations[0]=step;
  for(shiftX=middleValue-floor(numberOfPoints/2)*step; shiftX<=middleValue-floor(numberOfPoints/2)*step; shiftX+=step)
  {
    correlator->SetTransform(affineTransform);
    currentScore=correlator->GetValue(affineTransform->GetParameters());
    if(currentScore<bestScore)
    {
      bestScore=currentScore;
      shift=middleValue-floor(numberOfPoints/2)*step+j*step;
    }
    j++;
    affineTransform->Translate(translations);
  }


  return shift;
}


template <class TInputImage, class TOutputImage>
double
MidsagittalPlaneExtractionImageFilter<TInputImage, TOutputImage>::
findHighestPoint(typename Superclass::InputImageConstPointer volume, int dimensionChoice, double startingHeight)
{
  typedef TInputImage::RegionType requestedRegionType;
  typedef itk::ImageRegionConstIterator<TInputImage> inputIterType;

  requestedRegionType::SizeType requestedSize;
  requestedRegionType::IndexType requestedIndex;
  requestedRegionType requestedRegion;
  TInputImage::RegionType::SizeType inputSize;
  double nOfDimensions, upperBound, lowerBound, currentHeight, cutoff=0.1, sum=0;
  int found=0;

  if(dimensionChoice<0 || dimensionChoice>=TInputImage::ImageDimension) 
  {
    fprintf(stderr, "\nMidsagittalPlaneExtractionImageFilter::findHighestPoint: wrong dimension choice\n");
    exit(4);
  }

  nOfDimensions=TInputImage::ImageDimension;
  inputSize=volume->GetLargestPossibleRegion().GetSize();
  upperBound=inputSize[dimensionChoice];
  lowerBound=0;
  for(int i=0; i<nOfDimensions; i++)
  {
    if(i!=dimensionChoice)
    {
      requestedSize[i]=inputSize[i];
    }
    else
    {
      requestedSize[i]=1;
    }
    requestedIndex[i]=0;
  }

  requestedRegion.SetSize(requestedSize);

  if(startingHeight<0)
  {
    currentHeight=floor(inputSize[dimensionChoice]/2);
  }
  else
  {
    currentHeight=startingHeight;
  }

  while(upperBound-lowerBound>1)
  {
    sum=0;
    requestedIndex[dimensionChoice]=currentHeight;
    requestedRegion.SetIndex(requestedIndex);
    found=0;
    inputIterType inputIter(volume, requestedRegion);
    inputIter.GoToBegin();
    while(!inputIter.IsAtEnd() && found==0)
    {
//      requestedIndex=inputIter.GetIndex();
//      sum+=inputIter.Get();
//      if(sum>=cutoff)
      if(inputIter.Get()>=cutoff)
      {
        found=1;
      }
//      cout<<endl<<inputIter.Get()<<endl;
      ++inputIter;
    }

    if(found==1)
    {
      lowerBound=currentHeight;
    }
    else
    {
      upperBound=currentHeight;
    }
    currentHeight=floor((upperBound+lowerBound)/2);
  }


  requestedIndex[dimensionChoice]=upperBound;
  requestedRegion.SetIndex(requestedIndex);
  found=0;
  inputIterType inputIter(volume, requestedRegion);
  inputIter.GoToBegin();
  sum=0;
  while(!inputIter.IsAtEnd() && found==0)
  {
//    sum+=inputIter.Get();
//    if(sum>=cutoff)
    if(inputIter.Get()>=cutoff)
    {
      found=1;
    }
    ++inputIter;
  }

  if(found==1)
  {
    currentHeight=upperBound;
  }
  else
  {
    requestedIndex[dimensionChoice]=lowerBound;
    requestedRegion.SetIndex(requestedIndex);
    found=0;
    inputIterType inputIter(volume, requestedRegion);
    inputIter.GoToBegin();
    sum=0;
    while(!inputIter.IsAtEnd() && found==0)
    {
//      sum+=inputIter.Get();
//      if(sum>=cutoff)
      if(inputIter.Get()>=cutoff)
      {
        found=1;
      }
      ++inputIter;
    }
  }

  if(found==1)
  {
    currentHeight=lowerBound;
  }
  else
  {
    currentHeight=-1;
  }

  return currentHeight;
}

template <class TInputImage, class TOutputImage>
//itk::Point<double, TInputImage::ImageDimension>*
double*
MidsagittalPlaneExtractionImageFilter<TInputImage, TOutputImage>::
//findCentroid(typename Superclass::InputImageConstPointer volume, TInputImage::RegionType::IndexType dimensionMask)
findCentroid(typename Superclass::InputImageConstPointer volume, double* dimensionMask)
{
  typedef itk::ImageRegionConstIterator<TInputImage> inputIterType;
  
  double* centroid;
  inputIterType::IndexType index;
  double *dimensionsOfInterest, nOfDimensionsOfInterest=0, nOfBrightPixels=0;
  double weight=0, totalWeight=0;
  int j=0;

  centroid=new double[TInputImage::ImageDimension];
  inputIterType inputIter(volume, volume->GetLargestPossibleRegion());
  
  inputIter.GoToBegin();
  for(int i=0; i<TInputImage::ImageDimension; i++)
  {
    centroid[i]=0;
    if(dimensionMask[i]==0)
    {
      nOfDimensionsOfInterest++;
    }
  }

  dimensionsOfInterest=new double[nOfDimensionsOfInterest];
  for(int i=0; i<TInputImage::ImageDimension; i++)
  {
    if(dimensionMask[i]==0)
    {
      dimensionsOfInterest[j]=i;
      j++;
    }
  }

  while(!inputIter.IsAtEnd())
  {
    if(inputIter.Get()!=0)
    {
      index=inputIter.GetIndex();
      weight=inputIter.Get();
      totalWeight+=weight;
      for(int i=0; i<nOfDimensionsOfInterest; i++)
      {
        centroid[(int)dimensionsOfInterest[i]]+=index[(int)dimensionsOfInterest[i]]*weight;
      }
    }
    ++inputIter;
  }

  for(int i=0; i<nOfDimensionsOfInterest; i++)
  {
    centroid[(int)dimensionsOfInterest[i]]=centroid[(int)dimensionsOfInterest[i]]/totalWeight;
  }

  return centroid;  
} 


template <class TInputImage, class TOutputImage>
estimateType*
MidsagittalPlaneExtractionImageFilter<TInputImage, TOutputImage>::
estimateAngle(typename Superclass::InputImageConstPointer volume)
{
  typedef itk::FlipImageFilter<TInputImage> flipperType;
  typedef itk::ResampleImageFilter<TInputImage, TInputImage> transformerType;
  typedef itk::LinearInterpolateImageFunction<TInputImage, double> linearInterpolatorType;
  typedef itk::AffineTransform<double, TInputImage::ImageDimension> affineTransformType;
  typedef itk::NormalizedCorrelationImageToImageMetric<TInputImage, TInputImage> correlatorType;
  typedef itk::ThresholdImageFilter<TInputImage> thresholderType;

  thresholderType::Pointer thresholder=thresholderType::New();
  flipperType::Pointer flipper=flipperType::New();
  correlatorType::Pointer correlator=correlatorType::New();
  correlatorType::Pointer backupCorrelator=correlatorType::New();
  affineTransformType::Pointer affineTransform=affineTransformType::New();
  affineTransformType::Pointer identityTransform=affineTransformType::New();
  affineTransformType::Pointer savedAffineTransform=affineTransformType::New();
  affineTransformType::Pointer shifterAffineTransform=affineTransformType::New();
  transformerType::Pointer transformer=transformerType::New();
  transformerType::Pointer transformerShifter=transformerType::New();
  linearInterpolatorType::Pointer linearInterpolator=linearInterpolatorType::New();
  linearInterpolatorType::Pointer backupLinearInterpolator=linearInterpolatorType::New();
  TInputImage::Pointer flippedImage, originalImage;
  TInputImage::Pointer transformedImage;
  flipperType::FlipAxesArrayType flipAxes;
  affineTransformType::OutputVectorType rotationAxes, translations, verticalShift, horizontalShift;
  itk::Point <double , TInputImage::ImageDimension> aPoint, transformedPoint, flippedCenterOfMass;  
  affineTransformType::ParametersType params;
  double  *centerOfMass=NULL;
  TInputImage::RegionType::SizeType flippedSize, inputSize;
  correlatorType::MeasureType currentScore=10, bestScore=10;
  double inputVolumeTip, transformedVolumeTip;
  double dimensionsOfInterest[TInputImage::ImageDimension];
  double step=1, middleValue=0, numberOfPoints=360;
  double shiftStep=2, shiftMiddleValue=0, shiftNumberOfPoints=20;
  double angle, shiftX, shiftY;
  double theOrigin[TInputImage::ImageDimension];
  double theSpacing[TInputImage::ImageDimension];
  double bestAngle=0, bestShiftX=0;
  int j, singletonDimensionNumber=-1, verticalDimensionNumber;;
  char fname[100];
  estimateType *estimate=new estimateType;

  identityTransform->SetIdentity();
//  shifterTransform->SetIdentity();
  thresholder->ThresholdOutside(0, 0.2);
//  thresholder->ThresholdAbove(2);
  thresholder->SetOutsideValue(1);
  thresholder->SetInput(volume);
  thresholder->Update();
  originalImage=thresholder->GetOutput();
//  printVolumeTo(originalImage, 1, "c:\\tmp", "aorig0");    
  inputSize=originalImage->GetLargestPossibleRegion().GetSize();
  for(int i=0; i<TInputImage::ImageDimension; i++)
  {
    flipAxes[i]=0;
    translations[i]=0;
    rotationAxes[i]=0;
    verticalShift[i]=0;
    horizontalShift[i]=0;
    dimensionsOfInterest[i]=0;
    theOrigin[i]=0;
    theSpacing[i]=1;
    if(inputSize[i]==1)
    {
      if(singletonDimensionNumber==-1)
      {
        singletonDimensionNumber=i;
      }
      else
      {
        fprintf(stderr, "\nMidsagittalPlaneExtractionImageFilter::EstimateAngle():More than one singleton dimension. Quitting.\n");
        exit(5);
      }
    }
  }

  if(singletonDimensionNumber==-1)
  {
    fprintf(stderr, "\nMidsagittalPlaneExtractionImageFilter::estimateAngle():no singleton dimensions. Quitting.\n");
    exit(5);
  }

  if(singletonDimensionNumber==1)
  {
    verticalDimensionNumber=2;
  }
  else if(singletonDimensionNumber==2)
  {
    verticalDimensionNumber=1;
  }

  flipAxes[0]=1;
  rotationAxes[singletonDimensionNumber]=1;
  dimensionsOfInterest[singletonDimensionNumber]=-1;
  centerOfMass=findCentroid(volume, dimensionsOfInterest);
  for(int i=0; i<TInputImage::ImageDimension; i++)
  {
    flippedCenterOfMass[i]=centerOfMass[i];
//    theOrigin[i]=flippedCenterOfMass[i];
  }
  flippedCenterOfMass[0]=inputSize[0]-flippedCenterOfMass[0];
  horizontalShift[0]=-centerOfMass[0]+flippedCenterOfMass[0];
  inputVolumeTip=findHighestPoint((typename Superclass::InputImageConstPointer)originalImage, verticalDimensionNumber, inputSize[1]*0.75);
//  inputVolumeTip=findHighestPoint(volume, 1, inputSize[1]);


  affineTransform->SetIdentity();
//  printVolumeTo(volume, 2, "c:\\tmp", "avol0");    
  flipper->SetFlipAxes(flipAxes);
  flipper->SetInput(originalImage);
  flipper->Update();
  flippedImage=flipper->GetOutput();
  flipper=NULL;
  flippedImage->SetOrigin(theOrigin);
  flippedImage->SetSpacing(theSpacing);
  flippedImage->SetPhysicalToIndexTransform(identityTransform);
  flippedImage->SetIndexToPhysicalTransform(identityTransform);
//  printVolumeTo(flippedImage, 1, "c:\\tmp", "aflip0");    
  linearInterpolator->SetInputImage(flippedImage);
  correlator->SetInterpolator(linearInterpolator);
  correlator->SetFixedImage(originalImage);
  correlator->SetMovingImage(flippedImage);
  correlator->SetFixedImageRegion(originalImage->GetLargestPossibleRegion());
  backupCorrelator->SetFixedImage(originalImage);
  backupCorrelator->SetFixedImageRegion(originalImage->GetLargestPossibleRegion());
  backupCorrelator->SetTransform((itk::Transform<double,TInputImage::ImageDimension, TInputImage::ImageDimension>::Pointer)identityTransform);
    transformer->SetInput(flippedImage);
  transformer->SetInterpolator((itk::InterpolateImageFunction<TInputImage, double>::Pointer) linearInterpolator);
  transformer->SetSize(flippedImage->GetLargestPossibleRegion().GetSize());
  transformer->SetOutputSpacing(flippedImage->GetSpacing());
  transformer->SetOutputOrigin(flippedImage->GetOrigin());

  transformerShifter->SetInterpolator((itk::InterpolateImageFunction<TInputImage, double>::Pointer) linearInterpolator);
  transformerShifter->SetSize(flippedImage->GetLargestPossibleRegion().GetSize());
  transformerShifter->SetOutputSpacing(flippedImage->GetSpacing());
  transformerShifter->SetOutputOrigin(flippedImage->GetOrigin());
  transformerShifter->SetInput(flippedImage);
  
    shiftX=horizontalShift[0];
  estimate->shiftsY=new double[numberOfPoints+2];
  j=0;
  thresholder=thresholderType::New();
  thresholder->ThresholdOutside(0, 0.2);
  thresholder->SetOutsideValue(1);
  for(angle=middleValue-floor(numberOfPoints/2)*step; angle<=middleValue+floor(numberOfPoints/2)*step; angle+=step)
//  for(angle=-1; angle<=1; angle+=1)
  {
    affineTransform->SetIdentity();
/*    for(int i=0; i<TInputImage::ImageDimension; i++)
    {
      translations[i]=-flippedCenterOfMass[i]-20;
    }
    translations[TInputImage::ImageDimension-1]=0; 
    affineTransform->Translate(translations); */
    affineTransform->Rotate3D(rotationAxes, angle/180*pi, 1);
    transformedPoint=affineTransform->TransformPoint(flippedCenterOfMass);
    for(int i=0; i<TInputImage::ImageDimension; i++)
    {
      translations[i]=-transformedPoint[i]+flippedCenterOfMass[i];
    }

//  translations[0]=-translations[0];
//      params=affineTransform->GetParameters();
//      fprintf(stdout, "\n%f %f %f | %f %f %f |  %f %f %f |  %f %f %f\n", params[0], params[1], params[2], params[3]
//        , params[4], params[5], params[6], params[7], params[8], params[9], params[10], params[11]);
/*    for(int i=0; i<TInputImage::ImageDimension; i++)
    {
      translations[i]=flippedCenterOfMass[i];
    }  */
    affineTransform->Translate(translations, 0);
//      params=affineTransform->GetParameters();
//      fprintf(stdout, "\n%f %f %f | %f %f %f |  %f %f %f |  %f %f %f\n", params[0], params[1], params[2], params[3]
//        , params[4], params[5], params[6], params[7], params[8], params[9], params[10], params[11]);
//    savedAffineTransform->SetParameters(affineTransform->GetParameters());

    transformer->SetTransform((itk::Transform<double,TInputImage::ImageDimension, TInputImage::ImageDimension>::Pointer) affineTransform);
//      params=affineTransform->GetParameters();
//      fprintf(stdout, "\n%f %f %f | %f %f %f |  %f %f %f |  %f %f %f\n", params[0], params[1], params[2], params[3]
//        , params[4], params[5], params[6], params[7], params[8], params[9], params[10], params[11]);
    transformer->Update();
//    affineTransform->SetParameters(savedAffineTransform->GetParameters());
//      params=affineTransform->GetParameters();
//      fprintf(stdout, "\n%f %f %f | %f %f %f |  %f %f %f |  %f %f %f\n", params[0], params[1], params[2], params[3]
//        , params[4], params[5], params[6], params[7], params[8], params[9], params[10], params[11]);
    transformedImage=transformer->GetOutput();
//    printVolumeTo(flippedImage, 0, "c:\\tmp", "aflip0");    
//    printVolumeTo(transformedImage, 1, "c:\\tmp", "atrans0");    
    transformedVolumeTip=findHighestPoint((typename Superclass::InputImageConstPointer)transformedImage, verticalDimensionNumber, inputVolumeTip);
    verticalShift[verticalDimensionNumber]=-inputVolumeTip+transformedVolumeTip;
    estimate->shiftsY[j]=verticalShift[1];
    j++;


    
//    affineTransform->SetParameters(savedAffineTransform->GetParameters());

    affineTransform->Translate(verticalShift, 1);
//    horizontalShift[0]+=shiftMiddleValue-floor(shiftNumberOfPoints/2)*shiftStep;
    horizontalShift[0]+=-floor(shiftNumberOfPoints/2)*shiftStep;
    affineTransform->Translate(horizontalShift, 1);
//      params=affineTransform->GetParameters();
//      fprintf(stdout, "\n%f %f %f | %f %f %f |  %f %f %f |  %f %f %f\n", params[0], params[1], params[2], params[3]
//        , params[4], params[5], params[6], params[7], params[8], params[9], params[10], params[11]);
    horizontalShift[0]=shiftStep;
    for(int i=-floor(shiftNumberOfPoints/2)*shiftStep; i<=floor(shiftNumberOfPoints/2)*shiftStep; i+=shiftStep)
//    for(int i=-20; i<=-20; i+=2)
    {
      affineTransform->Translate(horizontalShift, 1);
//      savedAffineTransform->SetParameters(affineTransform->GetParameters());
//      params=affineTransform->GetParameters();
//      fprintf(stdout, "\n%f %f %f | %f %f %f |  %f %f %f |  %f %f %f\n", params[0], params[1], params[2], params[3]
//        , params[4], params[5], params[6], params[7], params[8], params[9], params[10], params[11]);

  transformerType::Pointer transformerShifter=transformerType::New();
  transformerShifter->SetInterpolator((itk::InterpolateImageFunction<TInputImage, double>::Pointer) linearInterpolator);
  transformerShifter->SetSize(flippedImage->GetLargestPossibleRegion().GetSize());
  transformerShifter->SetOutputSpacing(flippedImage->GetSpacing());
  transformerShifter->SetOutputOrigin(flippedImage->GetOrigin());
  transformerShifter->SetInput(flippedImage);


//      transformerShifter->SetInput(flippedImage);
      transformerShifter->SetTransform((itk::Transform<double,TInputImage::ImageDimension, TInputImage::ImageDimension>::Pointer) affineTransform);
      thresholder->SetInput(transformerShifter->GetOutput());
      thresholder->Update();
      transformedImage=thresholder->GetOutput();
//      affineTransform->SetParameters(savedAffineTransform->GetParameters());
//      params=affineTransform->GetParameters();
//      fprintf(stdout, "\n%f %f %f | %f %f %f |  %f %f %f |  %f %f %f\n", params[0], params[1], params[2], params[3]
//        , params[4], params[5], params[6], params[7], params[8], params[9], params[10], params[11]);
//      transformerShifter->Update();
//      transformedImage=transformerShifter->GetOutput();
//      sprintf(fname, "transformed%0.1f%d\0", angle, i);
//      printVolumeTo(transformedImage, 1, "c:\\tmp", fname);    

      backupLinearInterpolator->SetInputImage(transformedImage);
      backupCorrelator->SetMovingImage(transformedImage);
      backupCorrelator->SetInterpolator(backupLinearInterpolator);

//      correlator->SetTransform(identityTransform);
      correlator->SetTransform(affineTransform);
//    params=correlator->GetTransform()->GetParameters();
//      fprintf(stdout, "\nCorrelator before: %f %f %f | %f %f %f |  %f %f %f |  %f %f %f\n", params[0], params[1], params[2], params[3]
//        , params[4], params[5], params[6], params[7], params[8], params[9], params[10], params[11]);

//      currentScore=correlator->GetValue(affineTransform->GetParameters());
//      currentScore=correlator->GetValue(identityTransform->GetParameters());
      
//      params=affineTransform->GetParameters();
//      fprintf(stdout, "\n%f %f %f | %f %f %f |  %f %f %f |  %f %f %f\n", params[0], params[1], params[2], params[3]
//        , params[4], params[5], params[6], params[7], params[8], params[9], params[10], params[11]);
      currentScore=backupCorrelator->GetValue(identityTransform->GetParameters());
//  cout<<endl<<"angle="<<angle<<", shift="<<i<<"    correllation is "<<abs(currentScore)<<endl;
//    params=correlator->GetTransform()->GetParameters();
//      fprintf(stdout, "\nCorrelator after%f %f %f | %f %f %f |  %f %f %f |  %f %f %f\n", params[0], params[1], params[2], params[3]
//        , params[4], params[5], params[6], params[7], params[8], params[9], params[10], params[11]);
      if(currentScore<bestScore)
      {
        bestScore=currentScore;
        bestAngle=angle;
//        bestShiftX=shiftMiddleValue-floor(shiftNumberOfPoints/2)*shiftStep+i;
        bestShiftX=i;
      }
//      affineTransform->SetParameters(savedAffineTransform->GetParameters());
    }

  }

  
    
  estimate->angleEstimate=bestAngle;
  estimate->center=middleValue;
  estimate->stepY=step;
  estimate->n=numberOfPoints;
  estimate->shiftX=bestShiftX;
  estimate->flippedCenterOfMass=new double[TInputImage::ImageDimension];
  for(int i=0; i<TInputImage::ImageDimension; i++)
  {
    estimate->flippedCenterOfMass[i]=flippedCenterOfMass[i];
  }
  return estimate;
}
 
template <class TInputImage, class TOutputImage>
double
MidsagittalPlaneExtractionImageFilter<TInputImage, TOutputImage>::
estimateShift(typename Superclass::InputImageConstPointer volume)
{
  typedef itk::FlipImageFilter<TInputImage> flipperType;
  typedef itk::LinearInterpolateImageFunction<TInputImage, double> linearInterpolatorType;
  typedef itk::AffineTransform<double, TInputImage::ImageDimension> affineTransformType;
  typedef itk::NormalizedCorrelationImageToImageMetric<TInputImage, TInputImage> correlatorType;
  typedef itk::ThresholdImageFilter<TInputImage> thresholderType;
  typedef itk::ResampleImageFilter<TInputImage, TInputImage> transformerType;

  transformerType::Pointer transformer=transformerType::New();
  thresholderType::Pointer thresholder=thresholderType::New();
  flipperType::Pointer flipper=flipperType::New();
  correlatorType::Pointer correlator=correlatorType::New();
  affineTransformType::Pointer affineTransform=affineTransformType::New();
  affineTransformType::Pointer identityTransform=affineTransformType::New();
  linearInterpolatorType::Pointer linearInterpolator=linearInterpolatorType::New();
  linearInterpolatorType::Pointer correlationLinearInterpolator=linearInterpolatorType::New();
  TInputImage::Pointer flippedImage;
  TInputImage::Pointer originalImage;
  flipperType::FlipAxesArrayType flipAxes;
  affineTransformType::OutputVectorType rotationAxes, translations, verticalShift, horizontalShift;
  TInputImage::RegionType::SizeType flippedSize, inputSize;
  correlatorType::MeasureType currentScore=10, bestScore=10;
  double dimensionsOfInterest[TInputImage::ImageDimension];
  itk::Point <double , TInputImage::ImageDimension> aPoint, transformedPoint, flippedCenterOfMass;  
  double  *centerOfMass;
  double step=2, middleValue=0, numberOfPoints;
  double shiftX, bestShiftX=0, shift;
  double theOrigin[TInputImage::ImageDimension];
  char fname[100];
  int j=0, singletonDimensionNumber=-1;

  thresholder->SetOutsideValue(1);
  thresholder->ThresholdOutside(0, 0.1);
  thresholder->SetInput(volume);
  thresholder->Update();
  originalImage=thresholder->GetOutput();

  identityTransform->SetIdentity();
   inputSize=originalImage->GetLargestPossibleRegion().GetSize();
  numberOfPoints=inputSize[0]/2;
  for(int i=0; i<TInputImage::ImageDimension; i++)
  {
    flipAxes[i]=0;
    translations[i]=0;
    horizontalShift[i]=0;
    dimensionsOfInterest[i]=0;
    theOrigin[i]=0;
    if(inputSize[i]==1)
    {
      if(singletonDimensionNumber==-1)
      {
        singletonDimensionNumber=i;
      }
      else
      {
        fprintf(stderr, "\nMidsagittalPlaneExtractionImageFilter::EstimateAngle():More than one singleton dimension. Quitting.\n");
        exit(5);
      }
    }
  }

  if(singletonDimensionNumber==-1)
  {
    fprintf(stderr, "\nMidsagittalPlaneExtractionImageFilter::estimateAngle():no singleton dimensions. Quitting.\n");
    exit(5);
  }

  flipAxes[0]=1;
  dimensionsOfInterest[singletonDimensionNumber]=-1;
  centerOfMass=findCentroid(volume, dimensionsOfInterest);
  for(int i=0; i<TInputImage::ImageDimension; i++)
  {
    flippedCenterOfMass[i]=centerOfMass[i];
//    theOrigin[i]=flippedCenterOfMass[i];
  }
  flippedCenterOfMass[0]=inputSize[0]-flippedCenterOfMass[0];

  flipAxes[0]=1;
  middleValue=centerOfMass[0]-flippedCenterOfMass[0];


  flipper->SetFlipAxes(flipAxes);
  flipper->SetInput(originalImage);
  flipper->Update();
  flippedImage=flipper->GetOutput();
  flipper=NULL;
  flippedImage->SetOrigin(theOrigin);
  flippedImage->SetSpacing(originalImage->GetSpacing());

  linearInterpolator->SetInputImage(flippedImage);
  correlationLinearInterpolator->SetInputImage(flippedImage);
  correlator->SetInterpolator(correlationLinearInterpolator);
  correlator->SetFixedImage(originalImage);
  correlator->SetFixedImageRegion(originalImage->GetLargestPossibleRegion());
  correlator->SetMovingImage(flippedImage);
  affineTransform->SetIdentity();
//  printVolumeTo(flippedImage, 0, "c:\\tmp", "sflip0");    
//  printVolumeTo(originalImage, 0, "c:\\tmp", "sorig0");    
  
  
  translations[0]=-(middleValue-floor(numberOfPoints/2)*step);
  affineTransform->Translate(translations, 1);
  translations[0]=-step;
  for(shiftX=middleValue-floor(numberOfPoints/2)*step; shiftX<=middleValue+floor(numberOfPoints/2)*step; shiftX+=step)
  {
    thresholder=thresholderType::New();
    thresholder->SetOutsideValue(1);
    thresholder->ThresholdOutside(0, 0.1);
    transformer=transformerType::New();
    transformer->SetInterpolator((itk::InterpolateImageFunction<TInputImage, double>::Pointer) linearInterpolator);
    transformer->SetSize(flippedImage->GetLargestPossibleRegion().GetSize());
    transformer->SetOutputSpacing(flippedImage->GetSpacing());
    transformer->SetOutputOrigin(flippedImage->GetOrigin());
    transformer->SetInput(flippedImage);
    transformer->SetTransform((itk::Transform<double,TInputImage::ImageDimension, TInputImage::ImageDimension>::Pointer) affineTransform);
    thresholder->SetInput(transformer->GetOutput());
    thresholder->Update();

    sprintf(fname, "transformed%0.3f", shiftX);
//    printVolumeTo(thresholder->GetOutput(), 0, "c:\\tmp", fname);    
    correlationLinearInterpolator->SetInputImage(flippedImage);
    correlator->SetInterpolator(correlationLinearInterpolator);
    correlator->SetMovingImage(thresholder->GetOutput());
    correlator->SetTransform(identityTransform);
    currentScore=correlator->GetValue(affineTransform->GetParameters());
    if(currentScore<bestScore)
    {
      bestScore=currentScore;
      shift=shiftX;
    }
    j++;
    affineTransform->Translate(translations, 1);
  }


  return shift;
}

} // end namespace itk


#endif

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
