/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPCAShapeSignedDistanceFunctionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkPCAShapeSignedDistanceFunction.h"

#include "vnl/vnl_sample.h"
#include "itkImageRegionIterator.h"
#include "itkEuler2DTransform.h"


/**
 * This module tests the functionality of the PCAShapeSignedDistanceFunction
 * class.
 *
 * The mean image, principal component images, standard deviations, and 
 * and weighting parameters are randomly generated. The signed distance is 
 * evaluated at all image points and compared to expected values. 
 * The test fails if the evaluated results is not within a certain tolerance 
 * of the expected results.
 */
int itkPCAShapeSignedDistanceFunctionTest( int, char *[])
{
  unsigned int i;
  typedef double CoordRep;
  const unsigned int Dimension    = 2;
  const unsigned int ImageWidth   = 3;
  const unsigned int ImageHeight  = 2;
  const unsigned int NumberOfPCs  = 3;


  // define a pca shape function
  typedef itk::PCAShapeSignedDistanceFunction<CoordRep,Dimension> ShapeFunction;
  ShapeFunction::Pointer shape = ShapeFunction::New();
//  shape->DebugOn();
  shape->SetNumberOfPrincipalComponents(NumberOfPCs);


  // set up the transform
  typedef itk::Euler2DTransform<double> transformType;
  transformType::Pointer transform = transformType::New();
  shape->SetTransform(transform);


  // prepare for image creation 
  typedef ShapeFunction::ImageType ImageType;

  ImageType::SizeType imageSize = {{ImageWidth, ImageHeight}};

  ImageType::IndexType startIndex;
  startIndex.Fill(0);

  ImageType::RegionType  region;
  region.SetSize(imageSize);
  region.SetIndex(startIndex);


  // set up the random number generator
  vnl_sample_reseed();
  ImageType::PixelType randomPixel;


  // set up the mean image
  ImageType::Pointer meanImage = ImageType::New();
  meanImage->SetRegions(region);
  meanImage->Allocate();

  typedef itk::ImageRegionIterator<ImageType> ImageIterator;
  ImageIterator meanImageIt(meanImage, meanImage->GetBufferedRegion());

  for(meanImageIt.GoToBegin(); !meanImageIt.IsAtEnd(); ++meanImageIt)
    {
    randomPixel = vnl_sample_normal(0, 1);
    meanImageIt.Set(randomPixel); 
    }

  shape->SetMeanImage(meanImage);


  // set up the NumberOfPCs principal component images
  ShapeFunction::ImagePointerVector   pcImages(NumberOfPCs);
  typedef std::vector<ImageIterator>  ImageIteratorVector;
  ImageIteratorVector                 pcImageIts(NumberOfPCs);

  for(i=0; i<NumberOfPCs; i++)
    {
    pcImages[i] = ImageType::New();
    pcImages[i]->SetRegions(region);
    pcImages[i]->Allocate();

    pcImageIts[i] = ImageIterator(pcImages[i], pcImages[i]->GetBufferedRegion());

    for(pcImageIts[i].GoToBegin(); !pcImageIts[i].IsAtEnd(); ++pcImageIts[i])
      {
      randomPixel = vnl_sample_normal(0, 1);
      pcImageIts[i].Set(randomPixel); 
      }
    }

  shape->SetPrincipalComponentImages(pcImages);


  // set up the standard deviation for each principal component images
  ShapeFunction::ParametersType pcStandardDeviations(NumberOfPCs);

  for(i=0; i<NumberOfPCs; i++)
    { pcStandardDeviations[i] = vnl_sample_normal(0, 1); }

  shape->SetPrincipalComponentStandardDeviations(pcStandardDeviations);


  // set up the parameters
  unsigned int numberOfShapeParameters  = shape->GetNumberOfShapeParameters();
  unsigned int numberOfPoseParameters   = shape->GetNumberOfPoseParameters();
  unsigned int numberOfParameters = 
    numberOfShapeParameters  + numberOfPoseParameters;;
  ShapeFunction::ParametersType parameters(numberOfParameters);

  for(i=0; i<numberOfParameters; i++)
    { parameters[i] = vnl_sample_normal(0, 1); }

  shape->SetParameters(parameters);


  // we must initialize the function before use
  shape->Initialize();


  // check pca shape calculation
  ShapeFunction::PointType  point;
  ImageType::IndexType      index;
  ShapeFunction::OutputType output;
  ShapeFunction::OutputType expected;

  std::cout << "check results:" << std::endl;
  unsigned int numberOfRotationParameters = Dimension*(Dimension-1)/2;
  unsigned int startIndexOfTranslationParameters = 
    numberOfShapeParameters + numberOfRotationParameters;

  ShapeFunction::TransformType::InputPointType p;
  ShapeFunction::TransformType::InputPointType q;

  for(meanImageIt.GoToBegin(); !meanImageIt.IsAtEnd(); ++meanImageIt)
    {
    // from index to physical point
    index = meanImageIt.GetIndex(); 
    meanImage->TransformIndexToPhysicalPoint(index, point);

    // inverse Euler2DTransform: first translation then rotation
    p[0] =  point[0] - parameters[startIndexOfTranslationParameters];
    p[1] =  point[1] - parameters[startIndexOfTranslationParameters + 1]; 

    double angle = parameters[numberOfShapeParameters]; 
    q[0] =  p[0] * cos(-angle) - p[1] * sin(-angle);
    q[1] =  p[0] * sin(-angle) + p[1] * cos(-angle);    

    // evaluate shape function
    output = shape->Evaluate(q);

    // calculate expected function value
    expected = meanImage->GetPixel(index);
    for(i=0; i<NumberOfPCs; i++)
      {
      expected += pcImages[i]->GetPixel(index) *
      pcStandardDeviations[i] * 
      parameters[i];
      }

    // check result
    std::cout << "f(" << point << ") = " << output << std::endl;

    if(vnl_math_abs( output - expected ) > 1e-9)
      {
      std::cout << "But expected value is: " << expected << std::endl;
      return EXIT_FAILURE;
      }
    }


  return EXIT_SUCCESS;
}
