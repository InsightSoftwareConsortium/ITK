/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImageRegionIterator.h"
#include "itkGradientImageFilter.h"
#include "itkSphereSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"

#include "itkBinaryMedialNodeMetric.h"
#include "itkGradientImageToBloxBoundaryPointImageFilter.h"
#include "itkBloxBoundaryPointToCoreAtomImageFilter.h"

//
// Main Executable Test Function
//
int itkBinaryMedialNodeMetricTest(int, char* [])
{

  //
  // set up the initial image
  //
  std::cout << "Setting up initial image" << std::endl;

  const unsigned int dimension = 3;
  typedef itk::Image<unsigned char, dimension> ImageType;
  ImageType::Pointer inputImage = ImageType::New();

  // set up spacing and origin
  ImageType::SpacingValueType inputImageSpacing[] = {1.0,1.0,1.0};
  ImageType::PointValueType inputImageOrigin[] = {0,0,0};
  inputImage->SetOrigin(inputImageOrigin);
  inputImage->SetSpacing(inputImageSpacing);

  // set up size and largest region
  ImageType::SizeValueType inputImageSize[] = {20,20,20};
  ImageType::SizeType inputImageSizeObject;
  inputImageSizeObject.SetSize(inputImageSize);
  ImageType::RegionType largestRegion;
  largestRegion.SetSize(inputImageSizeObject);
  inputImage->SetLargestPossibleRegion(largestRegion);
  inputImage->SetBufferedRegion(largestRegion);
  inputImage->SetRequestedRegion(largestRegion);
  inputImage->Allocate();

  // set all pixels to 0
  typedef itk::ImageRegionIterator<ImageType> ImageIterType;
  ImageIterType iter = ImageIterType(inputImage, largestRegion);
  for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
    {
    iter.Set(0);
    }

  // add a sphere to the image
  std::cout << "Adding sphere to image" << std::endl;

  typedef itk::SphereSpatialFunction<dimension> SphereFunctionType;
  typedef SphereFunctionType::InputType SphereFunctionPositionType;

  SphereFunctionType::Pointer sphereFunc = SphereFunctionType::New();
  sphereFunc->SetRadius(5);
  SphereFunctionPositionType center;
  center[0] = 10;
  center[1] = 10;
  center[2] = 10;
  sphereFunc->SetCenter(center);

  ImageType::IndexType seedPos;
  const ImageType::IndexValueType centerPos[] = {10,10,10};
  seedPos.SetIndex(centerPos);

  typedef itk::FloodFilledSpatialFunctionConditionalIterator
    <ImageType, SphereFunctionType> FuncIterType;
  FuncIterType sphereIter = FuncIterType(inputImage, sphereFunc, seedPos);
  for ( ; !sphereIter.IsAtEnd(); ++sphereIter)
    {
    sphereIter.Set(255);
    }


  //
  // compute the gradient image
  //
  std::cout << "Computing gradient of image" << std::endl;

  typedef itk::GradientImageFilter<ImageType, float, float>  GradientImageFilterType;
  typedef GradientImageFilterType::OutputImageType           GradientImageType;

  GradientImageFilterType::Pointer gradientFilter = GradientImageFilterType::New();
  gradientFilter->SetInput(inputImage);


  //
  // convert to a blox boundary point image
  //
  std::cout << "Converting to blox boundary point image" << std::endl;

  typedef itk::GradientImageToBloxBoundaryPointImageFilter<GradientImageType> GrToBloxFilterType;
  typedef GrToBloxFilterType::OutputImageType                                 BloxBPImageType;

  GrToBloxFilterType::Pointer toBloxFilter = GrToBloxFilterType::New();
  toBloxFilter->SetInput(gradientFilter->GetOutput());
  toBloxFilter->Update();


  //
  // convert to core atom images
  //
  std::cout << "Converting to core atom image and duplicating" << std::endl;

  typedef itk::BloxBoundaryPointToCoreAtomImageFilter<3> BPToCoreAtomFilterType;
  typedef BPToCoreAtomFilterType::OutputImageType        CoreAtomImageType;

  BPToCoreAtomFilterType::Pointer toCoreAtomFilter = BPToCoreAtomFilterType::New();
  toCoreAtomFilter->SetInput(toBloxFilter->GetOutput());
  toCoreAtomFilter->SetDistanceMin(8.0);
  toCoreAtomFilter->SetDistanceMax(12.0);
  toCoreAtomFilter->SetEpsilon(0.05);
  toCoreAtomFilter->SetPolarity(0);

  CoreAtomImageType::Pointer coreAtomImageA = toCoreAtomFilter->GetOutput();
  CoreAtomImageType::Pointer coreAtomImageB = toCoreAtomFilter->GetOutput();
  toCoreAtomFilter->Update();


  //
  // Process the core atom images
  //
  std::cout << "Performing core atom analysis" << std::endl;

  coreAtomImageA->DoEigenanalysis();
  coreAtomImageA->DoCoreAtomVoting();
  coreAtomImageB->DoEigenanalysis();
  coreAtomImageB->DoCoreAtomVoting();


  //
  // test the distance metric
  //
  std::cout << "Number of medial nodes = " << coreAtomImageA->GetMedialNodeCount() << std::endl;

  typedef CoreAtomImageType::RegionType CARegionType;
  typedef itk::ImageRegionIterator<CoreAtomImageType> CAIterType;
  typedef itk::BinaryMedialNodeMetric<dimension> MetricType;
  typedef MetricType::MedialNode MedialNodeType;

  double totalDistance = 0;
  MetricType::Pointer medialBinaryMetric = MetricType::New();

  // loop over pixels in groups of two
  CARegionType::SizeType imSize = coreAtomImageA->GetLargestPossibleRegion().GetSize();
  //std::cout << "Size = " << imSize << std::endl;
  for (unsigned int x = 0; x < imSize[0]; x+=2)
    {
    for (unsigned int y = 0; y < imSize[1]; y++)
      {
      for (unsigned int z = 0; z < imSize[2]; z++)
        {

        //DEBUG
        std::cout << "Working on pixels ("<<x<<","<<y<<","<<z<<") and ("<<x+1<<","<<y<<","<<z<<")"<<std::endl;

        CoreAtomImageType::IndexType idx1;
        idx1[0] = x;
        idx1[1] = y;
        idx1[2] = z;
        CoreAtomImageType::IndexType idx2;
        idx2[0] = x+1;
        idx2[1] = y;
        idx2[2] = z;

        MedialNodeType* A1 = &(coreAtomImageA->GetPixel(idx1));
        MedialNodeType* A2 = &(coreAtomImageA->GetPixel(idx2));
        MedialNodeType* B1 = &(coreAtomImageB->GetPixel(idx1));
        MedialNodeType* B2 = &(coreAtomImageB->GetPixel(idx2));

        medialBinaryMetric->SetMedialNodes(A1,A2,B1,B2);
        medialBinaryMetric->ShowCalculation();
        medialBinaryMetric->Initialize();
        totalDistance += (1.0-medialBinaryMetric->GetResult());

        }
      }
    }

  // check to make sure the metric measured the images as the same
  double precision = 0.0000001;
  if ( totalDistance > precision )
    {
    std::cout << "[FAILED] Metric did not identify two identical images. Total distance = " << totalDistance << std::endl;
    return EXIT_FAILURE;
    }

  // Test printing
  std::cout << "Printing Metric" << std::endl << medialBinaryMetric << std::endl;

  // Test type name
  if (strcmp(medialBinaryMetric->GetNameOfClass(),"BinaryMedialNodeMetric"))
    {
    std::cout << "[FAILED] Class info not reported correctly" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Metric Type Info: " << medialBinaryMetric->GetNameOfClass() << std::endl;

  // finished successfully
  return EXIT_SUCCESS;
}
