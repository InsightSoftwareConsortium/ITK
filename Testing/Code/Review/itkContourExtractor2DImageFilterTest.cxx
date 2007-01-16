/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkContourExtractor2DImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageFileReader.h"
#include "itkContourExtractor2DImageFilter.h"
#include <iostream>
#include <vector>
#include <cmath>

const unsigned int Dimension = 2;
typedef unsigned char                                   PixelType;
typedef itk::Image<PixelType, Dimension>                ImageType;
typedef itk::ImageFileReader<ImageType>                 ReaderType;
typedef itk::ContourExtractor2DImageFilter<ImageType>   ExtractorType;
typedef ExtractorType::VertexType                       VertexType;
typedef std::pair<double, double>                       MyVertexType;
typedef std::vector<MyVertexType>                       MyVertexListType;
typedef std::vector<MyVertexListType>                   MyVertexListList;
const float FLOAT_EPSILON = 0.0001;

#include "PrecomputedContourData.h"

bool HasCorrectOutput(ExtractorType::Pointer extractor, 
                      MyVertexListList& correct)
{
  if (extractor->GetNumberOfOutputs() != correct.size()) return false;
  for(unsigned int i = 0; i < correct.size(); i++)
    {
    ExtractorType::VertexListConstPointer vertices = 
                extractor->GetOutput(i)->GetVertexList();
    MyVertexListType& correctVertices = correct[i];
    if (vertices->Size() != correctVertices.size()) return false;
    for(unsigned int j = 0; j < correctVertices.size(); j++)
      {
      const MyVertexType& correctVertex = correctVertices[j];
      const VertexType& vertex = vertices->ElementAt(j);
      if (fabs(correctVertex.first - vertex[0]) > FLOAT_EPSILON ||
          fabs(correctVertex.second - vertex[1]) > FLOAT_EPSILON) return false;
      }
    }
  return true;
}

int itkContourExtractor2DImageFilterTest(int argc, char *argv[])
{
  if( argc < 2 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " Input Test Image  " << std::endl;
    return 1;
    }
 
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  ExtractorType::Pointer extractor = ExtractorType::New();
  extractor->SetInput(reader->GetOutput());
  extractor->SetContourValue(127.5);
  bool testsPassed = true;
  try {
    extractor->VertexConnectHighPixelsOff();
    extractor->ReverseContourOrientationOff();
    extractor->Update();
    std::cout << "Test 1... ";
    if (!HasCorrectOutput(extractor, expected_disconnected_clockwise_outputs))
      {
      testsPassed = false;
      std::cout << "failed." << std::endl;
      }
    else std::cout << "passed." << std::endl;

    extractor->VertexConnectHighPixelsOff();
    extractor->ReverseContourOrientationOn();
    extractor->Update();
    std::cout << "Test 2... ";
    if (!HasCorrectOutput(extractor, 
            expected_disconnected_counterclockwise_outputs))
      {
      testsPassed = false;
      std::cout << "failed." << std::endl;
      }
    else std::cout << "passed." << std::endl;

    extractor->VertexConnectHighPixelsOn();
    extractor->ReverseContourOrientationOff();
    extractor->Update();
    std::cout << "Test 3... ";
    if (!HasCorrectOutput(extractor, expected_connected_clockwise_outputs))
      {
      testsPassed = false;
      std::cout << "failed." << std::endl;
      }
    else std::cout << "passed." << std::endl;

    extractor->VertexConnectHighPixelsOff();
    extractor->ReverseContourOrientationOff();
    // Move the region to evaluate in by one on the top and bottom
    ImageType::RegionType region = 
                        reader->GetOutput()->GetLargestPossibleRegion();
    ImageType::IndexType index = region.GetIndex();
    ImageType::SizeType size = region.GetSize();
    index[1] += 1;
    size[1] -= 2;
    extractor->SetRequestedRegion(ImageType::RegionType(index, size));
    extractor->Update();
    std::cout << "Test 4... ";
    if (!HasCorrectOutput(extractor, 
          expected_disconnected_clockwise_cropped_outputs))
      {
      testsPassed = false;
      std::cout << "failed." << std::endl;
      }
    else std::cout << "passed." << std::endl;

    } catch( itk::ExceptionObject & err ) { 
    std::cout << "ExceptionObject caught !" << std::endl; 
    std::cout << err << std::endl; 
    return -1;
    }
  if (testsPassed)
    {
    std::cout << "All tests passed." << std::endl;
    return 0;
    }
  return 1;
}
