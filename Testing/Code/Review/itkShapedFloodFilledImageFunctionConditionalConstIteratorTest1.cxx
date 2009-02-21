/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Test itk::ShapedFloodFilledImageFunctionConditionalConstIterator on a 2D
// image using the fully connected option.

#include "itkImageFileReader.h"
#include "itkBinaryThresholdImageFunction.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkFloodFilledImageFunctionConditionalConstIterator.h"

#include "itkShapedFloodFilledImageFunctionConditionalConstIterator.h"

int itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1(int argc, char *argv [] )
{
  if( argc < 2 )
    {
    std::cerr << "Error: missing arguments" << std::endl;
    std::cerr << argv[0] << " filename " << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    const unsigned int ImageDimension = 2;
    typedef unsigned char PixelType;
    
    typedef itk::Image<PixelType, ImageDimension> ImageType;
    typedef ImageType::RegionType                 RegionType;
    typedef ImageType::IndexType                  IndexType;
      
    typedef itk::BinaryThresholdImageFunction<ImageType> FunctionType;
    typedef itk::ShapedFloodFilledImageFunctionConditionalConstIterator<
                  ImageType, FunctionType> ShapedFloodFilledIteratorType;
    
    typedef itk::ImageFileReader<ImageType> ReaderType;
    
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName( argv[1] );
    reader->Update();
    
    IndexType index;
    index[0] = 29;
    index[1] = 47;
    
    std::vector<IndexType> seedList;
    seedList.push_back(index);
    
    RegionType region = reader->GetOutput()->GetBufferedRegion();

    FunctionType::Pointer function = FunctionType::New();
    
    function->SetInputImage ( reader->GetOutput() );
    function->ThresholdAbove ( 1 ); // >= 1  
    
    ShapedFloodFilledIteratorType shapedFloodIt(
        reader->GetOutput(), function, seedList);
    shapedFloodIt.SetFullyConnected(true); // 8-connected, default
    
    ImageType::Pointer visitedImage = ImageType::New();
    visitedImage->SetRegions(region);
    visitedImage->Allocate();
    visitedImage->FillBuffer(0);
    
    for (; !shapedFloodIt.IsAtEnd(); ++shapedFloodIt)
      {
      visitedImage->SetPixel( shapedFloodIt.GetIndex(), 255);
      }
    
    typedef itk::ImageRegionConstIterator<ImageType> ConstIteratorType;
    
    ConstIteratorType inIt(reader->GetOutput(), region);
    ConstIteratorType outIt(visitedImage, region);
    
    for (; !inIt.IsAtEnd(); ++inIt, ++outIt)
      {
      if (inIt.Get() != outIt.Get()) 
        {
        return EXIT_FAILURE;
        }
      }
    }
  catch (itk::ExceptionObject& e)
    {
    e.Print(std::cerr);
    return EXIT_FAILURE;
    }
  catch (...)
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
