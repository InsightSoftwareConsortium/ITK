/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2.cxx
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
// image using 4 connectivity.

#include "itkBinaryThresholdImageFunction.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkFloodFilledImageFunctionConditionalConstIterator.h"

#include "itkShapedFloodFilledImageFunctionConditionalConstIterator.h"

int itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2( int, char * [] )
{  
  try
    {
    const unsigned int ImageDimension = 2;
    typedef unsigned char PixelType;

    typedef itk::Image<PixelType, ImageDimension> ImageType;
    typedef ImageType::RegionType                 RegionType;
    typedef ImageType::IndexType                  IndexType;

    typedef itk::BinaryThresholdImageFunction<ImageType> FunctionType;
    typedef itk::FloodFilledImageFunctionConditionalConstIterator<
      ImageType, FunctionType> FloodFilledIteratorType;
    typedef itk::ShapedFloodFilledImageFunctionConditionalConstIterator<
      ImageType, FunctionType> ShapedFloodFilledIteratorType;  
    
    std::vector<IndexType> seedList;
    
    RegionType region;
    region.SetSize(0, 128);
    region.SetSize(1, 128);
    region.SetIndex(0, 0);
    region.SetIndex(1, 0);
    
    ImageType::Pointer inputImage = ImageType::New();
    inputImage->SetRegions(region);
    inputImage->Allocate();
    inputImage->FillBuffer(0);
    
    itk::ImageLinearIteratorWithIndex<ImageType> it( inputImage, region );
    
    // make sure that we create a 4-connected image!
    for( unsigned int dir = 0; dir < ImageDimension; ++dir )
      {
      it.SetDirection(dir);
      it.GoToBegin();
      while (!it.IsAtEnd())
        {
        while (!it.IsAtEndOfLine())
          {
          // add a seed
          if( seedList.empty() )
            {
            seedList.push_back(it.GetIndex());
            }
          
          it.Set(255);
          ++it;
          }
        
        // and jump over every 
        it.NextLine();
        if (!it.IsAtEnd())
          { 
          it.NextLine();
          }
        if (!it.IsAtEnd()) 
          {
          it.NextLine();
          }
        }
      }

    FunctionType::Pointer function = FunctionType::New();
    
    function->SetInputImage ( inputImage );
    function->ThresholdAbove ( 1 ); // >= 1  
    
    FloodFilledIteratorType floodIt(inputImage, function, seedList);
    ShapedFloodFilledIteratorType shapedFloodIt(inputImage, function, seedList);
    
    shapedFloodIt.SetFullyConnected(false); // 4-connected, default
    
    for (unsigned short i = 1; !floodIt.IsAtEnd(); ++floodIt, 
                                                   ++shapedFloodIt,
                                                   ++i)
      {
      if (floodIt.GetIndex() != shapedFloodIt.GetIndex()) 
        {
        return EXIT_FAILURE;
        }
      }
    
    if (!floodIt.IsAtEnd() || !shapedFloodIt.IsAtEnd()) 
      {
      return EXIT_FAILURE;
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
