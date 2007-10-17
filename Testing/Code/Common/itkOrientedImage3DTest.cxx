/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrientedImage3DTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImageFileReader.h"
#include "itkOrientedImage.h"

int itkOrientedImage3DTest( int ac, char * av[] )
{

  if( ac < 14 )
    {
    std::cerr << "Usage: " << av[0] 
    << " InputImage  "
    << "corner1x corner1y corner1z "
    << "corner2x corner2y corner2z "
    << "corner3x corner3y corner3z "
    << "corner4x corner4y corner4z "
    << std::endl;
    return EXIT_FAILURE;
    }
  
  const unsigned int Dimension = 3;
  typedef unsigned char PixelType;

  typedef itk::Image<PixelType, Dimension>    ImageType;
  typedef itk::ImageFileReader< ImageType >   ReaderType;

  ReaderType::Pointer reader = ReaderType::New();
  
  reader->SetFileName( av[1] );

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
  
  ImageType::ConstPointer image = reader->GetOutput();
   
  ImageType::DirectionType directionCosines = image->GetDirection();

  std::cout << directionCosines << std::endl;

  unsigned int element = 2;
  const double tolerance = 1e-5;

  ImageType::RegionType region = image->GetLargestPossibleRegion();
  ImageType::SizeType   size   = region.GetSize();

  const int numberOfPointsToTest = 4;

  ImageType::IndexType index[numberOfPointsToTest];
  ImageType::PointType physicalPoint;

  index[0][0] = 0;
  index[0][1] = 0;
  index[0][2] = 0;

  index[1][0] = size[0];
  index[1][1] = 0;
  index[1][2] = 0;

  index[2][0] = 0;
  index[2][1] = size[1];
  index[2][2] = 0;

  index[3][0] = 0;
  index[3][1] = 0;
  index[3][2] = size[2];



  for( unsigned int pointId=0; pointId < numberOfPointsToTest; ++pointId )
    {

    image->TransformIndexToPhysicalPoint( index[pointId], physicalPoint );

    for( unsigned int dim=0; dim < Dimension; ++dim )
      {
      const double expectedValue = atof( av[ element++ ] );
      const double currentValue = physicalPoint[dim];
      const double difference = currentValue - expectedValue;
      if( vnl_math_abs( difference ) > tolerance )
        {
        std::cerr << "Error: " << std::endl;
        std::cerr << "in Point # " << pointId << std::endl;
        std::cerr << "Expected      = " << expectedValue << std::endl;
        std::cerr << "Read          = " << currentValue << std::endl;
        std::cerr << "Index         = " << index[pointId] << std::endl;
        std::cerr << "PhysicalPoint = " << physicalPoint << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  return EXIT_SUCCESS;
}
