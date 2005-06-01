/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMask3DMeshSourceTest.cxx
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

#include <iostream>

#include "itkImage.h"
#include "itkMesh.h"
#include "itkBinaryMask3DMeshSource.h"
#include "itkImageRegionIteratorWithIndex.h"


int itkBinaryMask3DMeshSourceTest(int, char *[])
{

  // Define the dimension of the images
  const unsigned int Dimension = 3;

  // Declare the types of the output images
  typedef itk::Image<unsigned short,   Dimension>   ImageType;

  // Declare the type of the index,size and region to initialize images
  typedef itk::Index<Dimension>                     IndexType;
  typedef itk::Size<Dimension>                      SizeType;
  typedef itk::ImageRegion<Dimension>               RegionType;
  typedef ImageType::PixelType                      PixelType;

  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  // Declare the type of the Mesh
  typedef itk::Mesh<double>                         MeshType;
  typedef MeshType::PointType                       PointType;

  typedef itk::BinaryMask3DMeshSource< ImageType, MeshType >   MeshSourceType;

  const PixelType backgroundValue = 0;
  const PixelType internalValue   = 1;
  
  SizeType size;
  size[0] = 128;  
  size[1] = 128;  
  size[2] = 128;  

  IndexType start;
  start.Fill(0);

  RegionType region;
  region.SetSize( size );
  region.SetIndex( start );
    
  ImageType::Pointer image = ImageType::New();

  image->SetRegions( region );
  image->Allocate();

  image->FillBuffer( backgroundValue );

  IteratorType it( image, region );
  it.GoToBegin();

  PointType             point;
  PointType             center;
  PointType::VectorType radial;
 
  IndexType centralIndex = start;
  centralIndex[0] += size[0] / 2;
  centralIndex[1] += size[1] / 2;
  centralIndex[2] += size[2] / 2;

  image->TransformIndexToPhysicalPoint( centralIndex, center );
  
  // 
  //  Create a digitized sphere in the middle of the image.
  //
  while( !it.IsAtEnd() ) 
    {
    image->TransformIndexToPhysicalPoint( it.GetIndex(), point );
    radial = point - center;
    if ( radial.GetNorm() < 60.0)
      {
      it.Set( internalValue );
      }
    ++it;
    }

  MeshSourceType::Pointer meshSource = MeshSourceType::New();

  meshSource->SetInput( image );
  meshSource->SetObjectValue( internalValue );

  try
    {
    meshSource->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception thrown during Update() " << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << meshSource->GetNumberOfNodes() << std::endl;
  std::cout << meshSource->GetNumberOfCells() << std::endl;

  return EXIT_SUCCESS;

}




