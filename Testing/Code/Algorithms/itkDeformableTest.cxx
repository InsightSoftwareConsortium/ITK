/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <math.h>
#include <iostream>
#include <time.h>

#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkMesh.h"
#include "itkDeformableMesh3DFilter.h"
#include "itkSphereSource.h"
#include "itkGradientRecursiveGaussianImageFilter.h"


int itkDeformableTest(int, char**)
{
  const int WIDTH  = 200;
  const int HEIGHT = 200;
  const int DEPTH  = 200;

  
  // Define the dimension of the images
  const unsigned int Dimension = 3;

  // Declare the types of the output images
  typedef itk::Image<unsigned short,   Dimension>   ImageType;

  // Declare the type of the index,size and region to initialize images
  typedef itk::Index<Dimension>                     IndexType;
  typedef itk::Size<Dimension>                      SizeType;
  typedef itk::ImageRegion<Dimension>               RegionType;
  typedef itk::ImageRegionIterator<ImageType>       IteratorType;
  typedef ImageType::PixelType                      PixelType;

  // Declare the type of the Mesh
  typedef itk::Mesh<double>                         MeshType;
  typedef MeshType::PointType                       PointType;

  typedef itk::DeformableMesh3DFilter< 
                                  MeshType,
                                  MeshType >        MeshFilterType;

                                  
  typedef MeshFilterType::GradientImageType         GradientImageType;

  typedef itk::SphereSource< MeshType >             SphereSourceType;

  typedef itk::GradientRecursiveGaussianImageFilter< 
                              ImageType,
                              GradientImageType >   GradientFilterType;

  PixelType backgroundValue = 0;
  PixelType internalValue   = 0;
  
  SizeType size;
  size[0] = WIDTH;  
  size[1] = HEIGHT;  
  size[2] = DEPTH;  

  IndexType start;
  start.Fill(0);

  RegionType region;
  region.SetSize( size );
  region.SetIndex( start );
    
  ImageType::Pointer image = ImageType::New();

  image->SetRegions( region );
  image->Allocate();


  IteratorType it( image, region );
  it.GoToBegin();

  while( !it.IsAtEnd() ) 
    {
    it.Set( backgroundValue );
    ++it;
    }

  SizeType smallerSize;
  smallerSize[0] = 100;
  smallerSize[1] = 100;
  smallerSize[2] = 100;

  IndexType internalStart;
  internalStart[0] = 50;
  internalStart[1] = 50;
  internalStart[2] = 50;

  // Create one iterator for an internal region
  RegionType internalRegion;
  internalRegion.SetSize( smallerSize );
  internalRegion.SetIndex( internalStart );

  IteratorType ir( image, internalRegion );
  ir.GoToBegin();
  while( !ir.IsAtEnd() )
    {
    ir.Set( internalValue );  
    ++ir;
    }

  GradientFilterType::Pointer gradientFilter = 
                                GradientFilterType::New();

  gradientFilter->SetInput( image );
  gradientFilter->SetSigma( 3.0 );

  try
    {
    gradientFilter->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception thrown during gradient filter Update() " << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }

  SphereSourceType::Pointer sphereSource = SphereSourceType::New();
  
  typedef itk::Point<float,3>  FloatPointType;
  
  FloatPointType center;
  center[0] = WIDTH  / 2.0;
  center[1] = HEIGHT / 2.0;
  center[2] = DEPTH  / 2.0;
  
  FloatPointType  scale;
  scale[0]  = WIDTH  / 8.0;
  scale[1]  = HEIGHT / 8.0;
  scale[2]  = DEPTH  / 8.0;

  sphereSource->SetCenter(center);
  sphereSource->SetResolutionX(10);
  sphereSource->SetResolutionY(10);
  sphereSource->SetScale(scale);

  try
    {
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception thrown during sphere source filter Update() " << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }
    
 
  MeshFilterType::Pointer meshFilter = MeshFilterType::New();

  meshFilter->SetGradient( gradientFilter->GetOutput() );
  meshFilter->SetInput( sphereSource->GetOutput() );

  try
    {
    meshFilter->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception thrown during mesh filter Update() " << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;

}




