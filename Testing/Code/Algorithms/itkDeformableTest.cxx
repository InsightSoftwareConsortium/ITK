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

#include <itkImage.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkDeformableMesh3DFilter.h>
#include <itkMesh.h>
#include <itkGradientRecursiveGaussianImageFilter.h>
#include <itkCovariantVector.h>
#include <itkGradientImageFilter.h>
#include <itkGradientToMagnitudeImageFilter.h>
#include <itkDerivativeImageFilter.h>
#include <itkGradientVectorFlowImageFilter.h>
#include <itkLaplacianImageFilter.h>
#include "itkImageRegionIterator.h"
#include "itkShrinkImageFilter.h"
#include "itkBinaryMask3DMeshSource.h"

int itkDeformableTest(int, char*[] )
{
  int WIDTH = 32;
  int HEIGHT = 32;
  int DEPTH = 32;
  
  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<double, myDimension>           myImageType;

  // Declare the types of the output images
  typedef itk::Image<unsigned short, myDimension>   binaryImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>       myIndexType;

  // Declare the type of the size 
  typedef itk::Size<myDimension>        mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension> myRegionType;

  // Declare the type of the Mesh
  typedef itk::Mesh<double>   DMesh;

  typedef DMesh::PointType   OPointType;

  // Declare the type of the gradient image
  typedef itk::CovariantVector<double, myDimension> myGradientType;
  typedef itk::Image<myGradientType, myDimension>   myGradientImageType;
  typedef itk::CovariantVector<double, 2>           double2DVector;
  typedef itk::CovariantVector<int, 3>              int3DVector;
  typedef itk::CovariantVector<double, 3>           double3DVector;
  typedef itk::CovariantVector<int, 2>              int2DVector;

  typedef itk::BinaryMask3DMeshSource<DMesh>  myMeshSource;
  typedef itk::LaplacianImageFilter<myImageType, myImageType> myLaplacianFilterType;
  typedef itk::GradientVectorFlowImageFilter<myGradientImageType, myGradientImageType>
                                              myGVFFilterType;

  typedef itk::GradientImageFilter<myImageType, double, double>
                                              myGFilterType;

  typedef itk::GradientToMagnitudeImageFilter<myGradientImageType, myImageType>
                                              myGToMFilterType;

  typedef itk::DeformableMesh3DFilter<DMesh, DMesh>  DFilter;

  binaryImageType::Pointer       biimg=binaryImageType::New();
  myGradientImageType::Pointer   gdimg=myGradientImageType::New();

  typedef itk::ImageRegionIteratorWithIndex<myImageType>         myIteratorType;
  typedef itk::ImageRegionIteratorWithIndex<myGradientImageType> myGradientIteratorType;

  binaryImageType::SizeType      bisize={{WIDTH,HEIGHT,DEPTH}};
  binaryImageType::IndexType     biindex;
  binaryImageType::RegionType    biregion;
  biindex.Fill(0);
  biregion.SetSize(bisize);
  biregion.SetIndex(biindex);

  myGradientImageType::SizeType   gdsize={{WIDTH,HEIGHT,DEPTH}};
  myGradientImageType::IndexType  gdindex;
  myGradientImageType::RegionType gdregion;
  gdindex.Fill(0);
  gdregion.SetSize(gdsize);
  gdregion.SetIndex(gdindex);
  
  biimg->SetLargestPossibleRegion( biregion );
  biimg->SetBufferedRegion( biregion );
  biimg->SetRequestedRegion( biregion );
  biimg->Allocate();

  gdimg->SetLargestPossibleRegion( gdregion );
  gdimg->SetBufferedRegion( gdregion );
  gdimg->SetRequestedRegion( gdregion );
  gdimg->Allocate();

  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();

  mySizeType size={{WIDTH,HEIGHT,DEPTH}};
  myIndexType start;
  start.Fill(0);

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  itk::ImageRegionIteratorWithIndex <myImageType> it(inputImage, region);
  it.GoToBegin();
  itk::ImageRegionIteratorWithIndex <binaryImageType> bit(biimg, biregion);
  bit.GoToBegin();


  while( !it.IsAtEnd() ) 
  {
    it.Set( 0.0 );
    bit.Set( 0 );
    ++it;
    ++bit;
  }

  size[0] = 16;
  size[1] = 16;
  size[2] = 16;

  start[0] = 8;
  start[1] = 8;
  start[2] = 8;

  // Create one iterator for an internal region
  region.SetSize( size );
  region.SetIndex( start );
  biregion.SetSize( size );
  biregion.SetIndex( start );
  itk::ImageRegionIteratorWithIndex <myImageType> itb( inputImage, region );
  itk::ImageRegionIteratorWithIndex <binaryImageType> bitb( biimg, biregion );

  // Initialize the content the internal region
  while( !itb.IsAtEnd() ) 
  {
    itb.Set( 100.0 );
    bitb.Set ( 255 );
    ++itb;
    ++bitb;
  }

  //////////////////////////////////////////////////////////////////////////
  typedef itk::GradientRecursiveGaussianImageFilter<
                                            myImageType,
                                            myGradientImageType
                                            >  myFilterType;
            

  // Create a  Filter                                
  myFilterType::Pointer grfilter = myFilterType::New();
  myGFilterType::Pointer gfilter = myGFilterType::New();
  myGToMFilterType::Pointer gtomfilter = myGToMFilterType::New();

  // Connect the input images
  grfilter->SetInput( inputImage ); 

  // Set sigma
  grfilter->SetSigma( 1.0 );

  myLaplacianFilterType::Pointer m_LFilter = myLaplacianFilterType::New();
  myGVFFilterType::Pointer m_GVFFilter = myGVFFilterType::New();

  m_GVFFilter->SetInput(gfilter->GetOutput());
  m_GVFFilter->SetLaplacianFilter(m_LFilter);
  m_GVFFilter->SetNoiseLevel(500);

  gtomfilter->SetInput(grfilter->GetOutput());

  gfilter->SetInput(gtomfilter->GetOutput());
  gfilter->Update();

  std::cout << "The gradient map created!" << std::endl;

//  the gvf is temproraily disabled to speede up the test process
//  m_GVFFilter->Update();

//  std::cout << "GVF created! " << std::endl;

////////////////////////////////////////////////////////////////////////////////////////
// construct the deformable mesh
  myMeshSource::Pointer m_bmmeshsource = myMeshSource::New();

  DFilter::Pointer m_dfilter = DFilter::New();
  m_dfilter->SetInput(m_bmmeshsource->GetOutput());
//  m_dfilter->SetGradient(m_GVFFilter->GetOutput());
  m_dfilter->SetGradient(gfilter->GetOutput());

  m_bmmeshsource->SetBinaryImage( biimg );
  m_bmmeshsource->SetObjectValue( 255 );

  std::cout << "Deformable mesh created using Marching Cube!" << std::endl;

  double2DVector m_stiff;
  m_stiff[0] = 0.0001;
  m_stiff[1] = 0.1;

  double3DVector m_scale;
  m_scale[0] = 1;
  m_scale[1] = 1; 
  m_scale[2] = 1;
  m_dfilter->SetStiffness(m_stiff);
  m_dfilter->SetGradientMagnitude(0.8);
  m_dfilter->SetTimeStep(0.01);
  m_dfilter->SetStepThreshold(50);
  m_dfilter->SetScale(m_scale);
  std::cout << "Deformable mesh fitting...";
  m_dfilter->Update();
  std::cout << m_dfilter;
 
  std::cout << "Mesh Source: " << m_bmmeshsource;

// All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}




