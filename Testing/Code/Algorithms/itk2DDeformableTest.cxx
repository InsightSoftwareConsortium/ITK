/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itk2DDeformableTest.cxx
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

#include "itkBalloonForceFilter.h"
#include "itkSphereMeshSource.h"

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

// test images, change path to your local dir system pls

//#define BINARYMASK "../../../Data/bones/b_26.raw"
//#define INPUTFILE  "../../../Data/bones/test.raw"
//#define OUTPUTFILE "../../../tmp/test.raw"

int itk2DDeformableTest(int, char**)
{
// change the image size to your test images
  int WIDTH = 100;
  int HEIGHT = 100;
  
  // Define the dimension of the images
  const unsigned int myDimension = 2;

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

  // using unsigned char for masks 
  unsigned char *ImageBuffer = new unsigned char [WIDTH*HEIGHT];

  // Declare the type of the gradient image
  typedef itk::CovariantVector<double, myDimension> myGradientType;
  typedef itk::Image<myGradientType, myDimension>   myGradientImageType;
  typedef itk::Vector<double, 2>           double2DVector;
  typedef itk::CovariantVector<int, 3>              int3DVector;
  typedef itk::CovariantVector<double, 3>           double3DVector;
  typedef itk::CovariantVector<int, 2>              int2DVector;

  typedef itk::SphereMeshSource<DMesh>  myMeshSource;
  typedef itk::LaplacianImageFilter<myImageType, myImageType> myLaplacianFilterType;
  typedef itk::GradientVectorFlowImageFilter<myGradientImageType, myGradientImageType>
                                              myGVFFilterType;

  typedef itk::GradientImageFilter<myImageType, double, double>
                                              myGFilterType;

  typedef itk::GradientToMagnitudeImageFilter<myGradientImageType, myImageType>
                                              myGToMFilterType;

  typedef itk::BalloonForceFilter<DMesh, DMesh>  DFilter;

  binaryImageType::Pointer       biimg=binaryImageType::New();
  myGradientImageType::Pointer   gdimg=myGradientImageType::New();

  typedef itk::ImageRegionIteratorWithIndex<myImageType>         myIteratorType;
  typedef itk::ImageRegionIteratorWithIndex<myGradientImageType> myGradientIteratorType;

  binaryImageType::SizeType      bisize={{WIDTH,HEIGHT}};
  binaryImageType::IndexType     biindex;
  binaryImageType::RegionType    biregion;
  biindex.Fill(0);
  biregion.SetSize(bisize);
  biregion.SetIndex(biindex);

  myGradientImageType::SizeType   gdsize={{WIDTH,HEIGHT}};
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

  mySizeType size={{WIDTH,HEIGHT}};
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

  ///////////////////////////////////////////////////////////////////
  // if using local test file
  /*
  FILE *inputfile = fopen(INPUTFILE, "rb");
  int DEPTH = 1;
  for (int i=0; i<DEPTH; i++) {
    fread(ImageBuffer, 1, WIDTH*HEIGHT, inputfile);
    for (int j=0; j<WIDTH*HEIGHT; j++) {
      it.Set((double)ImageBuffer[j]);
      ++it;
    }
  }

  fclose(inputfile);

  FILE *binarymask = fopen(BINARYMASK, "rb");
  unsigned short mask;
  for (int i=0; i<DEPTH; i++) {
    fread(ImageBuffer, 1, WIDTH*HEIGHT, binarymask);
    for (int j=0; j<WIDTH*HEIGHT; j++) {
      if (ImageBuffer[j] != 0) mask = 255;
      else mask = 0;
      bit.Set( mask );
      ++bit;
    }
  }

  fclose(binarymask);
  */


  /////////////////////////////////////////////////////////////////////////
  // create user defined images for test
  

  while( !it.IsAtEnd() ) 
  {
    it.Set( 0.0 );
    bit.Set( 0 );
    ++it;
    ++bit;
  }

  size[0] = 60;
  size[1] = 60;
  size[2] = 60;

  start[0] = 20;
  start[1] = 20;
  start[2] = 20;

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

  itk::ShrinkImageFilter< myImageType, myImageType >::Pointer dshrink;
  dshrink = itk::ShrinkImageFilter< myImageType, myImageType >::New();
  dshrink->SetInput( inputImage );
  dshrink->SetNumberOfThreads(4);

  unsigned int dfactors[2] = { 1, 1 };
  dshrink->SetShrinkFactors(dfactors);
  dshrink->UpdateLargestPossibleRegion();

  myImageType::RegionType drequestedRegion;
  drequestedRegion = dshrink->GetOutput()->GetRequestedRegion();

  typedef itk::GradientRecursiveGaussianImageFilter<
                                            myImageType,
                                            myGradientImageType
                                            >  myFilterType;
            

  // Create a  Filter                                
  myFilterType::Pointer grfilter = myFilterType::New();
  myGFilterType::Pointer gfilter = myGFilterType::New();
  myGToMFilterType::Pointer gtomfilter = myGToMFilterType::New();

  // Connect the input images
  grfilter->SetInput( dshrink->GetOutput() ); 

  // Set sigma
  grfilter->SetSigma( 3.0 );

  myLaplacianFilterType::Pointer m_LFilter = myLaplacianFilterType::New();
  myGVFFilterType::Pointer m_GVFFilter = myGVFFilterType::New();

  m_GVFFilter->SetInput(gfilter->GetOutput());
  m_GVFFilter->SetLaplacianFilter(m_LFilter);
  m_GVFFilter->SetNoiseLevel(500);

  gtomfilter->SetInput(grfilter->GetOutput());

  gfilter->SetInput(gtomfilter->GetOutput());
  gfilter->Update();

  std::cout << "The gradient map created!" << std::endl;

//  the gvf is temproraily disabled because of the problem related with gradientimagefilter
//  m_GVFFilter->Update();

//  std::cout << "GVF created! " << std::endl;

////////////////////////////////////////////////////////////////////////////////////////
// construct the deformable mesh

  myMeshSource::Pointer m_bmmeshsource = myMeshSource::New();

  itk::Mesh<float, 3>::PointType meshcenter;

  meshcenter[0] = 50;
  meshcenter[1] = 50;
  meshcenter[2] = 0;

  m_bmmeshsource->SetCenter(meshcenter);

  m_bmmeshsource->SetResolutionX(1);
  m_bmmeshsource->SetResolutionY(100);

  DFilter::Pointer m_dfilter = DFilter::New();
  m_dfilter->SetInput(m_bmmeshsource->GetOutput());
  m_dfilter->SetGradient(gfilter->GetOutput());

  double2DVector m_stiff;
  myIndexType  modelcenter;
  m_stiff[0] = 0.001;
  m_stiff[1] = 1;
  modelcenter[0] = 50;
  modelcenter[1] = 50;
  m_dfilter->SetCenter(modelcenter);
  m_dfilter->SetPotential(biimg);

  m_dfilter->SetStiffness(m_stiff);
  m_dfilter->SetTimeStep(0.0185);
  m_dfilter->SetDistanceForGradient(1.0);
  m_dfilter->SetDistanceToStop(6.0);
  m_dfilter->SetResolution(100);
  std::cout << "Deformable mesh fitting...";
  m_dfilter->Update();
  
  DMesh::PointsContainerPointer     points;
  DMesh::PointsContainer::Iterator  pointsit;
  DMesh::CellsContainerPointer      cells;
  DMesh::CellsContainer::Iterator   cellsit;
  DMesh::PointType                  node;

  ///////////////////////////////////////////////////////////////////////
  // my local output
  /*
  FILE *outputfile = fopen(OUTPUTFILE, "wb");

  points = m_dfilter->GetOutput()->GetPoints();
  pointsit = points->Begin();

  inputfile = fopen(INPUTFILE, "rb");
  fread(ImageBuffer, 1, WIDTH*HEIGHT, inputfile);

  fclose(inputfile);

  int i = 0;
  while ( pointsit != points->End() ) {
    node = pointsit.Value();
    ImageBuffer[WIDTH*((int)node[1])+(int)(node[0])] = 255;
    ++pointsit;
    i++;
  }
  fwrite(ImageBuffer, 1, WIDTH*HEIGHT, outputfile);

  fclose(outputfile);
  */

  std::cout << "Mesh Source: " << m_bmmeshsource;

  delete [] ImageBuffer;

// All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}
