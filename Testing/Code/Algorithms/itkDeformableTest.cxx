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
#include <itkImage.h>
#include <itkFirstDerivativeRecursiveGaussianImageFilter.h>
#include <itkSecondDerivativeRecursiveGaussianImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkSphereSource.h>
#include <itkDeformableMesh3DFilter.h>
#include <itkMesh.h>
//#include <time.h>
#include <itkGradientRecursiveGaussianImageFilter.h>
#include <itkCovariantVector.h>


int WIDTH = 50;
int HEIGHT = 50;
int DEPTH = 50;

/* seed point */
int SEEDX = 25;//83;
int SEEDY = 25;//92;
int SEEDZ = 25;//3;

int main() 
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<double, myDimension>           myImageType;

  // Declare the types of the output images
  typedef itk::Image<unsigned short, myDimension>  outImageType;

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
  typedef itk::Image<myGradientType, myDimension>  myGradientImageType;


//  unsigned short *TestImage = new unsigned short[WIDTH*HEIGHT];
  unsigned char  *Test = new unsigned char[WIDTH*HEIGHT*DEPTH];


  typedef itk::SphereSource<DMesh>                   SphereSourceType;
  typedef itk::DeformableMesh3DFilter<DMesh, DMesh>  BFilter;
//  typedef itk::SphereSource<DMesh>            SphereSourceType;
//  typedef itk::BalloonForceFilter<DMesh, DMesh>     BFilter;

  outImageType::Pointer ptimg=outImageType::New();
  myGradientImageType::Pointer gdimg=myGradientImageType::New();
  outImageType::Pointer outputimg = outImageType::New();
//  outImageType::Pointer gdimg=outImageType::New();
//  outImageType::Pointer outputimg = outImageType::New();

  outImageType::SizeType outsize={{WIDTH,HEIGHT,DEPTH}};
  outImageType::IndexType index=outImageType::IndexType::ZeroIndex;
  outImageType::RegionType outregion;
  outregion.SetSize(outsize);
  outregion.SetIndex(index);

  myGradientImageType::SizeType gdsize={{WIDTH,HEIGHT,DEPTH}};
  myGradientImageType::IndexType gdindex=myGradientImageType::IndexType::ZeroIndex;
  myGradientImageType::RegionType gdregion;
  gdregion.SetSize(gdsize);
  gdregion.SetIndex(gdindex);
  
  ptimg->SetLargestPossibleRegion( outregion );
  ptimg->SetBufferedRegion( outregion );
  ptimg->SetRequestedRegion( outregion );
  ptimg->Allocate();

  gdimg->SetLargestPossibleRegion( gdregion );
  gdimg->SetBufferedRegion( gdregion );
  gdimg->SetRequestedRegion( gdregion );
  gdimg->Allocate();

  outputimg->SetLargestPossibleRegion( outregion );
  outputimg->SetBufferedRegion( outregion );
  outputimg->SetRequestedRegion( outregion );
  outputimg->Allocate();

  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();

  mySizeType size={{WIDTH,HEIGHT,DEPTH}};
  myIndexType start;
  start = myIndexType::ZeroIndex;

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

//  for local testing on image files (256*256*1 RGB)
/*  unsigned short outImage[WIDTH*HEIGHT*DEPTH];

  FILE *input;

  input = fopen(INFILE, "rb");

  double ss;
  int k=0;
  for (int i=0; i<DEPTH; i++) {
    fread(TestImage, 2, WIDTH*HEIGHT, input);
    k = 0;
    while( k < WIDTH*HEIGHT ) {    
      ss=(double)TestImage[k];
      it.Set(ss);
      k++;
      ++it;
    }
  }
  fclose(input);
*/
  while( !it.IsAtEnd() ) 
  {
    it.Set( 0.0 );
    ++it;
  }

  size[0] = 30;
  size[1] = 30;
  size[2] = 30;

  start[0] = 10;
  start[1] = 10;
  start[2] = 10;

  // Create one iterator for an internal region
  region.SetSize( size );
  region.SetIndex( start );
  itk::ImageRegionIteratorWithIndex <myImageType> itb( inputImage, region );

  // Initialize the content the internal region
  while( !itb.IsAtEnd() ) 
  {
    itb.Set( 100.0 );
    ++itb;
  }

  typedef itk::GradientRecursiveGaussianImageFilter<
                                            myImageType,
                                            myGradientImageType,
                                            double       >  myFilterType;
            
  // Create a  Filter                                
  myFilterType::Pointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput( inputImage ); 

  // Set sigma
  filter->SetSigma( 2.0 );
  
  // Execute the filter
  filter->Update();

  itk::ImageRegionIteratorWithIndex <myGradientImageType> outit(filter->GetOutput(), gdregion);
  outit.GoToBegin();

  std::cout << " Done !" << std::endl;

/*

  FILE *grgoutput=fopen("../../../../insight/local_copy/Jaw_grg_uint8.raw", "wb");  

  outit.GoToBegin();
  k = 0;
  while( !outit.IsAtEnd()) {  
    TestImage[k] = (unsigned char)(sqrt(outit.Get()[0]*outit.Get()[0]+outit.Get()[1]*outit.Get()[1]+outit.Get()[2]*outit.Get()[2]));
    k++;
    ++outit;
  }

  fwrite(TestImage, 1, HEIGHT*WIDTH*DEPTH, grgoutput);
  fclose(grgoutput);

  k = 0;
  double grad;
  outit.GoToBegin();
  while( !outit.IsAtEnd() ) {
  if ((k >= WIDTH) && (k+WIDTH < WIDTH*HEIGHT)) { 
    grad = 0.5*sqrt((double)((testImage[k-1]-testImage[k+1])*(testImage[k-1]-testImage[k+1])+
    (testImage[k-WIDTH]-testImage[k+WIDTH])*(testImage[k-WIDTH]-testImage[k+WIDTH])));
    outit.Set(grad);
  } else {
    outit.Set(0);
  }

*/

// allocating the input image data.
  BFilter::Pointer m_bfilter = BFilter::New();
  SphereSourceType::Pointer m_spheresource = SphereSourceType::New();
  m_bfilter->SetInput(m_spheresource->GetOutput());

  OPointType m_spherecenter;
  OPointType m_scale;
  m_spherecenter[0] = (double) SEEDX;
  m_spherecenter[1] = (double) SEEDY;
  m_spherecenter[2] = (double) SEEDZ;
  m_scale[0] = 3.0;
  m_scale[1] = 3.0;
  m_scale[2] = 3.0;
  m_spheresource->SetCenter(m_spherecenter);
  m_spheresource->SetResolutionX(5);
  m_spheresource->SetResolutionY(30);
  m_spheresource->SetSquareness1(0.5);
  m_spheresource->SetSquareness2(0.5);
  m_spheresource->SetScale(m_scale);
  m_spheresource->Update();

  myIndexType m_center={{SEEDX, SEEDY, SEEDZ}};
  m_bfilter->SetCenter(m_center);
  m_bfilter->SetNeighborRadius(5);
  m_bfilter->SetStiffnessV(0.00001);
  m_bfilter->SetStiffnessH(0.04);
  m_bfilter->SetTimeStep(0.001);
  m_bfilter->SetStepThreshold1(20);
  m_bfilter->SetStepThreshold2(30);
  itk::ImageRegionIteratorWithIndex <outImageType> ptit(ptimg, outregion);

// for local testing
/*
  input = fopen(PTFILE, "rb");
  k = 0;
  ptit.GoToBegin();
  for (int i=0; i<DEPTH; i++) {
    fread(TestImage, 2, WIDTH*HEIGHT, input);
    k = 0;
    while( k < WIDTH*HEIGHT ) {
      ptit.Set(TestImage[k]);
      k++;
      ++ptit;
    }
  }
  fclose(input);

  FILE *ptoutput=fopen("../../../../insight/local_copy/Jaw_pt_uint8.raw", "wb");  

  ptit.GoToBegin();
  k = 0;
  while( !ptit.IsAtEnd()) {    
    TestImage[k] = (unsigned char)(ptit.Get());
    k++;
    ++ptit;
  }

  fwrite(TestImage, 1, HEIGHT*WIDTH*DEPTH, ptoutput);
  fclose(ptoutput);
*/  
  ptit.GoToBegin();
  while( !ptit.IsAtEnd() ) 
  {
    ptit.Set( 0 );
    ++ptit;
  }

  outsize[0] = 30;
  outsize[1] = 30;
  outsize[2] = 30;

  index[0] = 10;
  index[1] = 10;
  index[2] = 10;

  // Create one iterator for an internal region
  outregion.SetSize( outsize );
  outregion.SetIndex( index );
  itk::ImageRegionIteratorWithIndex <outImageType> ptitb(ptimg, outregion);

  // Initialize the content the internal region
  ptitb.GoToBegin();
  while( !ptitb.IsAtEnd() ) 
  {
    ptitb.Set( 100 );
    ++ptitb;
  }

  outsize[0] = 50;
  outsize[1] = 50;
  outsize[2] = 50;

  index[0] = 0;
  index[1] = 0;
  index[2] = 0;

  outregion.SetSize(outsize);
  outregion.SetIndex(index);

  itk::ImageRegionIteratorWithIndex <myGradientImageType> gdit(gdimg, gdregion);

  int k = 0;
  gdit.GoToBegin();
  outit.GoToBegin();
  while( !gdit.IsAtEnd()) { 
    gdit.Set(outit.Get());
    k++;
    ++gdit;
    ++outit;
  }

  m_bfilter->SetPotential(ptimg);
  m_bfilter->SetGradient(gdimg);
  m_bfilter->SetImageOutput(outputimg);
  m_bfilter->SetXResolution(5);
  m_bfilter->SetYResolution(30);
  m_bfilter->SetZResolution(1);
  m_bfilter->Initialize();
  m_bfilter->SetStiffnessMatrix();

  DMesh::PointsContainerPointer     points;
  DMesh::PointsContainer::Iterator  pointsit;
  DMesh::CellsContainerPointer      cells;
  DMesh::CellsContainer::Iterator   cellsit;
  DMesh::PointType                  node;
//  const unsigned long *tp;

  for (k=0; k<50; k++) {
    m_bfilter->ComputeNormals();

    m_bfilter->ComputeForce();
//    m_bfilter->ComputeDt();
//    m_bfilter->InitialFit();
    m_bfilter->Advance();

//    if (k == 20) m_bfilter->NodeAddition();
    m_bfilter->NodesRearrange();
  
    m_bfilter->ComputeOutput();
  
  }
  
  points = m_bfilter->GetOutput()->GetPoints();
  pointsit = points->Begin();
  cells = m_bfilter->GetOutput()->GetCells();
  cellsit = cells->Begin();

  int i = 0;
  while ( i < HEIGHT*WIDTH*DEPTH ) {
    Test[i] = 0;
    i++;
  }

  i = 0;
  while ( pointsit != points->End() ) {
    node = pointsit.Value();
    Test[((int)(node[2]))*WIDTH*HEIGHT+((int)(node[1]))*WIDTH+((int)(node[0]))]=255;
    ++pointsit;
    i++;
  }
  return 0;

}




