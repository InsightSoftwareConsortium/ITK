#include <itkImage.h>
#include <itkFirstDerivativeRecursiveGaussianImageFilter.h>
#include <itkSecondDerivativeRecursiveGaussianImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkSphereSource.h>
#include <itkBalloonForceFilter.h>
#include <itkPoint.h>
#include <itkMesh.h>
#include <time.h>

int WIDTH = 100;
int HEIGHT = 100;
int DEPTH = 1;
int SEEDX = 50;
int SEEDY = 50;

time_t btime,etime;
int main() 
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<float, myDimension>           myImageType;

  // Declare the types of the output images
  typedef itk::Image<unsigned short, myDimension>  outImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>       myIndexType;

  // Declare the type of the size 
  typedef itk::Size<myDimension>        mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>     myRegionType;

  // Declare the type of the Mesh
  typedef itk::Mesh<float>            DMesh;

  unsigned char *testImage = new unsigned char[WIDTH*HEIGHT*DEPTH];

  typedef itk::SphereSource<DMesh>            SphereSourceType;
  typedef itk::BalloonForceFilter<DMesh, DMesh>     BFilter;

  outImageType::Pointer ptimg=outImageType::New();
  outImageType::Pointer gdimg=outImageType::New();
  outImageType::Pointer outputimg = outImageType::New();

  outImageType::SizeType outsize={{WIDTH,HEIGHT,DEPTH}};
  outImageType::IndexType index=outImageType::IndexType::ZeroIndex;
  outImageType::RegionType outregion;
  outregion.SetSize(outsize);
  outregion.SetIndex(index);

  ptimg->SetLargestPossibleRegion( outregion );
  ptimg->SetBufferedRegion( outregion );
  ptimg->SetRequestedRegion( outregion );
  ptimg->Allocate();

  gdimg->SetLargestPossibleRegion( outregion );
  gdimg->SetBufferedRegion( outregion );
  gdimg->SetRequestedRegion( outregion );
  gdimg->Allocate();

  outputimg->SetLargestPossibleRegion( outregion );
  outputimg->SetBufferedRegion( outregion );
  outputimg->SetRequestedRegion( outregion );
  outputimg->Allocate();

  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();

  mySizeType size;
  size[0] = 100;
  size[1] = 100;
  size[2] = 1;

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
  while( !it.IsAtEnd()) {    
    it.Set(0.0);
    ++it;
  }

  size[0] = 60;
  size[1] = 60;
  size[2] = 1;

  start[0] = 20;
  start[1] = 20;
  start[2] = 0;

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

  // Declare the type for the  Smoothing  filter
  typedef itk::RecursiveGaussianImageFilter<
                                      myImageType,
                                      myImageType,
                                      float       >  mySmoothingFilterType;
            

  // Create a  Filter                                
  mySmoothingFilterType::Pointer filter = mySmoothingFilterType::New();


  // Connect the input images
  filter->SetInput( inputImage );
  filter->SetSigma( 2.0 );
  filter->SetDirection( 0 );  // apply along Z

  
  // Execute the filter
  std::cout << "Executing Smoothing filter...";
  filter->Update();


  size[0] = 100;
  size[1] = 100;
  size[2] = 1;

  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  // Create one iterator for an internal region
  region.SetSize( size );
  region.SetIndex( start );
  itk::ImageRegionIteratorWithIndex <myImageType> outit(filter->GetOutput(), region);
  outit.GoToBegin();
  it.GoToBegin();
  while( !outit.IsAtEnd()) {    
    it.Set(outit.Get());
    ++outit;
  ++it;
  }

  filter->SetDirection( 1 );
  filter->Update();

  std::cout << " Done !" << std::endl;

  outit.GoToBegin();
  int k = 0;
  while( !outit.IsAtEnd()) {    
    testImage[k] = (unsigned char)outit.Get();
  k++;
    ++outit;
  }

  k = 0;
  float grad;
  outit.GoToBegin();
  while( !outit.IsAtEnd() ) {
  if ((k >= WIDTH) && (k+WIDTH < WIDTH*HEIGHT)) { 
    grad = 0.5*sqrt((float)((testImage[k-1]-testImage[k+1])*(testImage[k-1]-testImage[k+1])+
    (testImage[k-WIDTH]-testImage[k+WIDTH])*(testImage[k-WIDTH]-testImage[k+WIDTH])));
    outit.Set(grad);
  } else {
    outit.Set(0);
  }
  ++outit;
  k++;
  }

// allocating the input image data.
  BFilter::Pointer m_bfilter = BFilter::New();
  SphereSourceType::Pointer m_spheresource = SphereSourceType::New();
  m_bfilter->SetInput(m_spheresource->GetOutput());

  itk::Point<float,3> center; center = SEEDX,SEEDY,0;
  m_spheresource->SetCenter(center);
  m_spheresource->SetResolutionX(1);
  m_spheresource->SetResolutionY(200);
  itk::Point<float,3> scale; scale = 10,10,1;
  m_spheresource->SetScale(scale);
  m_spheresource->Update();

  m_bfilter->SetCenter(SEEDX, SEEDY, 0);
  m_bfilter->SetNeighborRadius(5);
//  m_bfilter->SetStepThreshold1(20);
//  m_bfilter->SetStepThreshold2(30);

  itk::ImageRegionIteratorWithIndex <outImageType> ptit(ptimg, outregion);

  k = 0;
  ptit.GoToBegin();
  while( !ptit.IsAtEnd()) { 
    ptit.Set((unsigned short)0);
  k++;
    ++ptit;
  }

  size[0] = 56;
  size[1] = 56;
  size[2] = 1;

  start[0] = 22;
  start[1] = 22;
  start[2] = 0;

  // Create one iterator for an internal region
  outregion.SetSize( size );
  outregion.SetIndex( start );
  itk::ImageRegionIteratorWithIndex <outImageType> ptitb(ptimg, outregion);

  // Initialize the content the internal region
  ptitb.GoToBegin();
  while( !ptitb.IsAtEnd() ) 
  {
    ptitb.Set( 100 );
    ++ptitb;
  }

  outregion.SetSize(outsize);
  outregion.SetIndex(index);

  itk::ImageRegionIteratorWithIndex <outImageType> gdit(gdimg, outregion);

  k = 0;
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
  m_bfilter->SetResolution(1, 200, 1);
  m_bfilter->Initialize();
  m_bfilter->SetStiffnessMatrix();

  for (k=0; k<30; k++) {
    m_bfilter->ComputeNormals();
    if (k > 20) m_bfilter->GradientFit();
    else m_bfilter->ComputeForce();
    m_bfilter->ComputeDt();
    m_bfilter->Advance();
    m_bfilter->NodesRearrange();
  m_bfilter->ComputeOutput();
  }
  
//  time(&btime);
//  m_bfilter->Update();
//  time(&etime);

  std::cout<<"Finished: "<<etime-btime<<" seconds."<<std::endl;
  
  // All objects should be automatically destroyed at this point
  return 0;

}




