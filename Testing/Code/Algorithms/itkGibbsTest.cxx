#include <iostream>
#include <string>
#include <math.h>

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include "itkImage.h"
#include "itkScalar.h"
#include "itkVector.h"
#include "vnl/vnl_matrix_fixed.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkGaussianSupervisedClassifier.h"
#include "itkGibbsPriorFilter.h"
#include "itkGibbsPriorFilter.cpp"

#define   IMGWIDTH            5
#define   IMGHEIGHT           5
#define   NFRAMES             1
#define   NUMBANDS            1  
#define   NDIMENSION          3
#define   NUM_CLASSES         3
#define   MAX_NUM_ITER       10

//using namespace itk;

int main(){
  typedef itk::Image<itk::Vector<int,NUMBANDS>,NDIMENSION> VecImageType; 

  VecImageType::Pointer vecImage = VecImageType::New();

  VecImageType::SizeType vecImgSize = { IMGWIDTH , IMGHEIGHT, NFRAMES };

  VecImageType::IndexType index = VecImageType::IndexType::ZeroIndex;
  VecImageType::RegionType region;

  region.SetSize( vecImgSize );
  region.SetIndex( index );

  vecImage->SetLargestPossibleRegion( region );
  vecImage->SetBufferedRegion( region );
  vecImage->Allocate();

  // setup the iterators
  typedef VecImageType::PixelType::VectorType VecPixelType;

  enum { VecImageDimension = VecImageType::ImageDimension };
  typedef itk::SimpleImageRegionIterator< VecImageType > VecIterator;

  VecIterator outIt( vecImage, vecImage->GetBufferedRegion() );
  outIt.Begin();

  //Set up the vector to store the image  data
  typedef VecImageType::PixelType     DataVector;
  DataVector   dblVec; 

  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------
  //Slice 1
  //Vector 1
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 2
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 3
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 4
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 5
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 6
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 7
  dblVec[0] = 10; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 8
  dblVec[0] = 11; 
  outIt.Set(dblVec); 
  ++outIt;
  
  //Vector 9
  dblVec[0] = 10; 
  outIt.Set(dblVec); 
  ++outIt;
  
  //Vector 10
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;
  
  //Vector 11
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;
  
  //Vector 12
  dblVec[0] = 10; 
  outIt.Set(dblVec); 
  ++outIt;
  
  //Vector 13
  dblVec[0] = 9; 
  outIt.Set(dblVec); 
  ++outIt; 
  
  //Vector 14
  dblVec[0] = 10; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 15
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 16
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 17
  dblVec[0] = 10; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 18
  dblVec[0] = 11; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 19
  dblVec[0] = 10; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 20
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 21
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 22
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 23
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 24
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;

  //Vector 25
  dblVec[0] = 0; 
  outIt.Set(dblVec); 
  ++outIt;

  //---------------------------------------------------------------
  //Generate the training data
  //---------------------------------------------------------------
  typedef itk::Image<unsigned short,NDIMENSION> ClassImageType; 
  ClassImageType::Pointer classImage  = ClassImageType::New();

  ClassImageType::SizeType classImgSize = { IMGWIDTH , IMGHEIGHT, NFRAMES };

  ClassImageType::IndexType classindex = ClassImageType::IndexType::ZeroIndex;
  ClassImageType::RegionType classregion;

  classregion.SetSize( classImgSize );
  classregion.SetIndex( classindex );

  classImage->SetLargestPossibleRegion( classregion );
  classImage->SetBufferedRegion( classregion );
  classImage->Allocate();

  // setup the iterators
  typedef ClassImageType::PixelType ClassImagePixelType;

  unsigned int ClassImageDimension = NDIMENSION;

  typedef  itk::SimpleImageRegionIterator<ClassImageType>  ClassImageIterator;

  ClassImageIterator classoutIt( classImage, classImage->GetBufferedRegion() );

  classoutIt.Begin();



  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------
  //Slice 1
  //Pixel no. 1
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 2 
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 3
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 4
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 5
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 6 
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 7
  classoutIt.Set( 2 );
  ++classoutIt;

  //Pixel no. 8
  classoutIt.Set( 2 );
  ++classoutIt;
  
  //Pixel no. 9
  classoutIt.Set( 2 );
  ++classoutIt;

  //Pixel no. 10 
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 11
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 12
  classoutIt.Set( 2 );
  ++classoutIt;

  //Pixel no. 13
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 14
  classoutIt.Set( 2);
  ++classoutIt;

  //Pixel no. 15
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 16
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 17
  classoutIt.Set( 2 );
  ++classoutIt;

  //Pixel no. 18 
  classoutIt.Set( 2 );
  ++classoutIt;

  //Pixel no. 19
  classoutIt.Set( 2 );
  ++classoutIt;

  //Pixel no. 20
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 21
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 22 
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 23
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 24
  classoutIt.Set( 1 );
  ++classoutIt;  

  //Pixel no. 25
  classoutIt.Set( 1 );
  ++classoutIt;

  //----------------------------------------------------------------------
  // Test code for the supervised classifier algorithm
  //----------------------------------------------------------------------

  //---------------------------------------------------------------------
  // Multiband data is now available in the right format
  //---------------------------------------------------------------------
  typedef 
	itk::Classifier<VecImageType,ClassImageType>::Pointer 
	  ClassifierType;

  //Instantiate the classifier to be used
  typedef itk::GaussianSupervisedClassifier<VecImageType,ClassImageType> 
    GaussianSupervisedClassifierType;

  GaussianSupervisedClassifierType::Pointer 
	  myGaussianClassifier = GaussianSupervisedClassifierType::New();

  //Set the Gibbs Prior labeller
  typedef itk::GibbsPriorFilter<VecImageType,ClassImageType> GibbsPriorFilterType;
  GibbsPriorFilterType::Pointer applyGibbsImageFilter = GibbsPriorFilterType::New();

  // Set the MRF labeller parameters
  applyGibbsImageFilter->SetNumClasses(NUM_CLASSES);
  applyGibbsImageFilter->SetMaxNumIter(MAX_NUM_ITER);
  applyGibbsImageFilter->SetErrorTollerance(0.00);
 
  applyGibbsImageFilter->SetInput(vecImage);
  applyGibbsImageFilter
    ->SetClassifier((ClassifierType) myGaussianClassifier ); 

  //Since a suvervised classifier is used, it requires a training image
  applyGibbsImageFilter->SetTrainingImage(classImage);  
  
  //Kick off the MRF labeller function
  applyGibbsImageFilter->Update();
  
  ClassImageType::Pointer  outClassImage = applyGibbsImageFilter->GetOutput();

  //Print the mrf labelled image
  ClassImageIterator labeloutIt( outClassImage, outClassImage->GetBufferedRegion() );
  labeloutIt.Begin();

  //Verify if the results were as per expectation
  bool passTest = true;

  //Loop through the data set
  while( ! labeloutIt.IsAtEnd() )
  {
	//vector1
	int classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "* ";
//      break;
    } else std::cout<< "o ";
    ++labeloutIt;
	
	//vector2
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "* ";
//      break;
    } else std::cout<< "o ";
    ++labeloutIt;

	//vector3
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "* ";
//      break;
    } else std::cout<< "o ";
    ++labeloutIt;

	//vector4
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "* ";
//      break;
    } else std::cout<< "o ";
    ++labeloutIt;

    //vector5
	classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "*" << std::endl;
//      break;
    } else std::cout<< "o" << std::endl;
    ++labeloutIt;

	//vector6
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "* ";
//      break;
    } else std::cout<< "o ";
    ++labeloutIt;

	//vector7
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 2)
    {
      passTest = false;
	  std::cout<< "o ";
//      break;
    } else std::cout<< "* ";
    ++labeloutIt;

	//vector8
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 2)
    {
      passTest = false;
	  std::cout<< "o ";
//      break;
    } else std::cout<< "* ";
    ++labeloutIt;

	//vector9
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 2)
    {
      passTest = false;
	  std::cout<< "o ";
//      break;
    } else std::cout<< "* ";
    ++labeloutIt;

	//vector10
	classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "*" << std::endl;
//      break;
    } else std::cout<< "o" <<std::endl;
    ++labeloutIt;

	//vector11
	classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "* ";
//      break;
    } else std::cout<< "o ";
    ++labeloutIt;

	//vector12
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 2)
    {
      passTest = false;
	  std::cout<< "o ";
//      break;
    } else std::cout<< "* ";
    ++labeloutIt;

	//vector13
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 2)
    {
      passTest = false;
	  std::cout<< "o ";
//      break;
    } else std::cout<< "* ";
    ++labeloutIt;

	//vector14
	classIndex = (int) labeloutIt.Get();
    if (classIndex != 2)
    {
      passTest = false;
	  std::cout<< "o ";
//      break;
    } else std::cout<< "* ";
    ++labeloutIt;

	//vector15
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "*" << std::endl;
//      break;
    } else std::cout<< "o" <<std::endl;
    ++labeloutIt;

	//vector16
	classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "* ";
//      break;
    } else std::cout<< "o ";
    ++labeloutIt;

	//vector17
	classIndex = (int) labeloutIt.Get();
    if (classIndex != 2)
    {
      passTest = false;
	  std::cout<< "o ";
//      break;
    } else std::cout<< "* ";
    ++labeloutIt;
	
	//vector18
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 2)
    {
      passTest = false;
	  std::cout<< "o ";
//      break;
    } else std::cout<< "* ";
    ++labeloutIt;

	//vector19
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 2)
    {
      passTest = false;
	  std::cout<< "o ";
//      break;
    } else std::cout<< "* ";
    ++labeloutIt;

	//vector20
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "*" << std::endl;
//      break;
    } else std::cout<< "o" <<std::endl;
    ++labeloutIt;

	//vector21
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "* ";
//      break;
    } else std::cout<< "o ";
    ++labeloutIt;

	//vector22
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "* ";
//      break;
    } else std::cout<< "o ";
    ++labeloutIt;

	//vector23
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "* ";
//      break;
    } else std::cout<< "o ";
    ++labeloutIt;

	//vector24
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "* ";
//      break;
    } else std::cout<< "o ";
    ++labeloutIt;

	//vector25
    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
	  std::cout<< "*" << std::endl;
//      break;
    } else std::cout<< "o" <<std::endl;
    ++labeloutIt;
  }//end while
  
  outIt.Begin();

  //Loop through the data set
  while( ! outIt.IsAtEnd() )
  {
    int classIndex = outIt.Get()[0];
	std::cout<< classIndex <<" ";
    ++outIt;
  }//end while

  std::cout<< std::endl;
  if( passTest == true ) 
    std::cout<< "Gibbs Prior Test Passed" << std::endl;
  else 
    std::cout<< "Gibbs Prior Test failed" << std::endl;

  return 0;
}