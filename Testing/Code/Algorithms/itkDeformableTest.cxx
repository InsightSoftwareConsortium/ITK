#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkImage.h"
#include "itkVector.h"
#include "vnl/vnl_matrix_fixed.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkGaussianSupervisedClassifier.h"
#include "itkGibbsPriorFilter.h"
#include "itkDeformableMesh.h"
#include "itkBalloonForceFilter.h"
#include "itkTriangleCell.h"
#include "itkImage.h"
#include "itkDefaultStaticMeshTraits.h"
#include "itkSimpleImageRegionIterator.h"
#include <iostream>
#include <string>
#include <math.h>

#define   IMGWIDTH            20
#define   IMGHEIGHT           20
#define   NFRAMES             1
#define   NUMBANDS            1  
#define   NDIMENSION          3
#define   NUM_CLASSES         3
#define   MAX_NUM_ITER        20

typedef itk::DeformableMesh<float>  DMesh;
typedef itk::Mesh<float>  MyMesh;
typedef itk::BalloonForceFilter<DMesh, DMesh> BFilter;
typedef itk::TriangleCell<DMesh::PixelType, DMesh::CellTraits>	   TriCell;

int main(){
  	unsigned short TestImage [400]={
297,277,317,289,300,312,306,283,282,308,308,342,335,325,315,300,304,318,307,308,

319,276,311,282,309,273,308,277,296,313,308,333,322,317,302,330,339,340,325,315,

272,316,296,299,298,310,271,319,315,280,338,342,349,349,330,319,313,314,342,301,

274,274,312,282,277,303,313,300,275,292,341,336,324,310,337,323,322,347,337,305,

296,272,304,304,281,304,302,284,315,270,325,349,337,317,308,332,324,303,334,325,

291,272,289,317,289,310,305,316,292,307,307,343,341,329,309,308,340,323,307,325,

274,286,282,291,270,296,274,288,274,275,341,301,325,333,321,305,347,346,327,317,

282,315,270,314,290,304,297,304,309,290,309,338,341,319,325,344,301,349,328,302,

314,289,296,270,274,277,317,280,278,285,315,347,314,316,307,336,341,335,330,337,

281,291,317,317,302,304,272,277,318,319,305,322,337,334,327,303,321,310,334,314,

321,311,328,326,331,308,325,348,334,346,309,316,308,349,322,349,304,331,304,321,

346,302,344,314,311,338,320,310,331,330,322,323,329,331,342,341,331,336,328,318,

309,336,327,345,312,309,330,334,329,317,324,304,337,330,331,334,340,307,328,343,

345,330,336,302,333,348,315,328,315,308,305,343,342,337,307,316,303,303,332,341,

327,322,320,314,323,325,307,316,336,315,341,347,343,336,315,347,306,303,339,326,

330,347,303,343,332,316,305,325,311,314,345,327,333,305,324,318,324,339,325,319,

334,326,330,319,300,335,305,331,343,324,337,324,319,339,327,317,347,331,308,318,

306,337,347,330,301,316,302,331,306,342,343,329,336,342,300,306,335,330,310,303,

308,331,317,315,318,333,340,340,326,330,339,345,307,331,320,312,306,342,303,321,

328,315,327,311,315,305,340,306,314,339,344,339,337,330,318,342,311,343,311,312
};
  unsigned short TestImage1[400];
  
  typedef itk::Image<itk::Vector<unsigned short,NUMBANDS>,NDIMENSION> VecImageType; 

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
  int i = 0;
  while ( !outIt.IsAtEnd() ) { 
    dblVec[0] = TestImage[i]; 
	outIt.Set(dblVec); 
	++outIt;
	i++;
  }

  //---------------------------------------------------------------
  //Generate the initial training data
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

  i = 0;
  while ( !classoutIt.IsAtEnd() ) {
	classoutIt.Set( 0 );
	if ( (i%IMGWIDTH<7) && (i%IMGWIDTH>2) && 
		(i/IMGWIDTH<7) && (i/IMGWIDTH>2)) {
	  classoutIt.Set( 1 );
	}
	if ( (i%IMGWIDTH<17) && (i%IMGWIDTH>12) && 
		(i/IMGWIDTH<17) && (i/IMGWIDTH>12)) {
	  classoutIt.Set( 2 );
	}
	++classoutIt;
	i++;
  }

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

  //Set the Gibbs Prior labeller parameters
  applyGibbsImageFilter->SetNumClasses(NUM_CLASSES);
  applyGibbsImageFilter->SetMaxNumIter(MAX_NUM_ITER);
  applyGibbsImageFilter->SetErrorTollerance(0.00);
  applyGibbsImageFilter->SetClusterSize(10);
  applyGibbsImageFilter->SetBoundaryGradient(6);
  applyGibbsImageFilter->SetObjectLabel(1);
 
  applyGibbsImageFilter->SetInput(vecImage);
  applyGibbsImageFilter
    ->SetClassifier((ClassifierType) myGaussianClassifier ); 

  //Since a suvervised classifier is used, it requires a training image
  applyGibbsImageFilter->SetTrainingImage(classImage);  
  
  //Kick off the Gibbs Prior labeller function
  applyGibbsImageFilter->Update();
  
  ClassImageType::Pointer  outClassImage = applyGibbsImageFilter->GetOutput();

  //Print the Gibbs Prior labelled image
  ClassImageIterator labeloutIt( outClassImage, outClassImage->GetBufferedRegion() );
  labeloutIt.Begin();

  i = 0;
  while ( !labeloutIt.IsAtEnd() ) {
	TestImage1[i] = labeloutIt.Get()*32000;
	i++;
	++labeloutIt;
  }

//disable all the file output, please enable it when you test the code locally
/*
  FILE *middle=fopen("middle.raw", "wb");
  fwrite(TestImage1, 2, IMGWIDTH*IMGHEIGHT, middle);
  fclose(middle);
*/
  //---------------------------------------------------------------------
  // Define the deformable Mesh
  //---------------------------------------------------------------------
  DMesh::Pointer m_Mesh(DMesh::New());
  BFilter::Pointer m_Filter = BFilter::New();
  DMesh::Pointer force(DMesh::New());
  DMesh::Pointer displace(DMesh::New());
  DMesh::Pointer derive(DMesh::New());
  DMesh::Pointer normal(DMesh::New());
  DMesh::Pointer location(DMesh::New());
  
  m_Mesh->SetDefault();
  m_Mesh->SetResolution(1, 40);
  m_Mesh->SetScale(2.0, 2.0, 1.0);
  m_Mesh->SetCenter(5, 5, 0);
  m_Mesh->Allocate();

  //---------------------------------------------------------------------
  // Define the output image
  //---------------------------------------------------------------------
  typedef itk::Image<unsigned short,3> ObjectImageType;

  ObjectImageType::Pointer outputimg=ObjectImageType::New();
  ObjectImageType::SizeType size={{IMGHEIGHT,IMGWIDTH,1}};
  ObjectImageType::IndexType outindex=ObjectImageType::IndexType::ZeroIndex;
  ObjectImageType::RegionType outregion;

  outregion.SetSize(size);
  outregion.SetIndex(outindex);

  outputimg->SetLargestPossibleRegion( outregion );
  outputimg->SetBufferedRegion( outregion );
  outputimg->SetRequestedRegion( outregion );
  outputimg->Allocate();
  	
  itk::SimpleImageRegionIterator <ObjectImageType> it(outputimg, outregion);
  it.Begin();	
  while( !it.IsAtEnd()) {	
	it.Set(0);	
	++it;
  }
  
  //---------------------------------------------------------------------
  // Define the Balloon Force filter
  //---------------------------------------------------------------------
  m_Filter->SetInput(m_Mesh);
  m_Filter->SetPotential(outClassImage);
  m_Filter->SetImageOutput(outputimg);
  m_Filter->SetResolution(1, 40, 1);
  m_Filter->SetCenter(5, 5, 0);
  m_Filter->SetLocations(location);
  m_Filter->SetForces(force);
  m_Filter->SetNormals(normal);
  m_Filter->SetDisplacements(displace);
  m_Filter->SetDerives(derive);
  m_Filter->Initialize();
  m_Filter->SetStiffnessMatrix();

  int k;
  for (k=0; k<85; k++) {
	m_Filter->ComputeForce();
	m_Filter->ComputeDt();
	m_Filter->Advance();
	if (k%20==0) m_Filter->GapSearch();
  }
  m_Filter->GapSearch();
  m_Filter->ComputeOutput();

  int p =0, j;
  MyMesh::PointType ds;
  MyMesh::PointsContainerPointer      myoutput = m_Filter->GetOutput()->GetPoints();
  MyMesh::PointsContainer::Iterator   m_output = myoutput->Begin();
  while( m_output != myoutput->End() ) {
	ds = m_output.Value();
	++m_output;
	i = (int)ds[0];
	j = (int)ds[1];
	k = IMGWIDTH*j + i;
//	std::cout<< i << ", " << j <<std::endl;;
	TestImage1[k] = 0;
	p++;
  }
/*
  FILE *output=fopen("new1.raw", "wb");
  fwrite(TestImage1, 2, IMGWIDTH*IMGHEIGHT, output);
  fclose(output);
*/
  it.Begin();
  classoutIt.Begin();
  while( !it.IsAtEnd()) {	
    if (classoutIt.Get() == 1) classoutIt.Set(0);
	if (it.Get() == 1) classoutIt.Set(1);	
	++it;
	++classoutIt;
  }

  outIt.Begin();
  i = 0;
  while ( !outIt.IsAtEnd() ) { 
    dblVec[0] = TestImage[i]; 
	outIt.Set(dblVec); 
	++outIt;
	i++;
  }

// here is a simple example of send the reulst of deformable model back
// to refine the gibbs prior model parameters.   
  applyGibbsImageFilter->Advance();

  labeloutIt.Begin();

  i = 0;
  while ( !labeloutIt.IsAtEnd() ) {
	TestImage1[i] = labeloutIt.Get()*32000;
	i++;
	++labeloutIt;
  }
/*
  middle=fopen("new2.raw", "wb");
  fwrite(TestImage1, 2, IMGWIDTH*IMGHEIGHT, middle);
  fclose(middle);
*/
  return 0;

}

