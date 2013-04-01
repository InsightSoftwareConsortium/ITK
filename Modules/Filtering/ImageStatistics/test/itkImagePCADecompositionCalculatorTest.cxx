/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

// Insight classes
#include "itkLightProcessObject.h"
#include "itkTextOutput.h"

#include "itkImagePCADecompositionCalculator.h"

// class to support progress feeback


class ShowProgressObject
{
public:
  ShowProgressObject(itk::LightProcessObject * o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::LightProcessObject::Pointer m_Process;
};


int itkImagePCADecompositionCalculatorTest(int, char* [] )
{

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  //Data definitions
  const unsigned int  IMGWIDTH         =  2;
  const unsigned int  IMGHEIGHT        =  2;
  const unsigned int  NDIMENSION       =  2;


  //------------------------------------------------------
  //Create 3 simple test images with
  //------------------------------------------------------
  typedef itk::Image<double,NDIMENSION> InputImageType;

  typedef
    itk::ImageRegionIterator< InputImageType > InputImageIterator;


  InputImageType::Pointer image1 = InputImageType::New();

  InputImageType::Pointer image2 = InputImageType::New();

  InputImageType::Pointer image3 = InputImageType::New();

  InputImageType::Pointer image4 = InputImageType::New();

  InputImageType::Pointer image5 = InputImageType::New();

  InputImageType::Pointer image6 = InputImageType::New();

  InputImageType::Pointer image7 = InputImageType::New();

  InputImageType::Pointer image8 = InputImageType::New();

  InputImageType::SizeType inputImageSize = {{ IMGWIDTH, IMGHEIGHT }};

  InputImageType::IndexType index;
  index.Fill(0);
  InputImageType::RegionType region;

  region.SetSize( inputImageSize );
  region.SetIndex( index );

  //--------------------------------------------------------------------------
  // Set up Image 1 first
  //--------------------------------------------------------------------------

  image1->SetRegions( region );
  image1->Allocate();

  // setup the iterators
  InputImageIterator image1It( image1, image1->GetBufferedRegion() );

  //--------------------------------------------------------------------------
  // Set up Image 2 first
  //--------------------------------------------------------------------------

  image2->SetRegions( region );
  image2->Allocate();

  // setup the iterators
  InputImageIterator image2It( image2, image2->GetBufferedRegion() );

  //--------------------------------------------------------------------------
  // Set up Image 3 first
  //--------------------------------------------------------------------------

  image3->SetRegions( region );
  image3->Allocate();

  // setup the iterators
  InputImageIterator image3It( image3, image3->GetBufferedRegion() );

  //--------------------------------------------------------------------------
  // Set up Image 4 first
  //--------------------------------------------------------------------------

  image4->SetRegions( region );
  image4->Allocate();

  // setup the iterators
  InputImageIterator image4It( image4, image4->GetBufferedRegion() );

  //--------------------------------------------------------------------------
  // Set up Image 5 first
  //--------------------------------------------------------------------------

  image5->SetRegions( region );
  image5->Allocate();

  // setup the iterators
  InputImageIterator image5It( image5, image5->GetBufferedRegion() );

  //--------------------------------------------------------------------------
  // Set up Image 6 first
  //--------------------------------------------------------------------------

  image6->SetRegions( region );
  image6->Allocate();

  // setup the iterators
  InputImageIterator image6It( image6, image6->GetBufferedRegion() );

  //--------------------------------------------------------------------------
  // Set up Image 7 first
  //--------------------------------------------------------------------------

  image7->SetRegions( region );
  image7->Allocate();

  // setup the iterators
  InputImageIterator image7It( image7, image7->GetBufferedRegion() );

  //--------------------------------------------------------------------------
  // Set up Image 8 first
  //--------------------------------------------------------------------------

  image8->SetRegions( region );
  image8->Allocate();

  // setup the iterators
  InputImageIterator image8It( image8, image8->GetBufferedRegion() );


  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------
  // The first two vectors are the first two principal components of the data:
  // [1 1 1 1] , [2 0 0 2], [0 3 3 0]
  // The second two vectors are some of those data, which we will project down
  // to the PC-space
  // The next three vectors are a new basis set to project down into, so we can
  // test changing bases mid-stream.
  // The last image is the "mean image," here set to zero.

  //Image no. 1
  image1It.Set( -0.3853 ); ++image1It;
  image1It.Set( 0.5929 ); ++image1It;
  image1It.Set( 0.5929 ); ++image1It;
  image1It.Set( -0.3853 ); ++image1It;

  //Image no. 2
  image2It.Set( -0.5929 ); ++image2It;
  image2It.Set( -0.3853 ); ++image2It;
  image2It.Set( -0.3853 ); ++image2It;
  image2It.Set( -0.5929 ); ++image2It;

  //Image no. 3
  image3It.Set( 2 ); ++image3It;
  image3It.Set( 0 ); ++image3It;
  image3It.Set( 0 ); ++image3It;
  image3It.Set( 2 ); ++image3It;

  //Image no. 4
  image4It.Set( 0 ); ++image4It;
  image4It.Set( 3 ); ++image4It;
  image4It.Set( 3 ); ++image4It;
  image4It.Set( 0 ); ++image4It;

  //Image no. 5
  image5It.Set( 0.70710678 ); ++image5It;  // 1/sqrt(2)
  image5It.Set( 0.70710678 ); ++image5It;
  image5It.Set( 0 ); ++image5It;
  image5It.Set( 0 ); ++image5It;

  //Image no. 6
  image6It.Set( -0.70710678 ); ++image6It;
  image6It.Set( 0.70710678 ); ++image6It;
  image6It.Set( 0 ); ++image6It;
  image6It.Set( 0 ); ++image6It;

  //Image no. 7
  image7It.Set( 0 ); ++image7It;
  image7It.Set( 0 ); ++image7It;
  image7It.Set( 1 ); ++image7It;
  image7It.Set( 0 ); ++image7It;

  //Image no. 8
  image8It.Set( 0 ); ++image8It;
  image8It.Set( 0 ); ++image8It;
  image8It.Set( 0 ); ++image8It;
  image8It.Set( 0 ); ++image8It;

  //----------------------------------------------------------------------
  // Test code for the Decomposition Calculator
  //----------------------------------------------------------------------

  //----------------------------------------------------------------------
  //Set the image Decomposition Calculator
  //----------------------------------------------------------------------
  typedef itk::ImagePCADecompositionCalculator<InputImageType>
    ImagePCAShapeModelEstimatorType;

  ImagePCAShapeModelEstimatorType::Pointer
    decomposer = ImagePCAShapeModelEstimatorType::New();

  //----------------------------------------------------------------------
  //Set the parameters of the clusterer
  //----------------------------------------------------------------------
  // add the first two vectors to the projection basis
  ImagePCAShapeModelEstimatorType::BasisImagePointerVector basis;
  basis.push_back(image1);
  basis.push_back(image2);
  decomposer->SetBasisImages(basis);

  // compute some projections!
  ImagePCAShapeModelEstimatorType::BasisVectorType proj3, proj4;
  decomposer->SetImage(image3);
  decomposer->Compute();
  proj3 = decomposer->GetProjection();

  decomposer->SetImage(image4);
  decomposer->Compute();
  proj4 = decomposer->GetProjection();

  // get the basis images
  ImagePCAShapeModelEstimatorType::BasisImagePointerVector basis_check;
  basis_check = decomposer->GetBasisImages();

  basis.clear();
  basis.push_back(image5);
  basis.push_back(image6);
  basis.push_back(image7);
  decomposer->SetBasisImages(basis);

  ImagePCAShapeModelEstimatorType::BasisVectorType proj3_2, proj4_2;
  //decomposer->SetImage(image4); // DON'T set image4 -- it should still
  // be cached with the decomposer. Test that this works between basis changes.
  decomposer->Compute();
  proj4_2 = decomposer->GetProjection();

  decomposer->SetImage(image3);
  decomposer->Compute();
  proj3_2 = decomposer->GetProjection();

  ImagePCAShapeModelEstimatorType::BasisVectorType proj3_3, proj4_3;
  // now test it with a mean image set
  decomposer->SetMeanImage(image8);

  decomposer->SetImage(image3);
  decomposer->Compute();
  proj3_3 = decomposer->GetProjection();

  decomposer->SetImage(image4);
  decomposer->Compute();
  proj4_3 = decomposer->GetProjection();


  // get the basis images
  ImagePCAShapeModelEstimatorType::BasisImagePointerVector basis_check_2;
  basis_check_2 = decomposer->GetBasisImages();

  //Test the printself function to increase coverage
  decomposer->Print(std::cout);

  // Print the basis and projections: first the PCA basis
  std::cout << "The basis of projection is: " << std::endl;
  for (ImagePCAShapeModelEstimatorType::BasisImagePointerVector::const_iterator
     basis_it = basis_check.begin(); basis_it != basis_check.end(); ++basis_it)
    {
    std::cout << "[";
    InputImageIterator basisImage_it( *basis_it, (*basis_it)->GetBufferedRegion() );
    for (basisImage_it.GoToBegin(); !basisImage_it.IsAtEnd(); ++basisImage_it)
      {
      std::cout << basisImage_it.Get() << " ";
      }
    std::cout << "]" << std::endl;
    }


  //Print the projections
  std::cout << "The projection of [0 2 2 0] is [" << proj3 << "]" << std::endl;
  std::cout << "this should be approx [-1.5412 -2.3716]" << std::endl;

  std::cout << "The projection of [0 3 3 0] is [" << proj4 << "]" << std::endl;
  std::cout << "this should be approx [3.5574 -2.3119]" << std::endl;

  // Print the basis and projections: now the new basis
  std::cout << std::endl;
  std::cout << "Now the basis of projection is: " << std::endl;
  for (ImagePCAShapeModelEstimatorType::BasisImagePointerVector::const_iterator
     basis_it = basis_check_2.begin(); basis_it != basis_check_2.end(); ++basis_it)
    {
    std::cout << "[";
    InputImageIterator basisImage_it( *basis_it, (*basis_it)->GetBufferedRegion() );
    for (basisImage_it.GoToBegin(); !basisImage_it.IsAtEnd(); ++basisImage_it)
      {
      std::cout << basisImage_it.Get() << " ";
      }
    std::cout << "]" << std::endl;
    }


  //Print the projections
  std::cout << "The projection of [0 2 2 0] is [" << proj3_2 << "]" << std::endl;
  std::cout << "this should be approx [1.4142 -1.4142 0]" << std::endl;

  std::cout << "The projection of [0 3 3 0] is [" << proj4_2 << "]" << std::endl;
  std::cout << "this should be approx [2.1213 2.1213 3.000]" << std::endl;

  std::cout << "The projection of [0 2 2 0] is (mean of zero set) [" << proj3_3 << "]" << std::endl;
  std::cout << "this should be approx [1.4142 -1.4142 0]" << std::endl;

  std::cout << "The projection of [0 3 3 0] is (mean of zero set) [" << proj4_3 << "]" << std::endl;
  std::cout << "this should be approx [2.1213 2.1213 3.000]" << std::endl;

  //Test for the eigen values for the test case precomputed using Matlab
  std::cout << "" << std::endl;
  if( proj3[0] < -1.54 && proj3[0] > -1.55 && proj4[1] < -2.31 && proj4[1] > -2.32 &&
      proj3_2[1] < -1.414 && proj3_2[1] > -1.415 && proj4_2[2] < 3.01 && proj4_2[2] > 2.99 &&
      proj3_3 == proj3_2 && proj4_3 == proj4_2)
    {
    std::cerr << "Test Passed" << std::endl;
    return EXIT_SUCCESS;
    }
  std::cerr << "Test failed" << std::endl;
  std::cerr << "The project is out of the range of Matlab precomputed values" << std::endl;
  return EXIT_FAILURE;
}
