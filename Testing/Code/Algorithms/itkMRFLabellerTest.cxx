/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRFLabellerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

// Insight classes
#include "itkImage.h"
#include "itkVector.h"
#include "vnl/vnl_matrix_fixed.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkGaussianSupervisedClassifier.h"
#include "itkMRFImageFilter.h"

//Data definitons 
#define   IMGWIDTH            2
#define   IMGHEIGHT           2
#define   NFRAMES             4
#define   NUMBANDS            2  
#define   NDIMENSION          3
#define   NUM_CLASSES         3
#define   MAX_NUM_ITER       50


int main()
{

  //------------------------------------------------------
  //Create a simple test image with width, height, and 
  //depth 4 vectors each with each vector having data for 
  //2 bands.
  //------------------------------------------------------
  typedef itk::Image<itk::Vector<double,NUMBANDS>,NDIMENSION> VecImageType; 

  VecImageType::Pointer vecImage = VecImageType::New();

  VecImageType::SizeType vecImgSize = {{ IMGWIDTH , IMGHEIGHT, NFRAMES }};

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
  typedef itk::ImageRegionIteratorWithIndex< VecImageType > VecIterator;

  VecIterator outIt( vecImage, vecImage->GetBufferedRegion() );
  outIt.Begin();

  //Set up the vector to store the image  data
  typedef VecImageType::PixelType     DataVector;
  DataVector   dblVec; 

  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------
  //Slice 1
  //Vector no. 1
  dblVec[0] = 21; 
  dblVec[1] = 19;
  outIt.Set(dblVec); 
  ++outIt;

  //Vector no. 2
  dblVec[0] = 20; 
  dblVec[1] = 20;
  outIt.Set(dblVec); 
  ++outIt;

  //Vector no. 3
  dblVec[0] = 8; 
  dblVec[1] = 11;
  outIt.Set(dblVec); 
  ++outIt;

  //Vector no. 4
  dblVec[0] = 10; 
  dblVec[1] = 12;
  outIt.Set(dblVec); 
  ++outIt;

  //Slice 2
  //Vector no. 1
  dblVec[0] = 22; 
  dblVec[1] = 21;
  outIt.Set(dblVec); 
  ++outIt;

  //Vector no. 2
  dblVec[0] = 21; 
  dblVec[1] = 22;
  outIt.Set(dblVec);
  ++outIt;

  //Vector no. 3
  dblVec[0] = 11; 
  dblVec[1] = 12;
  outIt.Set(dblVec); 
  ++outIt;

  //Vector no. 4
  dblVec[0] = 9; 
  dblVec[1] = 10;
  outIt.Set(dblVec); 
  ++outIt;

  //Slice 3
  //Vector no. 1 
  dblVec[0] = 19; 
  dblVec[1] = 20;
  outIt.Set(dblVec); 
  ++outIt;

  //Vector no. 2
  dblVec[0] = 19; 
  dblVec[1] = 21;
  outIt.Set(dblVec); 
  ++outIt;

  //Vector no. 3
  dblVec[0] = 11; 
  dblVec[1] = 11;
  outIt.Set(dblVec); 
  ++outIt;

  //Vector no. 4
  dblVec[0] = 11; 
  dblVec[1] = 10;
  outIt.Set(dblVec); 
  ++outIt;

  //Slice 4
  //Vector no. 1
  dblVec[0] = 18; 
  dblVec[1] = 18;
  outIt.Set(dblVec); 
  ++outIt;

  //Vector no. 2
  dblVec[0] = 18; 
  dblVec[1] = 20;
  outIt.Set(dblVec); 
  ++outIt;
  
  //Vector no. 3
  dblVec[0] = 12; 
  dblVec[1] = 10;
  outIt.Set(dblVec); 
  ++outIt;
  
  //Vector no. 4
  dblVec[0] = 14; 
  dblVec[1] = 13;
  outIt.Set(dblVec); 
  ++outIt;

  //---------------------------------------------------------------
  //Generate the training data
  //---------------------------------------------------------------
  typedef itk::Image<unsigned short,NDIMENSION> ClassImageType; 
  ClassImageType::Pointer classImage  = ClassImageType::New();

  ClassImageType::SizeType classImgSize = {{ IMGWIDTH , IMGHEIGHT, NFRAMES }};

  ClassImageType::IndexType classindex = ClassImageType::IndexType::ZeroIndex;
  ClassImageType::RegionType classregion;

  classregion.SetSize( classImgSize );
  classregion.SetIndex( classindex );

  classImage->SetLargestPossibleRegion( classregion );
  classImage->SetBufferedRegion( classregion );
  classImage->Allocate();

  // setup the iterators
  typedef ClassImageType::PixelType ClassImagePixelType;

  typedef  itk::ImageRegionIteratorWithIndex<ClassImageType>  ClassImageIterator;

  ClassImageIterator classoutIt( classImage, classImage->GetBufferedRegion() );

  classoutIt.Begin();



  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------
  //Slice 1
  //Pixel no. 1
  classoutIt.Set( 2 );
  ++classoutIt;

  //Pixel no. 2 
  classoutIt.Set( 2 );
  ++classoutIt;

  //Pixel no. 3
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 4
  classoutIt.Set( 1 );
  ++classoutIt;

  //Slice 2
  //Pixel no. 1
  classoutIt.Set( 0 );
  ++classoutIt;
  
  //Pixel no. 2
  classoutIt.Set( 0 );
  ++classoutIt;

  //Pixel no. 3
  classoutIt.Set( 0 );
  ++classoutIt;

  //Pixel no. 4
  classoutIt.Set( 0 );
  ++classoutIt;

  //Slice 3
  //Pixel no. 1 
  classoutIt.Set( 2 );
  ++classoutIt;

  //Pixel no. 2
  classoutIt.Set( 2 );
  ++classoutIt;
  
  //Pixel no. 3
  classoutIt.Set( 1 );
  ++classoutIt;

  //Pixel no. 4
  classoutIt.Set( 1 );
  ++classoutIt;

  //Slice 4
  //Pixel no. 1
  classoutIt.Set( 0 );
  ++classoutIt;
  
  //Pixel no. 2
  classoutIt.Set( 0 );
  ++classoutIt;

  //Pixel no. 3
  classoutIt.Set( 0 );
  ++classoutIt;

  //Pixel no. 4
  classoutIt.Set( 0 );
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

  //Set the MRF labeller
  typedef itk::MRFImageFilter<VecImageType,ClassImageType> MRFImageFilterType;
  MRFImageFilterType::Pointer applyMRFImageFilter = MRFImageFilterType::New();

  // Set the MRF labeller parameters
  applyMRFImageFilter->SetNumClasses(NUM_CLASSES);
  applyMRFImageFilter->SetMaxNumIter(MAX_NUM_ITER);
  applyMRFImageFilter->SetErrorTollerance(0.00);
 
  applyMRFImageFilter->SetInput(vecImage);
  applyMRFImageFilter
    ->SetClassifier((ClassifierType) myGaussianClassifier ); 

  //Since a suvervised classifier is used, it requires a training image
  applyMRFImageFilter->SetTrainingImage(classImage);  
  
  //Kick off the MRF labeller function
  applyMRFImageFilter->Update();
  
  ClassImageType::Pointer  outClassImage = applyMRFImageFilter->GetOutput();

  //Print the mrf labelled image
  ClassImageIterator labeloutIt( outClassImage, outClassImage->GetBufferedRegion() );
  labeloutIt.Begin();

  //Verify if the results were as per expectation
  bool passTest = true;

  //Loop through the data set
  while( ! labeloutIt.IsAtEnd() )
  {
    int classIndex = (int) labeloutIt.Get();
    if (classIndex != 2)
    {
      passTest = false;
      break;
    }
    ++labeloutIt;

    classIndex = (int) labeloutIt.Get();
    if (classIndex != 2)
    {
      passTest = false;
      break;
    }
    ++labeloutIt;

    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
      break;
    }
    ++labeloutIt;

    classIndex = (int) labeloutIt.Get();
    if (classIndex != 1)
    {
      passTest = false;
      break;
    }
    ++labeloutIt;

  }//end while

  if( passTest == true ) 
    std::cout<< "MRF labeller Test Passed" << std::endl;
  else 
    std::cout<< "MRF labeller Test failed" << std::endl;

  return 0;
}
