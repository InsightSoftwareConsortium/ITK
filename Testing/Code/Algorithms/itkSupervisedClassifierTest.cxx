/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSupervisedClassifierTest.cxx
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
#include "itkImageRegionIterator.h"
#include "itkGaussianSupervisedClassifier.h"

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
  typedef VecImageType::PixelType VecPixelType;

  enum { VecImageDimension = VecImageType::ImageDimension };
  typedef
    itk::ImageRegionIterator< VecImageType > VecIterator;

  VecIterator outIt( vecImage, vecImage->GetBufferedRegion() );
  outIt = outIt.Begin();


  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------

  //Slice 1
  //Vector no. 1
  VecPixelType vec;
  vec = 21,19; outIt.Set( vec ); ++outIt;
  //Vector no. 2
  vec = 20,20; outIt.Set( vec ); ++outIt;
  //Vector no. 3
  vec = 8,11; outIt.Set( vec ); ++outIt;
  //Vector no. 4
  vec = 10,12; outIt.Set( vec ); ++outIt;
  //Slice 2
  //Vector no. 1
  vec = 22,21; outIt.Set( vec ); ++outIt;
  //Vector no. 2
  vec = 21,22; outIt.Set( vec ); ++outIt;
  //Vector no. 3
  vec = 11,12; outIt.Set( vec ); ++outIt;
  //Vector no. 4
  vec = 9,10; outIt.Set( vec ); ++outIt;
  
  //Slice 3
  //Vector no. 1 
  vec = 19,20; outIt.Set( vec ); ++outIt;
  //Vector no. 2
  vec = 19,21; outIt.Set( vec ); ++outIt;
  //Vector no. 3
  vec = 11,11; outIt.Set( vec ); ++outIt;
  //Vector no. 4
  vec = 11,10; outIt.Set( vec ); ++outIt;
  
  //Slice 4
  //Vector no. 1
  vec = 18,18; outIt.Set( vec ); ++outIt;
  //Vector no. 2
  vec = 18,20; outIt.Set( vec ); ++outIt;
  //Vector no. 3
  vec = 12,10; outIt.Set( vec ); ++outIt;
  //Vector no. 4
  vec = 14,13; outIt.Set( vec ); ++outIt;

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

  typedef
    itk::ImageRegionIterator<ClassImageType> ClassImageIterator;

  ClassImageIterator 
	  classoutIt( classImage, classImage->GetBufferedRegion() );
  classoutIt = classoutIt.Begin();



  ClassImagePixelType outputPixel;
  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------
  //Slice 1
  //Pixel no. 1
  outputPixel = 2;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 2 
  outputPixel = 2;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 3
  outputPixel = 1;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 4
  outputPixel = 1;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Slice 2
  //Pixel no. 1
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;
  
  //Pixel no. 2
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 3
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 4
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Slice 3
  //Pixel no. 1 
  outputPixel = 2;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 2
  outputPixel = 2;
  classoutIt.Set( outputPixel );
  ++classoutIt;
  
  //Pixel no. 3
  outputPixel = 1;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 4
  outputPixel = 1;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Slice 4
  //Pixel no. 1
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;
  
  //Pixel no. 2
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 3
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //Pixel no. 4
  outputPixel = 0;
  classoutIt.Set( outputPixel );
  ++classoutIt;

  //----------------------------------------------------------------------
  // Test code for the supervised classifier algorithm
  //----------------------------------------------------------------------

  //---------------------------------------------------------------------
  // Multiband data is now available in the right format
  //---------------------------------------------------------------------
  typedef itk::GaussianSupervisedClassifier<VecImageType,ClassImageType> 
	  GaussianSupervisedClassifierType;
  GaussianSupervisedClassifierType::Pointer 
	  applyClassifier = GaussianSupervisedClassifierType::New();

  // Set the Classifier parameters
  applyClassifier->SetNumClasses(NUM_CLASSES);
  applyClassifier->SetInputImage(vecImage);
  applyClassifier->SetTrainingImage(classImage);  

  //Run the gaussian classifier algorithm
  applyClassifier->ClassifyImage();

  ClassImageType::Pointer outClassImage =  
	  applyClassifier->GetClassifiedImage();

  //Print the gaussian classified image
  ClassImageIterator labeloutIt( outClassImage, 
                                 outClassImage->GetBufferedRegion() );
  labeloutIt = labeloutIt.Begin();
  ClassImageIterator labeloutItEnd = labeloutIt.End();

  int i=0;
  while(labeloutIt != labeloutItEnd)
    {
    //Print the classified index
    int classIndex = (int) labeloutIt.Get();
    std::cout << " Pixel No" << i << " Value " << classIndex << std::endl;
    ++i;
    ++labeloutIt;
    }//end while

  //Verify if the results were as per expectation
  labeloutIt = labeloutIt.Begin();
  labeloutItEnd = labeloutIt.End();
  bool passTest = true;

  //Loop through the data set
  while(labeloutIt != labeloutItEnd)
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
    {
    std::cout<< "Supervised Classifier Test Passed" << std::endl;
    }
  else
    {
    std::cout<< "Supervised Classifier Test failed" << std::endl;
    }

  return 0;
}
