/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEigenAnalysis2DImageFilterTest.cxx
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




#include <itkImage.h>
#include <itkEigenAnalysis2DImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkCovariantVector.h>




// Define the dimension of the images
const unsigned int myDimension = 2;

// Declare type for Eigen Vectors
typedef itk::Vector<double, myDimension> myVectorType;

// Declare the types of the images
typedef itk::Image<double, myDimension>           myImageType;
typedef itk::Image<myVectorType, myDimension>     myVectorImageType;

// Declare the type of the index to access images
typedef itk::Index<myDimension>             myIndexType;

// Declare the type of the size 
typedef itk::Size<myDimension>              mySizeType;

// Declare the type of the Region
typedef itk::ImageRegion<myDimension>        myRegionType;


// Declare Iterator types apropriated for each image 
typedef itk::ImageRegionIteratorWithIndex<myImageType>  myIteratorType;
typedef itk::ImageRegionIteratorWithIndex<myVectorImageType>  myVectorIteratorType;


// Declare the Filter
typedef itk::EigenAnalysis2DImageFilter< myImageType,
                                         myImageType,
                                         myVectorImageType >  myFilterType;







// Function for image initialization
void InitializeImage( myImageType * image, double value   )
{

  myImageType::Pointer inputImage( image );

  // Define their size, and start index
  mySizeType size;
  size[0] = 2;
  size[1] = 2;

  myIndexType start;
  start = myIndexType::ZeroIndex;

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );
  
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  myIteratorType it( inputImage, 
                     inputImage->GetRequestedRegion() );
  
  it.GoToBegin();
  while( !it.IsAtEnd() ) 
    {
    it.Set( value );
    ++it;
    }

}



// Function for image printing
void PrintImage( myImageType * image, const char *text )
{

  myImageType::Pointer imagePtr( image );

  // Create an iterator for going through the image
  myIteratorType it( imagePtr, 
                     imagePtr->GetRequestedRegion() );

  it.GoToBegin();
  
  //  Print the content of the image
  std::cout << text << std::endl;
  while( !it.IsAtEnd() ) 
  {
    std::cout << it.Get() << std::endl;
    ++it;
  }

}


// Function for image printing
void PrintImage( myVectorImageType * image, const char *text )
{

  myVectorImageType::Pointer imagePtr( image );

  // Create an iterator for going through the image
  myVectorIteratorType it( imagePtr, 
                           imagePtr->GetRequestedRegion() );

  it.GoToBegin();
  
  //  Print the content of the image
  std::cout << text << std::endl;
  while( !it.IsAtEnd() ) 
  {
    std::cout << it.Get() << std::endl;
    ++it;
  }

}





int main() 
{
  // Create the images
  myImageType::Pointer inputImageXX  = myImageType::New();
  myImageType::Pointer inputImageXY  = myImageType::New();
  myImageType::Pointer inputImageYY  = myImageType::New();

  const double myPI = 4.0 * atan(1.0);

  InitializeImage( inputImageXX, cos( myPI / 6.0 ) );
  InitializeImage( inputImageXY, sin( myPI / 6.0 ) );
  InitializeImage( inputImageYY, cos( myPI / 6.0 ) );


  // Create a  Filter                                
  myFilterType::Pointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput1( inputImageXX ); 
  filter->SetInput2( inputImageXY ); 
  filter->SetInput3( inputImageYY ); 

  
  // Execute the filter
  filter->Update();

  // Get 
  myImageType::Pointer maxEigenValue = filter->GetMaxEigenValue();
  myImageType::Pointer minEigenValue = filter->GetMinEigenValue();
  
  myVectorImageType::Pointer maxEigenVector = filter->GetMaxEigenVector();

  PrintImage( maxEigenValue, "Max Eigen Value");
  PrintImage( minEigenValue, "Min Eigen Value");
  PrintImage( maxEigenVector, "Max Eigen Vector");

  
  // All objects should be automatically destroyed at this point

  return 0;

}




