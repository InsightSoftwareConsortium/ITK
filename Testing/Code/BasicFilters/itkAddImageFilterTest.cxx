/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAddImageFilterTest.cxx
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
#include <itkAddImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>



// Define the dimension of the images
const unsigned int myDimension = 3;

// Declare the types of the images
typedef itk::Image<float, myDimension>  myInputImageType;
typedef itk::Image<float, myDimension>  myOutputImageType;

// Declare the type of the index to access images
typedef itk::Index<myDimension>         myIndexType;

// Declare the type of the size 
typedef itk::Size<myDimension>          mySizeType;

// Declare the type of the Region
typedef itk::ImageRegion<myDimension>        myRegionType;

// Declare the type of the Region
typedef itk::ImageRegionIteratorWithIndex<myInputImageType>  myImageIteratorType;

// Declare the type for the ADD filter
typedef itk::AddImageFilter<
                              myInputImageType,
                              myOutputImageType  >  myFilterType;
 



// Function for image initialization
void InitializeImage( myInputImageType * image, double value   )
{

  myInputImageType::Pointer inputImage( image );

  // Define their size, and start index
  mySizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  myIndexType start;
  start = myIndexType::ZeroIndex;

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );
  
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  myImageIteratorType it( inputImage, 
                     inputImage->GetRequestedRegion() );
  
  it.GoToBegin();
  while( !it.IsAtEnd() ) 
    {
    it.Set( value );
    ++it;
    }


}



// Function for image printing
void PrintImage( myInputImageType * image, const char *text )
{
  // Create an iterator for going through the image
  myImageIteratorType it( image, 
                          image->GetRequestedRegion() );
  
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

  // Create two images
  myInputImageType::Pointer inputImageA  = myInputImageType::New();
  myInputImageType::Pointer inputImageB  = myInputImageType::New();
  
  InitializeImage( inputImageA, 12 );
  InitializeImage( inputImageB, 17 );

  PrintImage( inputImageA, "Input image A" ); 
  PrintImage( inputImageB, "Input image B" ); 

  // Create an ADD Filter                                
  myFilterType::Pointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput( 0, inputImageA ); 
  filter->SetInput( 1, inputImageB );

  // Get the Smart Pointer to the Filter Output 
  myOutputImageType::Pointer outputImage = filter->GetOutput();

  
  // Execute the filter
  filter->Update();

  PrintImage( outputImage, "Resulting image" ); 

  // All objects should be automatically destroyed at this point
  return 0;

}




