/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReflectImageFilterTest.cxx
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
#include <itkReflectImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkImageLinearIteratorWithIndex.h>


int main() 
{

  // Define the dimension of the image
  const unsigned int myDimension = 3;

  // Declare the types of the image
  typedef itk::Image<unsigned short, myDimension>  myImageType;

  // Declare the type of the Region
  typedef myImageType::RegionType       myRegionType;

  // Declare the type of the index to access images
  typedef myRegionType::IndexType        myIndexType;

  // Declare the type of the size 
  typedef myRegionType::SizeType         mySizeType;

  // Declare the type for the ADD filter
  typedef itk::ReflectImageFilter<
                        myImageType,
                        myImageType  >  myFilterType;
 
  // Declare the pointers to images
  typedef myImageType::Pointer   myImagePointer;

  // Create an image
  myImagePointer inputImage = myImageType::New();
  
  // Define their size, and start index
  mySizeType size;
  size[0] = 15;
  size[1] = 2;
  size[2] = 1;

  myIndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image 
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();


  // Declare Iterator types apropriated for each image 
  typedef itk::ImageRegionIteratorWithIndex<myImageType>  myIteratorType;

  // Create one iterator for the input Image  (this is a light object)
  myIteratorType it( inputImage, inputImage->GetRequestedRegion() );

  // Initialize the content of the input Image 
  // with a ramp along the 0 direction
  std::cout << "Input Image " << std::endl;
  unsigned long counter = 0;
  while( !it.IsAtEnd() ) 
  {
    it.Set( counter );
    std::cout << it.Get() << std::endl;
    ++counter;
    ++it;
  }

  // Create the Filter                                
  myFilterType::Pointer filter = myFilterType::New();


  // Connect the input image
  filter->SetInput( inputImage ); 

  // Get the Smart Pointer to the Filter Output 
  myImagePointer outputImage = filter->GetOutput();

  
  // Execute the filter
  filter->Update();

  // Create an iterator for going through the image output
  myIteratorType ot( outputImage, outputImage->GetRequestedRegion() );
  
  //  Print the content of the result image
  std::cout << " Result " << std::endl;
  while( !ot.IsAtEnd() ) 
  {
    std::cout << ot.GetIndex() << " = " << ot.Get() << std::endl;
    ++ot;
  }


  // All objects should be automatically destroyed at this point
  return 0;

}




