/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkJoinImageFilterTest.cxx
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
#include <itkJoinImageFilter.h>
#include <itkImageRegionIterator.h>
#include <itkVector.h>
#include <vnl/vnl_sample.h>

int main() 
{
  // Define the dimension of the images
  const unsigned int myDimension = 2;

  // Declare the types of the images
  typedef itk::Image<char, myDimension>  myImageType1;
  typedef itk::Image<itk::Vector<unsigned short, 2>, myDimension>  myImageType2;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>         myIndexType;

  // Declare the type of the size 
  typedef itk::Size<myDimension>          mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>        myRegionType;

  // Create two images
  myImageType1::Pointer inputImageA  = myImageType1::New();
  myImageType2::Pointer inputImageB  = myImageType2::New();
  
  // Define their size, and start index
  mySizeType size;
  size[0] = 4;
  size[1] = 4;

  myIndexType start;
  start[0] = 0;
  start[1] = 0;

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImageA->SetLargestPossibleRegion( region );
  inputImageA->SetBufferedRegion( region );
  inputImageA->SetRequestedRegion( region );
  inputImageA->Allocate();

  // Initialize Image B
  inputImageB->SetLargestPossibleRegion( region );
  inputImageB->SetBufferedRegion( region );
  inputImageB->SetRequestedRegion( region );
  inputImageB->Allocate();

  // Declare Iterator types apropriated for each image 
  typedef itk::ImageRegionIterator<myImageType1>  myIteratorType1;
  typedef itk::ImageRegionIterator<myImageType2>  myIteratorType2;

  // Create one iterator for Image A (this is a light object)
  myIteratorType1 it1( inputImageA, region );

  // Initialize the content of Image A
  std::cout << "Image #1 " << std::endl;
  while( !it1.IsAtEnd() ) 
  {
    it1.Set( (char) vnl_sample_uniform(0, 255) );
    std::cout << (int) it1.Get() << std::endl;
    ++it1;
  }

  // Create one iterator for Image B (this is a light object)
  myIteratorType2 it2( inputImageB, region );

  // Initialize the content of Image B
  std::cout << std::endl;
  std::cout << "Image #2 " << std::endl;
  itk::Vector<unsigned short, 2> vec;
  while( !it2.IsAtEnd() ) 
  {
  vec[0] = (unsigned short) vnl_sample_uniform(0, 32765);
  vec[1] = (unsigned short) vnl_sample_uniform(0, 32765);
  it2.Set( vec );
  std::cout << it2.Get() << std::endl;
  ++it2;
  }

  // Declare the type for the Join Filter
  typedef itk::JoinImageFilter<myImageType1, myImageType2> myFilterType;
  typedef itk::JoinImageFilter<myImageType2, myImageType1> myFilterType1;
  typedef itk::JoinImageFilter<myImageType1, myImageType1> myFilterType2;
  typedef itk::JoinImageFilter<myImageType2, myImageType2> myFilterType3;  
            
  typedef itk::ImageRegionIterator<myFilterType::OutputImageType> myOutputIteratorType;
  typedef itk::ImageRegionIterator<myFilterType1::OutputImageType> myOutputIteratorType1;
  typedef itk::ImageRegionIterator<myFilterType2::OutputImageType> myOutputIteratorType2;
  typedef itk::ImageRegionIterator<myFilterType3::OutputImageType> myOutputIteratorType3;
  

  //
  // Join image #1 and #2
  //

  // Setup a JoinImageFilter 
  myFilterType::Pointer filter = myFilterType::New();
  filter->SetInput1( inputImageA ); 
  filter->SetInput2( inputImageB );
  filter->Update();

  // Create an iterator for going through the image output
  myFilterType::OutputImageType::Pointer outputImage = filter->GetOutput();
  myOutputIteratorType it3(outputImage, outputImage->GetRequestedRegion());
  
  //  Print the content of the result image
  std::cout << std::endl;
  std::cout << "Joining #1 and #2 image " << std::endl;
  while( !it3.IsAtEnd() ) 
    {
    std::cout << it3.Get() << std::endl;
    ++it3;
    }

  //
  // Join image #2 and #1
  //

  // Setup a JoinImageFilter 
  myFilterType1::Pointer filter1 = myFilterType1::New();
  filter1->SetInput1( inputImageB ); 
  filter1->SetInput2( inputImageA );
  filter1->Update();

  // Create an iterator for going through the image output
  myFilterType1::OutputImageType::Pointer outputImage1 = filter1->GetOutput();
  myOutputIteratorType1 it4(outputImage1, outputImage1->GetRequestedRegion());
  
  //  Print the content of the result image
  std::cout << std::endl;
  std::cout << "Joining #2 and #1 image " << std::endl;
  while( !it4.IsAtEnd() ) 
    {
    std::cout << it4.Get() << std::endl;
    ++it4;
    }

  //
  // Join image #1 and #1
  //

  // Setup a JoinImageFilter 
  myFilterType2::Pointer filter2 = myFilterType2::New();
  filter2->SetInput1( inputImageA ); 
  filter2->SetInput2( inputImageA );
  filter2->Update();

  // Create an iterator for going through the image output
  myFilterType2::OutputImageType::Pointer outputImage2 = filter2->GetOutput();
  myOutputIteratorType2 it5(outputImage2, outputImage2->GetRequestedRegion());
  
  //  Print the content of the result image
  std::cout << std::endl;
  std::cout << "Joining #1 and #1 image " << std::endl;
  while( !it5.IsAtEnd() ) 
    {
    std::cout << (int) it5.Get()[0] << "  " << (int) it5.Get()[1] << std::endl;
    ++it5;
    }

  //
  // Join image #2 and #2
  //

  // Setup a JoinImageFilter 
  myFilterType3::Pointer filter3 = myFilterType3::New();
  filter3->SetInput1( inputImageB ); 
  filter3->SetInput2( inputImageB );
  filter3->Update();

  // Create an iterator for going through the image output
  myFilterType3::OutputImageType::Pointer outputImage3 = filter3->GetOutput();
  myOutputIteratorType3 it6(outputImage3, outputImage3->GetRequestedRegion());
  
  //  Print the content of the result image
  std::cout << std::endl;
  std::cout << "Joining #2 and #2 image " << std::endl;
  while( !it6.IsAtEnd() ) 
    {
    std::cout << it6.Get() << std::endl;
    ++it6;
    }


  // All objects should be automatically destroyed at this point
  return 0;

}




