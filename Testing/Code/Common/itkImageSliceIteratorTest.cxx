/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkImageSliceIteratorTest.cxx
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
#include <iostream>

#include "itkImage.h"
#include "itkImageSliceIterator.h"
#include "itkImageSliceConstIterator.h"




int main()
{
  std::cout << "Creating an image" << std::endl;
  typedef itk::Image<unsigned short,3> ImageType;

  ImageType::Pointer myImage = ImageType::New();
  
  ImageType::SizeType size;

  size[0] = 100;
  size[1] = 100;
  size[2] = 100;

  ImageType::IndexType start;
  start = ImageType::IndexType::ZeroIndex;

  ImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  myImage->SetLargestPossibleRegion( region );
  myImage->SetBufferedRegion( region );
  myImage->SetRequestedRegion( region );
  myImage->Allocate();

  typedef itk::ImageSliceIterator< ImageType > IteratorType;

  typedef itk::ImageSliceConstIterator< ImageType > ConstIteratorType;

  IteratorType it( myImage, region );

  it.GoToBegin();
  it.SetFirstDirection( 0 ); // 0=x, 1=y, 2=z
  it.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z

  ImageType::IndexType index;
  ImageType::PixelType value;
  
  while( !it.IsAtEnd() )
  {
    while( !it.IsAtEndOfSlice() )
    {
      while( !it.IsAtEndOfLine() )
      {
        index = it.GetIndex();
        value = index[0] + index[1] + index[2];
        it.Set( value );
        ++it;
      }
      it.NextLine();
    }
    it.NextSlice();
  }

  
  // Verification 
  std::cout << "Verifying for iterator...";
  IteratorType ot( myImage, region );

  ot.GoToBegin();
  ot.SetFirstDirection( 0 ); // 0=x, 1=y, 2=z
  ot.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z
 
  while( !ot.IsAtEnd() )
  {
    while( !ot.IsAtEndOfSlice() )
    {
      while( !ot.IsAtEndOfLine() )
      {
        index = ot.GetIndex();
        value = index[0] + index[1] + index[2];
        if( ot.Get() != value )
        {
          std::cerr << "Values don't correspond to what was stored "
            << std::endl;
          std::cerr << "Test failed at index ";
          std::cerr << index << std::endl;
          return EXIT_FAILURE;
        }
        ++ot;
      }
      ot.NextLine();
    }
    ot.NextSlice();
  }
  std::cout << "  Done !" << std::endl;


  // Verification 
  std::cout << "Verifying for const iterator...";
  ConstIteratorType cot( myImage, region );

  cot.GoToBegin();
  cot.SetFirstDirection( 0 ); // 0=x, 1=y, 2=z
  cot.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z
 
  while( !cot.IsAtEnd() )
  {
    while( !cot.IsAtEndOfSlice() )
    {
      while( !cot.IsAtEndOfLine() )
      {
        index = cot.GetIndex();
        value = index[0] + index[1] + index[2];
        if( cot.Get() != value )
        {
          std::cerr << "Values don't correspond to what was stored "
            << std::endl;
          std::cerr << "Test failed at index ";
          std::cerr << index << std::endl;
          return EXIT_FAILURE;
        }
        ++cot;
      }
      cot.NextLine();
    }
    cot.NextSlice();
  }
  std::cout << "  Done !" << std::endl;




  std::cout << "Test passed" << std::endl;

  return EXIT_SUCCESS;

}



