/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkImageLinearIteratorTest.cxx
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
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageLinearConstIteratorWithIndex.h"




int main()
{
  std::cout << "Creating an image of indices" << std::endl;

  const unsigned int ImageDimension = 3;

  typedef itk::Index< ImageDimension >             PixelType;

  typedef itk::Image< PixelType, ImageDimension >  ImageType;

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

  typedef itk::ImageLinearIteratorWithIndex< ImageType > IteratorType;

  typedef itk::ImageLinearConstIteratorWithIndex< ImageType > ConstIteratorType;

  IteratorType it( myImage, region );


  it.GoToBegin();
  it.SetDirection( 0 ); // 0=x, 1=y, 2=z

  ImageType::IndexType index;
  
  while( !it.IsAtEnd() )
  {
    while( !it.IsAtEndOfLine() )
    {
      index = it.GetIndex();
      it.Set( index );
      ++it;
    }
    it.NextLine();
  }

  
  // Verification 
  IteratorType ot( myImage, region );

  ot.GoToBegin();
  ot.SetDirection( 0 ); // 0=x, 1=y, 2=z
 
  std::cout << "Verifying iterator... ";

  while( !ot.IsAtEnd() )
  {
    while( !ot.IsAtEndOfLine() )
    {
      index = ot.GetIndex();
      if( ot.Get() != index )
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
  std::cout << "   Done ! " << std::endl;

  
  // Verification 
  ConstIteratorType cot( myImage, region );

  cot.GoToBegin();
  cot.SetDirection( 0 ); // 0=x, 1=y, 2=z
 
  std::cout << "Verifying const iterator... ";

  while( !cot.IsAtEnd() )
  {
    while( !cot.IsAtEndOfLine() )
    {
      index = cot.GetIndex();
      if( cot.Get() != index )
      {
        std::cerr << "Values don't correspond to what was stored "
          << std::endl;
        std::cerr << "Test failed at index ";
        std::cerr << index << " value is " << cot.Get() <<  std::endl;
        return EXIT_FAILURE;
      }
      ++cot;
    }
    cot.NextLine();
  }
  std::cout << "   Done ! " << std::endl;



  // Verification 
  std::cout << "Verifying iterator in reverse direction... ";

  IteratorType ior( myImage, region );

  ior.GoToEnd();
  ior.SetDirection( 0 ); // 0=x, 1=y, 2=z
 

  while( !ior.IsAtBegin() )
  {
    while( !ior.IsAtBeginOfLine() )
    {
      index = ior.GetIndex();
      if( ior.Get() != index )
      {
        std::cerr << "Values don't correspond to what was stored "
          << std::endl;
        std::cerr << "Test failed at index ";
        std::cerr << index << " value is " << ior.Get() <<  std::endl;
        return EXIT_FAILURE;
      }
      --ior;
    }
    ior.PreviousLine();
  }
  std::cout << "   Done ! " << std::endl;



  // Verification 
  std::cout << "Verifying const iterator in reverse direction... ";

  ConstIteratorType cor( myImage, region );

  cor.GoToEnd();
  cor.SetDirection( 0 ); // 0=x, 1=y, 2=z
 

  while( !cor.IsAtBegin() )
  {
    while( !cor.IsAtBeginOfLine() )
    {
      index = cor.GetIndex();
      if( cor.Get() != index )
      {
        std::cerr << "Values don't correspond to what was stored "
          << std::endl;
        std::cerr << "Test failed at index ";
        std::cerr << index << " value is " << cor.Get() <<  std::endl;
        return EXIT_FAILURE;
      }
      --cor;
    }
    cor.PreviousLine();
  }
  std::cout << "   Done ! " << std::endl;




  std::cout << "Test passed" << std::endl;




  return EXIT_SUCCESS;

}



