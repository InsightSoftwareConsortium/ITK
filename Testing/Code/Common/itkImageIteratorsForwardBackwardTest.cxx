/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkImageIteratorsForwardBackwardTest.cxx
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
#include "itkNumericTraits.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkImageLinearIterator.h"




int main()
{

  std::cout << "Creating an image" << std::endl;
  typedef itk::Image<unsigned short,3> ImageType;

  ImageType::Pointer myImage = ImageType::New();
  
  ImageType::SizeType size;

  size[0] = 10;
  size[1] = 10;
  size[2] = 10;

  ImageType::IndexType start;
  start = ImageType::IndexType::ZeroIndex;

  ImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  myImage->SetLargestPossibleRegion( region );
  myImage->SetBufferedRegion( region );
  myImage->SetRequestedRegion( region );
  myImage->Allocate();

  typedef itk::SimpleImageRegionIterator< ImageType > IteratorType;

  IteratorType it( myImage, region );


  it.Begin();

  ImageType::IndexType index;
  ImageType::PixelType value;
  
  value = itk::NumericTraits< ImageType::PixelType >::Zero;

  // Store information on the Image
  std::cout << "Storing data on the image ... " << std::endl;

  while( !it.IsAtEnd() )
  {
    index = it.GetIndex();
    value++;
    it.Set( value );
    ++it;
  }

  
  // Verification 
  IteratorType ot( myImage, region );
  std::cout << "Verifying the data forwards... " << std::endl;

  ot.Begin();
 
  value = itk::NumericTraits< ImageType::PixelType >::Zero;

  while( !ot.IsAtEnd() )
  {
    value++;
    if( ot.Get() != value )
    {
      std::cerr << "Values don't correspond to what was stored "
        << std::endl;
      std::cerr << "Test failed at index ";
      std::cerr << ot.GetIndex() << std::endl;
      std::cerr << "Value stored is = " << ot.Get() << std::endl;
      std::cerr << "Value should be = " << value    << std::endl;
      return EXIT_FAILURE;
    }
    ++ot;
  }
 
  // Verification 
  std::cout << "Verifying the data backwards... " << std::endl;

//  ot.End();
 
  while( !ot.IsAtEnd() )
  {
    if( ot.Get() != value )
    {
      std::cerr << "Values don't correspond to what was stored "
        << std::endl;
      std::cerr << "Test failed at index ";
      std::cerr << ot.GetIndex() << std::endl;
      std::cerr << "Value stored is = " << ot.Get() << std::endl;
      std::cerr << "Value should be = " << value    << std::endl;
      return EXIT_FAILURE;
    }
    value--;
    --ot;
  }


  std::cout << "Test passed" << std::endl;

  return EXIT_SUCCESS;

}



