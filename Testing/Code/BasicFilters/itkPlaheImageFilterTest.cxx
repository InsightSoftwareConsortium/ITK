/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPlaheImageFilterTest.cxx
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

// Disable warning for long symbol names in this file only
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif


#include "itkImage.h"
#include "itkPlaheImageFilter.h"
#include "itkGaussianImageSource.h"


int itkPlaheImageFilterTest(int, char**) 
{

  // Define the dimension of the images
  const unsigned int myDimension = 2;

  // Define the pixel type of the images
  typedef   float           myPixelType;

  // Declare the types of the images
  typedef itk::Image<myPixelType, myDimension>  myImageType;

  // Declare the type of the index to access images
  typedef myImageType::IndexType          myIndexType;

  // Declare the type of the size 
  typedef myImageType::SizeType           mySizeType;

  // Declare the type of the Region
  typedef myImageType::RegionType         myRegionType;

  // Declare the type for the filter
  typedef itk::PlaheImageFilter< myImageType > myFilterType;
 
  // Declare the type for the image source
  typedef itk::GaussianImageSource< myImageType > mySourceType;
 
  // Declare the pointers to images
  typedef myImageType::Pointer    myImageTypePointer;
  
  // Declare the pointers to the filter
  typedef myFilterType::Pointer   myFilterTypePointer;

  // Declare the pointers to the image source
  typedef mySourceType::Pointer   mySourceTypePointer;

  // Create the image source
  mySourceTypePointer imageSource = mySourceType::New();
  
  // Define their size, and start index
  unsigned long usize[ myDimension ];
  usize[0] = 20;
  usize[1] = 20;

  mySizeType size;
  size[0] = usize[0];
  size[1] = usize[1];

  myIndexType start;
  start[0] = 0;
  start[1] = 0;

  float spacing[ myDimension ];
  spacing[0] = 1.0;
  spacing[1] = 1.0;

  float origin[ myDimension ];
  origin[0] = 1.0;
  origin[1] = 1.0;

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  imageSource->SetSize( usize );
  imageSource->SetOrigin( origin );
  imageSource->SetSpacing( spacing );

  // Create the Filter                                
  myFilterTypePointer filter = myFilterType::New();

  // window[] is a neighborhood of a evaluated pixel
  unsigned int pixelWindow[ myDimension ];
  pixelWindow[0] = 9;
  pixelWindow[1] = 9;

  filter->SetWindow( pixelWindow );
  filter->SetAlpha( 0.3 );
  filter->SetBeta(  0.3 );

  // Connect the input images
  filter->SetInput( imageSource->GetOutput() ); 

  // Get the Smart Pointer to the Filter Output 
  myImageTypePointer outputImage = filter->GetOutput();
  
  // Execute the filter
  filter->Update();

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}




