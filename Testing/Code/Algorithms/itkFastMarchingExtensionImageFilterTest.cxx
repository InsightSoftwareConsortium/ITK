/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkFastMarchingExtensionImageFilterTest.cxx
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
#include "itkFastMarchingExtensionImageFilter.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"

#include "vnl/vnl_math.h"

int main()
{
  // create a fastmarching object
  typedef float PixelType;
  typedef itk::Image<PixelType,2> FloatImage;
  typedef itk::FastMarchingExtensionImageFilter<FloatImage,unsigned char,1,
    FloatImage> MarcherType;

  MarcherType::Pointer marcher = MarcherType::New();
  bool passed;
  
  // setup trial points
  typedef MarcherType::NodeType NodeType;
  typedef MarcherType::NodeContainer NodeContainer;

  NodeContainer::Pointer trialPoints = NodeContainer::New();

  NodeType node;

  FloatImage::OffsetType offset0 = {{28,35}};
  
  node.SetValue( 0.0 );
  node.SetIndex( itk::Index<2>::ZeroIndex + offset0 );
  trialPoints->InsertElement(0, node);
  
  marcher->SetTrialPoints( trialPoints );

  // specify the size of the output image
  FloatImage::SizeType size = {{64,64}};
  marcher->SetOutputSize( size );

  // setup a speed image of ones
  FloatImage::Pointer speedImage = FloatImage::New();
  FloatImage::RegionType region;
  region.SetSize( size );
  speedImage->SetLargestPossibleRegion( region );
  speedImage->SetBufferedRegion( region );
  speedImage->Allocate();

  itk::ImageRegionIterator<FloatImage>
    speedIter( speedImage, speedImage->GetBufferedRegion() );
  for ( ; !speedIter.IsAtEnd(); ++speedIter )
    {
    speedIter.Set( 1.0 );
    }

  marcher->SetSpeedImage( speedImage );

  // deliberately cause an exception by not setting AuxTrialValues
  passed = false;
  try
    {
    marcher->Update();
    }
  catch ( itk::ExceptionObject err )
    {
    passed = true;
    std::cout << err << std::endl;
    }
  if ( !passed ) { return EXIT_FAILURE; }

  typedef MarcherType::AuxValueVectorType VectorType;
  typedef MarcherType::AuxValueContainer AuxValueContainer;

  AuxValueContainer::Pointer auxTrialValues = AuxValueContainer::New();

  // deliberately cause an exception setting AuxTrialValues of the wrong size
  marcher->SetAuxiliaryTrialValues( auxTrialValues );

  passed = false;
  try
    {
    marcher->Update();
    }
  catch ( itk::ExceptionObject err )
    {
    passed = true;
    std::cout << err << std::endl;
    }
  if ( !passed ) { return EXIT_FAILURE; }


  VectorType vector;
  vector[0] = 48;

  auxTrialValues->InsertElement(0,vector);

  // run the algorithm
  passed = true;
  try
    {
    marcher->Update();
    }
  catch ( itk::ExceptionObject err )
    {
    passed = false;
    std::cout << err << std::endl;
    }
  if ( !passed ) { return EXIT_FAILURE; }


  // check the results
  passed = true;
  FloatImage::Pointer output = marcher->GetOutput();
  itk::ImageRegionIterator<FloatImage>
    iterator( output, output->GetBufferedRegion() );


  for ( ; !iterator.IsAtEnd(); ++iterator )
    {

    FloatImage::IndexType tempIndex;
    double distance;
    float output;

    tempIndex = iterator.GetIndex();
    tempIndex -= offset0;
    distance = 0.0;
    for ( int j = 0; j < 2; j++ )
      {
      distance += tempIndex[j] * tempIndex[j];
      }
    distance = vnl_math_sqrt( distance );

    output = (float) iterator.Get();

    if ( vnl_math_abs( output ) / distance > 1.42 )
      {
      std::cout << iterator.GetIndex() << " ";
      std::cout << vnl_math_abs( output ) / distance << " ";
      std::cout << vnl_math_abs( output ) << " " << distance << std::endl;
      passed = false;
      }
    
    }

  // Exercise other member functions
  std::cout << "Auxiliary alive values: " << marcher->GetAuxiliaryAliveValues().GetPointer();
  std::cout << std::endl;

  std::cout << "Auxiliary trial values: " << marcher->GetAuxiliaryTrialValues().GetPointer();
  std::cout << std::endl;

  marcher->Print( std::cout );

  if ( passed )
    {
    std::cout << "Fast Marching test passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "Fast Marching test failed" << std::endl;
    return EXIT_FAILURE;
    }


return EXIT_SUCCESS;

}

