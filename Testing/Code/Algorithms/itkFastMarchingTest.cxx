/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkFastMarchingTest.cxx
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
#include "itkFastMarchingImageFilter.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkOutputWindow.h"

#include "vnl/vnl_math.h"

// this class is used to send output to stdout and not the itk window
class TextOutput : public itk::OutputWindow
{
public:
  typedef TextOutput              Self;
  typedef itk::SmartPointer<Self>  Pointer;
  typedef itk::SmartPointer<const Self>  ConstPointer;
  itkNewMacro(TextOutput);
  virtual void DisplayText(const char* s)
    { std::cout << s << std::endl; }
};


int main()
{

  itk::OutputWindow::SetInstance(TextOutput::New().GetPointer());

  // create a fastmarching object
  typedef float PixelType;
  typedef itk::Image<PixelType,2> FloatImage;
  typedef itk::FastMarchingImageFilter<FloatImage,FloatImage> FloatFMType;

  FloatFMType::Pointer marcher = FloatFMType::New();
  
  // setup trial points
  typedef FloatFMType::NodeType NodeType;
  typedef FloatFMType::NodeContainer NodeContainer;

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

  speedImage->Print( std::cout );
  marcher->SetSpeedImage( speedImage );

  // turn on debugging
  marcher->DebugOn();

  // update the marcher
  marcher->Update();

  // check the results
  FloatImage::Pointer output = marcher->GetOutput();
  itk::ImageRegionIterator<FloatImage>
    iterator( output, output->GetBufferedRegion() );

  bool passed = true;

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
  std::cout << "SpeedConstant: " << marcher->GetSpeedConstant() << std::endl;
  std::cout << "StoppingValue: " << marcher->GetStoppingValue() << std::endl;
  std::cout << "CollectPoints: " << marcher->GetCollectPoints() << std::endl;

  std::cout << "SpeedImage: " << marcher->GetSpeedImage().GetPointer();
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

}
