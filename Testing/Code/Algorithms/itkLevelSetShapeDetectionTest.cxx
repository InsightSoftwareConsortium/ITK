/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkLevelSetShapeDetectionTest.cxx
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
#include "itkShapeDetectionLevelSetFilter.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"

int main()
{

  /* -------------------------------------------------
   * Create an initial level set of dimension 64x64
   * with the zero set being a circle of radius 8
   * centered at (28,35).
   */

  // create a fastmarching object
  typedef itk::Image<float,2> FloatImage;
  typedef itk::FastMarchingImageFilter<FloatImage> FastMarcherType;

  FastMarcherType::Pointer marcher = FastMarcherType::New();
  
  // setup trial points
  typedef FastMarcherType::NodeType NodeType;
  typedef FastMarcherType::NodeContainer NodeContainer;

  NodeContainer::Pointer trialPoints = NodeContainer::New();

  NodeType node;

  FloatImage::IndexType index0 = {{28,35}};
  
  node.value = 0.0;
  node.index = index0;
  trialPoints->InsertElement(0, node);
  
  marcher->SetTrialPoints( trialPoints );

  // specify the size of the output image
  FloatImage::SizeType size = {{64,64}};
  marcher->SetOutputSize( size );

  // update the marcher
  marcher->Update();

  // walk the marcher output
  FloatImage::Pointer levelSet = marcher->GetOutput();
  itk::ImageRegionIterator<FloatImage>
    iterator( levelSet, levelSet->GetBufferedRegion() );

  for( ; !iterator.IsAtEnd(); ++iterator )
    {
    iterator.Set( iterator.Get() - 8.0 );
    }


  /* -------------------------------------------------
   * Create a edge potential image with all ones
   */
  FloatImage::Pointer edgeImg = FloatImage::New();
  edgeImg->CopyInformation( levelSet );
  edgeImg->SetBufferedRegion(
    levelSet->GetBufferedRegion() );
  edgeImg->Allocate();

  itk::ImageRegionIterator<FloatImage>
    edgeIter( edgeImg, edgeImg->GetBufferedRegion() );

  for(; !edgeIter.IsAtEnd(); ++edgeIter )
    {
    edgeIter.Set( 1.0 );
    }

  /* -----------------------------------------------
   * Create a shape detection object
   */

  typedef itk::ShapeDetectionLevelSetFilter<FloatImage,FloatImage>
    ShapeDetectorType;

  ShapeDetectorType::Pointer detector = ShapeDetectorType::New();

  detector->SetInput( levelSet );
  detector->SetEdgeImage( edgeImg );
  detector->SetNumberOfIterations( 10 );

  /* -------------------------------------------------
   * Test the full-band version of the algorithm
   */
  std::cout << "Running the full-band version" << std::endl;
  detector->Update();


  /* -------------------------------------------------
   * Test the narrow-band version of the algorithm
   */
  std::cout << "Running the narrow-band version" << std::endl;
  detector->NarrowBandingOn();
  detector->SetNarrowBandwidth( 10.0 );
  detector->Update();

  return EXIT_SUCCESS;

}
