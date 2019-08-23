/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVoronoiPartitioningImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkUnaryFunctorImageFilter.h"
#include "itkScalarToRGBPixelFunctor.h"

int itkVoronoiPartitioningImageFilterTest(int argc, char* argv[])
{
  using FloatImage = itk::Image<float,2>;

  if (argc != 4)
    {
    std::cout << "Usage: itkVoronoiPartitioningImageFilterTest input output show_boundaries" << std::endl;
    return EXIT_FAILURE;
    }


  /* ------------------------------------------------
   * Load an image
   */
  itk::ImageFileReader<FloatImage>::Pointer original
    = itk::ImageFileReader<FloatImage>::New();
  original->SetFileName( argv[1] );

  /* -------------------------------------------------
   * Preprocess the image
   */
  itk::DiscreteGaussianImageFilter<FloatImage, FloatImage>::Pointer gaussian3
    = itk::DiscreteGaussianImageFilter<FloatImage, FloatImage>::New();
  // itk::SimpleFilterWatcher gaussian3Watcher(gaussian3);
  gaussian3->SetInput(original->GetOutput());
  gaussian3->SetVariance(1.0);
  //gaussian3->UpdateLargestPossibleRegion();

  // Try the Voronoi partitioning
  //
  using VoronoiSegmentationType =
      itk::VoronoiPartitioningImageFilter<FloatImage, FloatImage>;

  VoronoiSegmentationType::Pointer voronoi=VoronoiSegmentationType::New();
  voronoi->SetInput( gaussian3->GetOutput() );
  voronoi->SetNumberOfSeeds( 6 );
  voronoi->SetOutputBoundary( std::stoi(argv[3]) == 1 );
  voronoi->SetSteps( 7 );
  voronoi->SetSigmaThreshold( 4.0 );
  voronoi->SetMinRegion( 10 );
  voronoi->InteractiveSegmentationOn();

  itk::SimpleFilterWatcher voronoiWatcher(voronoi);

  // Write out an image of the voronoi diagram
  using RGBPixelType = itk::RGBPixel<unsigned char>;
  using RGBImageType = itk::Image<RGBPixelType, 2>;
  using ColormapFunctorType = itk::Functor::ScalarToRGBPixelFunctor<float>;
  using ColormapFilterType = itk::UnaryFunctorImageFilter<FloatImage,
    RGBImageType, ColormapFunctorType>;
  using WriterType = itk::ImageFileWriter<RGBImageType>;

  WriterType::Pointer writer = WriterType::New();
  ColormapFilterType::Pointer colormapper = ColormapFilterType::New();

  try
    {
    colormapper->SetInput( voronoi->GetOutput() );
    writer->SetInput( colormapper->GetOutput() );
    writer->SetFileName( argv[2] );
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }

  //
  return EXIT_SUCCESS;

}
