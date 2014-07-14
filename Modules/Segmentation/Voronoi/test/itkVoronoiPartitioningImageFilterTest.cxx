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
#include "itkFilterWatcher.h"
#include "itkUnaryFunctorImageFilter.h"
#include "itkScalarToRGBPixelFunctor.h"

int itkVoronoiPartitioningImageFilterTest(int argc, char* argv[])
{
  typedef itk::Image<float,2>          FloatImage;

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
  // FilterWatcher gaussian3Watcher(gaussian3);
  gaussian3->SetInput(original->GetOutput());
  gaussian3->SetVariance(1.0);
  //gaussian3->UpdateLargestPossibleRegion();

  // Try the Voronoi partitioning
  //
  typedef itk::VoronoiPartitioningImageFilter<FloatImage, FloatImage>
    VoronoiSegmentationType;

  VoronoiSegmentationType::Pointer voronoi=VoronoiSegmentationType::New();
  voronoi->SetInput( gaussian3->GetOutput() );
  voronoi->SetNumberOfSeeds( 6 );
  voronoi->SetOutputBoundary( atoi(argv[3]) == 1 );
  voronoi->SetSteps( 7 );
  voronoi->SetSigmaThreshold( 4.0 );
  voronoi->SetMinRegion( 10 );
  voronoi->InteractiveSegmentationOn();

  FilterWatcher voronoiWatcher(voronoi);

  // Write out an image of the voronoi diagram
  typedef itk::RGBPixel<unsigned char>   RGBPixelType;
  typedef itk::Image<RGBPixelType, 2>    RGBImageType;
  typedef itk::Functor::ScalarToRGBPixelFunctor<float>
                                         ColorMapFunctorType;
  typedef itk::UnaryFunctorImageFilter<FloatImage,
    RGBImageType, ColorMapFunctorType>   ColorMapFilterType;
  typedef itk::ImageFileWriter<  RGBImageType  >
                                         WriterType;

  WriterType::Pointer writer = WriterType::New();
  ColorMapFilterType::Pointer colormapper = ColorMapFilterType::New();

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
