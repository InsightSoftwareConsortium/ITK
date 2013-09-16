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
#include <iostream>

#include "itkDiscreteGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

template<typename TIMAGE>
int itkDiscreteGaussianImageFilterTestA(int argc, char* argv[])
{
    if( argc < 6 )
      {
      std::cerr << "Missing Arguments" << std::endl;
      return EXIT_FAILURE;
      }

    typedef TIMAGE ImageType;

    const char * input_file_name = argv[3];
    float sigma = atof(argv[4]);
    const char * output_file_name = argv[5];

    typedef itk::ImageFileReader<ImageType> ReaderType;
    typename ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(input_file_name);

    typedef itk::DiscreteGaussianImageFilter<ImageType, ImageType> FilterType;

    typename FilterType::Pointer filter = FilterType::New();
    filter->SetInput(reader->GetOutput());
    filter->SetVariance(sigma);
    filter->Update();

    typedef itk::ImageFileWriter<ImageType> WriterType;

    typename WriterType::Pointer writer = WriterType::New();
    writer->SetFileName(output_file_name);
    writer->SetInput(filter->GetOutput());
    writer->UseCompressionOn();
    writer->Update();

    return EXIT_SUCCESS;
}

int itkDiscreteGaussianImageFilterTest2(int argc, char *argv[])
{

    if (argc < 3)
      {
      std::cerr
              << "Test DiscreteGaussianImageFilter working on both vector type and scalar type images on one 2D RGB image and 2D scalar image."
              << std::endl
              << "DiscreteGaussianImageFilterTest #image_dim #vector_dim #image_file_name #sigma #output_file_name"
              << std::endl << "e.g. test on a vector image: " << std::endl
              << "DiscreteGaussianImageFilterTest 2 3 ShapesRGB.mha 4.5 ShapesRGB_Smoothed.mha"
              << std::endl << "test on a scalar image: " << std::endl
              << "DiscreteGaussianImageFilterTest 2 3 Shapes.mha 4.5 Shapes_Smoothed.mha"
              << std::endl;
      return EXIT_FAILURE;
      }

    unsigned int img_dim = atoi(argv[1]);
    unsigned int vec_dim = atoi(argv[2]);

    if (img_dim != 2)
      {
      std::cerr << "This test only supports 2D image for demo! exiting ..."
                << std::endl;
      return EXIT_FAILURE;
      }

    if (vec_dim != 1 && vec_dim != 3)
      {
      std::cerr
        << "This test only supports 3-channel image or 1-channel image for demo! Exiting ... "
        << std::endl;
        return EXIT_FAILURE;
      }

    typedef float                           ScalarPixelType;
    typedef itk::Image<ScalarPixelType, 2>  ScalarImageType;
    typedef itk::Vector<ScalarPixelType, 3> VectorPixelType;
    typedef itk::Image<VectorPixelType, 2>  VectorImageType;

    switch (vec_dim)
      {
      case 1:
        itkDiscreteGaussianImageFilterTestA<ScalarImageType> (argc, argv);
        break;
      case 3:
        itkDiscreteGaussianImageFilterTestA<VectorImageType> (argc, argv);
        break;
      }

    return EXIT_SUCCESS;
}
