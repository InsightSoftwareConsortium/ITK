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
#include "itkAffineTransform.h"
#include "itkBSplineTransform.h"
#include "itkTransformToStrainFilter.h"
#include "itkImageFileWriter.h"

int
itkTransformToStrainFilterTest(int argc, char * argv[])
{
  /** Check command line arguments. */
  if (argc < 3)
  {
    std::cerr << "Usage: ";
    std::cerr << argv[0] << "<transformName> <strainFieldFileName> [bSplineParametersFile]" << std::endl;
    return EXIT_FAILURE;
  }

  const std::string transformName = argv[1];
  const std::string strainFieldFileName = argv[2];
  std::string       bSplineParametersFile;
  if (argc > 3)
  {
    bSplineParametersFile = argv[3];
  }

  /** Typedefs. */
  const unsigned int Dimension = 2;
  typedef float      ScalarPixelType;
  typedef double     CoordRepresentationType;
  const unsigned int SplineOrder = 3;

  typedef itk::Transform<CoordRepresentationType, Dimension, Dimension>          TransformType;
  typedef itk::AffineTransform<CoordRepresentationType, Dimension>               AffineTransformType;
  typedef itk::BSplineTransform<CoordRepresentationType, Dimension, SplineOrder> BSplineTransformType;
  typedef TransformType::ParametersType                                          ParametersType;

  typedef itk::TransformToStrainFilter<TransformType, ScalarPixelType, ScalarPixelType> TransformToStrainFilterType;

  // Create output information.
  typedef TransformToStrainFilterType::SizeType SizeType;
  SizeType                                      size;
  size.Fill(20);

  typedef TransformToStrainFilterType::SpacingType SpacingType;
  SpacingType                                      spacing;
  spacing.Fill(0.7);

  typedef TransformToStrainFilterType::PointType OriginType;
  OriginType                                     origin;
  origin.Fill(-10.0);

  // Create transforms.
  AffineTransformType::Pointer  affineTransform = AffineTransformType::New();
  BSplineTransformType::Pointer bSplineTransform = BSplineTransformType::New();
  // if ( transformName == "Affine" )
  //{
  //[>* Set the options. <]
  // OriginType centerOfRotation;
  // centerOfRotation[ 0 ] = -3.0; centerOfRotation[ 1 ] = -3.0;
  // affineTransform->SetCenter( centerOfRotation );

  //[>* Create and set parameters. <]
  // ParametersType parameters( affineTransform->GetNumberOfParameters() );
  // parameters[ 0 ] =   1.1;
  // parameters[ 1 ] =   0.1;
  // parameters[ 2 ] =  -0.2;
  // parameters[ 3 ] =   0.9;
  // parameters[ 4 ] =  10.3;
  // parameters[ 5 ] = -33.8;
  // affineTransform->SetParameters( parameters );
  //}
  // else if ( transformName == "BSpline" )
  if (transformName == "BSpline")
  {
    /* Set the options. */
    BSplineTransformType::PhysicalDimensionsType dimensions;
    for (unsigned int dd = 0; dd < Dimension; ++dd)
    {
      dimensions[dd] = spacing[dd] * (size[dd] - 1.0);
    }
    BSplineTransformType::MeshSizeType  meshSize;
    BSplineTransformType::DirectionType direction;
    direction.SetIdentity();

    meshSize[0] = 7 - SplineOrder;
    meshSize[1] = 10 - SplineOrder;

    bSplineTransform->SetTransformDomainOrigin(origin);
    bSplineTransform->SetTransformDomainPhysicalDimensions(dimensions);
    bSplineTransform->SetTransformDomainMeshSize(meshSize);
    bSplineTransform->SetTransformDomainDirection(direction);

    // Create and set parameters.
    ParametersType parameters(bSplineTransform->GetNumberOfParameters());
    std::ifstream  input(bSplineParametersFile.c_str());
    if (input.is_open())
    {
      for (unsigned int i = 0; i < parameters.GetSize(); ++i)
      {
        input >> parameters[i];
      }
      input.close();
    }
    else
    {
      std::cerr << "ERROR: B-spline parameter file not found." << std::endl;
      return EXIT_FAILURE;
    }
    bSplineTransform->SetParametersByValue(parameters);
  }
  else
  {
    std::cerr << "ERROR: Not a valid transform." << std::endl;
    return EXIT_FAILURE;
  }

  // Create and setup strain field generator.
  TransformToStrainFilterType::Pointer transformToStrainFilter = TransformToStrainFilterType::New();
  std::cout << "Name of Class: " << transformToStrainFilter->GetNameOfClass() << std::endl;
  transformToStrainFilter->SetSize(size);
  transformToStrainFilter->SetSpacing(spacing);
  transformToStrainFilter->SetOrigin(origin);

  // for coverage, exercise access methods
  spacing = transformToStrainFilter->GetSpacing();
  origin = transformToStrainFilter->GetOrigin();
  TransformToStrainFilterType::DirectionType direction = transformToStrainFilter->GetDirection();
  transformToStrainFilter->SetDirection(direction);
  std::cout << "Spacing   " << spacing << std::endl
            << "Origin    " << origin << std::endl
            << "Direction \n"
            << direction << std::endl;
  if (transformName == "Affine")
  {
    transformToStrainFilter->SetTransform(affineTransform);
  }
  else if (transformName == "BSpline")
  {
    transformToStrainFilter->SetTransform(bSplineTransform);
  }
  std::cout << "Transform: " << transformToStrainFilter->GetTransform() << std::endl;

  // Write strain field to disk.
  typedef itk::ImageFileWriter<TransformToStrainFilterType::OutputImageType> WriterType;
  WriterType::Pointer                                                        writer = WriterType::New();
  writer->SetInput(transformToStrainFilter->GetOutput());
  writer->SetFileName(strainFieldFileName);

  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "Exception detected while generating strain field" << strainFieldFileName << std::endl;
    std::cerr << " : " << err << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
