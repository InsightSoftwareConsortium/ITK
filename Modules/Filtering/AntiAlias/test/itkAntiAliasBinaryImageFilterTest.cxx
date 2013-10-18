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

#include "itkAntiAliasBinaryImageFilter.h"

#include "itkTestingMacros.h"
#include "itkImageFileWriter.h"

//
// This is a simple test of the AntiAliasBinaryImageFilter class.  It creates a
// binary image of a sphere, then applies the filter.  If you want to view the
// output, uncomment the lines which write the binary file.  Output will be the
// floating point distance transform from the surface, which can be directly
// rendered using Marching Cubes or some other suitable algorithm.  See
// documentation for AntiAliasBinaryImageFilter and
// SparseFieldLevelSetImageFilter for more information.
//
namespace AntiAliasBinaryImageFilterTestNamespace {

const int V_WIDTH  = 64;
const int V_HEIGHT = 64;
const int V_DEPTH  = 64;

float sphere(float x, float y, float z)
{
    float dis;
    dis = (x - (float)V_WIDTH/2.0)*(x - (float)V_WIDTH/2.0)
      /((0.2f*V_WIDTH)*(0.2f*V_WIDTH)) +
      (y - (float)V_HEIGHT/2.0)*(y - (float)V_HEIGHT/2.0)
      /((0.2f*V_HEIGHT)*(0.2f*V_HEIGHT)) +
      (z - (float)V_DEPTH/2.0)*(z - (float)V_DEPTH/2.0)
      /((0.2f*V_DEPTH)*(0.2f*V_DEPTH));
    return(1.0f-dis);
}

void evaluate_function(itk::Image<char, 3> *im,
          float (*f)(float, float, float) )
{
  itk::Image<char, 3>::IndexType idx;

  for(int z = 0; z < V_DEPTH; ++z)
    {
      idx[2] = z;
      for (int y = 0; y < V_HEIGHT; ++y)
        {
          idx[1] = y;
          for (int x = 0; x < V_WIDTH; ++x)
            {
              idx[0] = x;
              if ( f((float)x,(float)y,(float)z) >= 0.0 )
                {  im->SetPixel(idx, 1 ); }
              else
                {  im->SetPixel(idx, 0 ); }
            }
        }
    }
}

} // end namespace

int itkAntiAliasBinaryImageFilterTest(int argc, char * argv [] )
{
  if( argc < 2 )
    {
    std::cerr << "Missing arguments." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " outputImage " << std::endl;
    return EXIT_FAILURE;
    }
  const char * outputImage = argv[1];

  typedef char                         InputDataType;
  typedef itk::Image<InputDataType, 3> BinaryImageType;
  typedef itk::Image<float, 3>         RealImageType;

  itk::AntiAliasBinaryImageFilter<BinaryImageType, RealImageType>::Pointer
    antialiaser = itk::AntiAliasBinaryImageFilter<BinaryImageType, RealImageType>::New();

  // Create a binary image of a sphere.
  BinaryImageType::Pointer image = BinaryImageType::New();
  BinaryImageType::RegionType region;
  BinaryImageType::RegionType::SizeType sz;
  BinaryImageType::RegionType::IndexType idx;
  for (unsigned k = 0; k < 3; k++)
    {
    sz[k] = 64;
    idx[k] = 0;
    }
  region.SetSize(sz);
  region.SetIndex(idx);
  image->SetRegions(region);
  image->Allocate();
  AntiAliasBinaryImageFilterTestNamespace::evaluate_function(image,
                             AntiAliasBinaryImageFilterTestNamespace::sphere);

  // Set up and apply the filter.
  antialiaser->SetInput(image);
  antialiaser->SetMaximumRMSError(0.02);

  // For increased code coverage.  Does nothing.
  antialiaser->GetMaximumRMSError();


  // Generally a good idea to set this value as a safeguard against infinte
  // loops if the MaximumRMSError has been set too low.
  antialiaser->SetNumberOfIterations(100);

  TRY_EXPECT_NO_EXCEPTION( antialiaser->Update() );

  std::cout << "Maximum RMS change value threshold was: 0.02 " << std::endl;
  std::cout << "Last RMS change value was: " << antialiaser->GetRMSChange() << std::endl;
  if( antialiaser->GetRMSChange() > 0.02 )
    {
    std::cerr << "Unexpected RMSChange." << std::endl;
    return EXIT_FAILURE;
    }

  if (antialiaser->GetElapsedIterations() >= 100)
    {
    std::cout << "ERROR: Filter did not converge.";
    return EXIT_FAILURE;
    }
  else
    {
    typedef itk::ImageFileWriter<RealImageType> WriterType;
    WriterType::Pointer writer = WriterType::New();
    writer->SetInput( antialiaser->GetOutput() );
    writer->SetFileName( outputImage );
    TRY_EXPECT_NO_EXCEPTION( writer->Update() );

    // Repeat just to make sure we reinitialize properly.
    antialiaser->SetNumberOfIterations(200);
    antialiaser->Update();

    antialiaser->GetLowerBinaryValue();
    antialiaser->GetUpperBinaryValue();

    std::cout << "Maximum RMS change value threshold was: 0.02 " << std::endl;
    std::cout << "Last RMS change value was: " << antialiaser->GetRMSChange() << std::endl;

    if( antialiaser->GetRMSChange() > 0.02 )
      {
      std::cerr << "Unexpected RMSChange." << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}
