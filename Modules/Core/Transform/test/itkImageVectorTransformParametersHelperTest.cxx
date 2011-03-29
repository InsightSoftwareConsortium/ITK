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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkTransformParameters.h"
#include "itkImageVectorTransformParametersHelper.h"

using namespace itk;

namespace{

typedef double                                        ValueType;
const   unsigned int                                  ImageDimension = 2;
const   unsigned int                                  VectorDimension = 4;
typedef itk::Vector< ValueType, VectorDimension >     VectorPixelType;
typedef itk::Image< VectorPixelType, ImageDimension > ImageVectorType;
typedef ImageVectorType::Pointer                      ImageVectorPointer;
typedef ImageVectorType::RegionType                   RegionType;
typedef RegionType::SizeType                          SizeType;
typedef ImageVectorType::IndexType                    IndexType;
typedef ImageVectorType::PixelContainer               VectorPixelContainer;
typedef TransformParameters< ValueType >              TransformParametersType;
typedef ImageVectorTransformParametersHelper< ValueType,
                                        VectorDimension,
                                        ImageDimension >
                                      ImageVectorTransformParametersHelperType;
}

int testMemoryAccess( TransformParametersType& params,
                      ImageVectorPointer imageOfVectors,
                      int dimLength )
{
  int result = EXIT_SUCCESS;

  for (int y = 0; y < dimLength; y++)
    {
    for (int x = 0; x < dimLength; x++)
      {
      IndexType index;
      index[0] = x;
      index[1] = y;

      // The image index returns a N-dim vector, so have to check each
      // element against the values returned by parameter object.
      unsigned long offset = (x + y * dimLength) * VectorDimension;
      VectorPixelType vectorpixel = imageOfVectors->GetPixel( index );
      for(unsigned int ind=0; ind < VectorDimension; ind++)
        {
        ValueType paramsValue = params[offset+ind];
        if( vectorpixel[ind] != paramsValue )
          {
          std::cout << "VectorImage pixel value does not match params value."
                    << "vectorpixel[" << ind << "]: " << vectorpixel[ind]
                    << std::endl
                    << "params[" << offset+ind << "]: "
                    << paramsValue << std::endl;
          result = EXIT_FAILURE;
          }
        }
      }
    }
  return result;
}

/******************************************************/

int itkImageVectorTransformParametersHelperTest(int, char *[])
{
  int result = EXIT_SUCCESS;

  ImageVectorPointer imageOfVectors = ImageVectorType::New();

  IndexType start;
  start.Fill( 0 );

  SizeType size;
  const int dimLength = 3;
  size.Fill( dimLength );

  RegionType region;
  region.SetSize( size );
  region.SetIndex( start );

  imageOfVectors->SetRegions( region );
  imageOfVectors->Allocate();

  ImageVectorType::PointType     origin;
  ImageVectorType::SpacingType   spacing;

  origin.Fill( 0.0 );
  spacing.Fill( 1.0 );

  imageOfVectors->SetOrigin( origin );
  imageOfVectors->SetSpacing( spacing );

  ValueType vectorinitvalues[VectorDimension] = {0.0, 0.1, 0.2, 0.3};
  VectorPixelType vectorvalues(vectorinitvalues);

  //
  // Fill up the image values with the function
  //
  //   Intensity = f(x,y) = x + 3 * y
  //
  //
  for (int y = 0; y < dimLength; y++)
    {
    for (int x = 0; x < dimLength; x++)
      {
      IndexType index;
      index[0] = x;
      index[1] = y;

      const ValueType value = x + y * dimLength;

      VectorPixelType & vectorpixel = imageOfVectors->GetPixel( index );
      vectorpixel.Fill( value );
      vectorpixel += vectorvalues;

      std::cout << value << " ";
      }
    std::cout << std::endl;
    }

  // Create a parameter object and assign the ImageVector helper.
  TransformParametersType params;
  ImageVectorTransformParametersHelperType* imageVectorParamsHelper =
    new ImageVectorTransformParametersHelperType;
  //Assign the helper to the parameter object.
  //TransformParameters object will manage the helper once its been set.
  params.SetHelper( imageVectorParamsHelper );
  //Set the image in the helper. It will point the parameter data pointer
  // to the image data.
  params.SetParametersObject( imageOfVectors );

  result = testMemoryAccess( params, imageOfVectors, dimLength );

  return result;
}
