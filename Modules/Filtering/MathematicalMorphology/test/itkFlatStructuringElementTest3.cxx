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

#include "itkFlatStructuringElement.h"
#include "itkImageFileWriter.h"

template <class TSEType>
void SEToFile( const TSEType &e, const std::string &fname)
{

  const unsigned int Dimension = TSEType::NeighborhoodDimension;

  typedef itk::Image<unsigned char, Dimension> ImageType;

  typename ImageType::Pointer img = ImageType::New();

  typename ImageType::IndexType start;
  start.Fill(0);

  typename ImageType::SizeType size;
  for (unsigned int i = 0; i < Dimension; ++i)
    {
    size[i] = e.GetRadius()[i]*2+1;
    }


  typename ImageType::RegionType region(start, size);
  img->SetRegions(region);
  img->Allocate();
  img->FillBuffer(0);

  typename TSEType::ConstIterator SEIt;
  itk::ImageRegionIterator<ImageType> it(img, region);

  for( SEIt = e.Begin(); SEIt != e.End(); ++SEIt, ++it )
    {
    it.Set(*SEIt);
    }

  typedef itk::ImageFileWriter<ImageType> WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(fname);
  writer->SetInput(img);
  writer->Update();
}

int itkFlatStructuringElementTest3(int argc, char *argv[])
{
  // test polygon SEs
  if ( argc < 4 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << argv[0] << " OutputImage Radius Lines Dimension" << std::endl;
    return EXIT_FAILURE;
  }

  int dimension = 2;
  std::string outputImage = argv[1];
  int radius = atoi(argv[2]);
  int lines = atoi(argv[3]);
  if ( argc > 4 )
    {
    dimension = atoi(argv[4]);
    }

  if (dimension == 2)
    {
    typedef itk::FlatStructuringElement< 2 > SE2Type;

    SE2Type::RadiusType r2;
    r2.Fill(radius);
    SE2Type P=SE2Type::Polygon( r2,  lines);
    SEToFile(P, outputImage);

    }
  else if (dimension == 3)
    {
    typedef itk::FlatStructuringElement< 3 > SE3Type;

    SE3Type::RadiusType r3;
    r3.Fill(radius);
    SE3Type P=SE3Type::Polygon( r3,  lines);
    SEToFile(P, outputImage);
    }
  else
    {
    std::cerr << "Only 2 and 3 dimensions are supported." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
