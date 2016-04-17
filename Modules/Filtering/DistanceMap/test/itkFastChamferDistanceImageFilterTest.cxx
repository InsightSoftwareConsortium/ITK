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

#include "itkFastChamferDistanceImageFilter.h"
#include "itkMath.h"
// simple signed distance function

namespace {
template <typename TPoint>
double
SimpleSignedDistance( const TPoint & p )
{
  TPoint center;
  center.Fill( 16 );
  double radius = 10;

  double accum = 0.0;
  for( unsigned int j = 0; j < TPoint::PointDimension; j++ )
    {
    accum += static_cast< double >( itk::Math::sqr( p[j] - center[j] ) );
    }
  accum = std::sqrt( accum );
  if (itk::Math::abs(accum - radius) > 1)
    {
    if((accum - radius) > 0)
      {
      return radius;
      }
    else
      {
      return -radius;
      }
    }
  else
    {
    return ( accum - radius );
    }
}

}

template< unsigned int VDimension >
int FastChamferDistanceImageFilterTest( unsigned int iPositive,
                                        unsigned int iNegative,
                                        unsigned int iOther )
{
  std::cout<< "Test ITK Chamfer Distance Image Filter" << std::endl;
  std::cout << "Compute the distance map of a 32^d image" << std::endl;

  typedef float PixelType;

  typedef itk::Image<PixelType,VDimension> ImageType;
  typedef itk::Point<double,VDimension>    PointType;

  typename ImageType::SizeType size;
  size.Fill( 32 );
  typename ImageType::IndexType index;
  index.Fill( 0 );
  typename ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  typename ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  IteratorType it(inputImage,region);

  // Set the image to 0
  while( !it.IsAtEnd() )
    {
    PointType point;
    inputImage->TransformIndexToPhysicalPoint( it.GetIndex(), point );
    it.Set( SimpleSignedDistance( point ) );
    ++it;
    }

  /* Create Fast Chamfer Distance filter */
  typedef itk::FastChamferDistanceImageFilter<
                                ImageType,ImageType> ChamferFilterType;
  typename ChamferFilterType::Pointer filter = ChamferFilterType::New();

  filter->SetInput(inputImage);

  typename ImageType::Pointer outputImage = filter->GetOutput();

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  filter->Print(std::cout);

  //Create NarrowBand
  typedef typename ChamferFilterType::NarrowBandType  NarrowBandType;

  typename NarrowBandType::Pointer band = NarrowBandType::New();
  band->SetTotalRadius(4);
  band->SetInnerRadius(2);
  filter->SetMaximumDistance(5);
  std::cout<<"Band initial size: "<<band->Size()<<std::endl;
  filter->SetNarrowBand(band.GetPointer());
  filter->Update();

  std::cout<<"Band size: "<<band->Size()<<std::endl;

  //Loop through the band
  typedef typename NarrowBandType::ConstIterator itNBType;
  itNBType itNB = band->Begin();
  itNBType itNBend = band->End();

//  BandNodeType *tmp;
  unsigned int innerpositive=0;
  unsigned int innernegative=0;
  unsigned int otherpoints=0;
  while( itNB != itNBend )
    {
    if(itNB->m_NodeState == 3)
      {
      innerpositive++;
      }
    else if(itNB->m_NodeState == 2)
      {
      innernegative++;
      }
    else
      {
      otherpoints++;
      }
    ++itNB;
    }
  int returnVal(EXIT_SUCCESS);
  if( innerpositive != iPositive )
    {
    std::cout << "Inner positive points: " << innerpositive << " != " << iPositive << std::endl;
    returnVal = EXIT_FAILURE;
    }

  if( innernegative != iNegative )
    {
    std::cout << "Inner negative points: " << innernegative << " != " << iNegative << std::endl;
    returnVal = EXIT_FAILURE;
    }

  if( otherpoints != iOther )
    {
    std::cout << "Rest of points: " << otherpoints << " != " << iOther << std::endl;
    returnVal = EXIT_FAILURE;
    }

  //Exercising filter methods
  float inweights[VDimension];
  for( unsigned int dim = 0; dim < VDimension; dim++ )
    {
    if( dim == 0 )
      {
      inweights[dim] = 0.926f;
      }
    else if( dim == 1 )
      {
      inweights[dim] = 1.34f;
      }
    else
      {
      inweights[dim] = 0.f;
      }
    }
  filter->SetWeights(inweights);
  const float *outweights =  filter->GetWeights().GetDataPointer();

  std::cout << "outweights = " << outweights << std::endl;

  if( itk::Math::NotAlmostEquals( filter->GetMaximumDistance(), 5 ) )
    {
    std::cout << "filter->GetMaximumDistance() != 5" <<std::endl;
    returnVal = EXIT_FAILURE;
    }
  /* For debugging write the result
  typedef itk::ImageFileWriter< ImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName("chamferoutput.mhd");
  writer->SetInput(filter->GetOutput());
  writer->Update();
  */

  if(returnVal == EXIT_SUCCESS)
    {
    std::cout << "Test passed" << std::endl;
    }
  return returnVal;
}

int itkFastChamferDistanceImageFilterTest( int argc, char* argv[] )
{
  if( argc != 2 )
    {
    std::cout << "This test requires at least one argument (the image dimension)" <<std::endl;
    return EXIT_FAILURE;
    }

  int Dimension = atoi( argv[1] );

  std::cout <<"Dimension = " << Dimension << std::endl;
  if( Dimension == 1 )
    {
    return FastChamferDistanceImageFilterTest< 1 >( 4, 6, 8 );
    }
  if( Dimension == 2 )
    {
    return FastChamferDistanceImageFilterTest< 2 >( 144, 124, 256 );
    }
  if( Dimension == 3 )
    {
    return FastChamferDistanceImageFilterTest< 3 >( 3068, 2066, 5520 );
    }
  if( Dimension == 4 )
    {
    return FastChamferDistanceImageFilterTest< 4 >( 49472, 28928, 93136 );
    }
  return EXIT_FAILURE;
}
