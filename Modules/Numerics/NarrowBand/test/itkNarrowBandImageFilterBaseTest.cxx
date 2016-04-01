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
#include "itkImageFileWriter.h"
#include "itkNarrowBandImageFilterBase.h"
#include "itkCurvatureFlowFunction.h"
#include "itkRandomImageSource.h"
#include "itkAddImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"

namespace itk
{
  template <typename TInputImageType, typename TOutputImageType>
  class NbTestClass: public NarrowBandImageFilterBase<TInputImageType, TOutputImageType>
  {
     public:
     typedef NbTestClass Self;

     typedef NarrowBandImageFilterBase<TInputImageType, TOutputImageType> Superclass;
     typedef SmartPointer<Self>       Pointer;
     typedef SmartPointer<const Self> ConstPointer;
     typedef TOutputImageType         ImageType;

    /** Standard method for creation through object factory. */
        itkNewMacro(Self);

    /** Run-time class information. */
     itkTypeMacro(NbTestClass,NarrowBandImageFilterBase);

     typedef CurvatureFlowFunction<TOutputImageType> FiniteFunctionType;


  protected:
    typename FiniteFunctionType::Pointer m_Function;

    NbTestClass ()
    {
      m_Function=FiniteFunctionType::New();
      this->SetDifferenceFunction(m_Function);
    }

    virtual bool Halt () ITK_OVERRIDE
    {
      if (this->GetElapsedIterations() == 20)
        {
        return true;
        }
      else
        {
        return false;
        }
    }

    virtual void CreateNarrowBand() ITK_OVERRIDE
      {
      //Create a band
      typename ImageType::SizeType sz= this->GetInput()->GetRequestedRegion().GetSize();
      typename ImageType::IndexType tl= this->GetInput()->GetRequestedRegion().GetIndex();
      typename Superclass::IndexType in;

      for (in[0]=32+tl[0]; in[0]<tl[0]+(long int)(sz[0]); in[0]++)
        {
        for (in[1]=tl[1]+32; in[1]<tl[1]+(long int)(sz[1]);in[1]++)
          {
          this->InsertNarrowBandNode (in);
          }
        }
      }
  };
}

namespace
{
// simple signed distance function
template <typename TPoint>
double
SimpleSignedDistance( const TPoint & p )
{
  TPoint center;
  center.Fill( 32 );
  double radius = 19.5;

  double accum = 0.0;
  for( unsigned int j = 0; j < TPoint::PointDimension; j++ )
    {
    accum += itk::Math::sqr( p[j] - center[j] );
    }
  accum = std::sqrt( accum );
  return ( accum - radius );

}
}


int itkNarrowBandImageFilterBaseTest(int argc, char* argv[])
{
  if(argc < 2)
    {
    std::cerr << "Usage: " << argv[0] << " OutputImage\n";
    return EXIT_FAILURE;
    }

  typedef float         PixelType;
  typedef unsigned char WriterPixelType;
  const unsigned int ImageDimension = 2;
  typedef itk::Image<PixelType,ImageDimension>       ImageType;
  typedef itk::Image<WriterPixelType,ImageDimension> WriterImageType;
  typedef itk::Point<double,ImageDimension>          PointType;

  ImageType::SizeType size = {{64,64}};
  ImageType::IndexType index = {{0,0}};
  ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator iter( inputImage, region );
  iter.GoToBegin();

  while( !iter.IsAtEnd() )
    {
    PointType point;
    inputImage->TransformIndexToPhysicalPoint( iter.GetIndex(), point );
    iter.Set( SimpleSignedDistance( point ) );
    ++iter;
    }

  typedef itk::RandomImageSource<ImageType> RandomSourceType;
  RandomSourceType::Pointer randomSource = RandomSourceType::New();
  ImageType::SizeValueType tam[2];
  tam[0]=64;
  tam[1]=64;
  randomSource->SetSize(tam);
  randomSource->SetMin(-2);
  randomSource->SetMax(2);
  //  For testing purposes we want random source to produce
  //  deterministic values. This is accompished by restricting the
  //  number of threads to 1.
  randomSource->SetNumberOfThreads(1);

  typedef itk::AddImageFilter<ImageType,ImageType,ImageType> AddFilterType;
  AddFilterType::Pointer addFilter = AddFilterType::New();
  addFilter->SetInput1(inputImage);
  addFilter->SetInput2(randomSource->GetOutput());

  typedef itk::NbTestClass <ImageType,ImageType> FilterType;

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(addFilter->GetOutput());
  filter->Print(std::cout);
  try
    {
    typedef itk::RescaleIntensityImageFilter<ImageType, WriterImageType> RescaleType;
    RescaleType::Pointer rescale = RescaleType::New();
    rescale->SetInput(filter->GetOutput());
    rescale->SetOutputMinimum(0);
    rescale->SetOutputMaximum(255);

    typedef itk::ImageFileWriter<WriterImageType> WriterType;
    WriterType::Pointer writer = WriterType::New();
    writer->SetInput( rescale->GetOutput() );
    writer->SetFileName( argv[1] );
    writer->Write();
    }
  catch (itk::ExceptionObject &err)
    {
    (&err)->Print(std::cerr);
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test Passed. " << std::endl;
  return EXIT_SUCCESS;

}
