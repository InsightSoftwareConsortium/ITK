#include <iostream>
//#include "itkImageFileWriter.h"
#include "itkNarrowBandImageFilterBase.h"
#include "itkCastImageFilter.h"
#include "itkCurvatureFlowFunction.h"
#include "itkRandomImageSource.h"
#include "itkAddImageFilter.h"

namespace itk 
{
  template <class TInputImageType, class TOutputImageType>
  class NbTestClass: public NarrowBandImageFilterBase<TInputImageType, TOutputImageType>
  {
     public:
     typedef NbTestClass Self;
    
     typedef NarrowBandImageFilterBase<TInputImageType, TOutputImageType> Superclass;
     typedef SmartPointer<Self> Pointer;
     typedef SmartPointer<const Self> ConstPointer;
     typedef TOutputImageType ImageType;

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

    virtual bool Halt ()
    {
      if (this->GetElapsedIterations()==20) return true;
      else return false;
    }

    virtual void Initialize() {
      typename ImageType::SizeType sz= this->GetInput()->GetRequestedRegion().GetSize();
      typename ImageType::IndexType tl= this->GetInput()->GetRequestedRegion().GetIndex();
      typename Superclass::IndexType in;

      for (in[0]=32+tl[0];in[0]<tl[0]+sz[0];in[0]++)
        for (in[1]=tl[1]+32;in[1]<tl[1]+sz[1];in[1]++)       
          this->InsertNarrowBandNode (in); 
     m_RegionList=m_NarrowBand->SplitBand(this->GetNumberOfThreads());
    }

  };
}

namespace {
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
    accum += vnl_math_sqr( p[j] - center[j] );
    }
  accum = vcl_sqrt( accum );
  return ( accum - radius );

}

}


int itkNarrowBandImageFilterBaseTest(int, char* [])
{
  typedef float PixelType;
  const unsigned int ImageDimension = 2;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Point<double,ImageDimension> PointType;
  
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
  unsigned long tam[2];
  tam[0]=64;
  tam[1]=64;
  randomSource->SetSize(tam);
  randomSource->SetMin(0);
  randomSource->SetMax(10);
  randomSource->Update();
  
  typedef itk::AddImageFilter<ImageType,ImageType,ImageType> AddFilterType;
  AddFilterType::Pointer addFilter = AddFilterType::New();
  addFilter->SetInput1(inputImage);
  addFilter->SetInput2(randomSource->GetOutput());
  addFilter->Update();

  typedef itk::NbTestClass <ImageType,ImageType> FilterType;

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(addFilter->GetOutput());    

  
  try {
  filter->Update();
  /* For Debugging
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( "outputNB.mhd" );
  writer->Write();
 */
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
