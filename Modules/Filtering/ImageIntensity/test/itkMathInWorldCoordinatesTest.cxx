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
#include "itkImage.h"
#include "itkVector.h"
#include "itkAddImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkDivideImageFilter.h"
#include "itkTernaryAddImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "vcl_cmath.h"


/** SetPixel template function -- allows same test template function
 *  to handle scalar and vector images
 */
template <typename TScalar>
void SetPixelValue(TScalar &pixel,float value)
{
  pixel = value;
}
template <typename TScalar, unsigned VDim>
void SetPixelValue(itk::Vector<TScalar,VDim> &pixel, float value)
{
  for(unsigned i = 0; i < VDim; i++)
    {
    pixel.SetNthComponent(i,value);
    }
}

/** IsSame template function -- compares either scalar or vector values
 */
template <typename TScalar, unsigned VDim>
bool IsSame(const itk::Vector<TScalar,VDim> &a, const itk::Vector<TScalar,VDim> &b)
{
  return (a - b).GetNorm() < 0.0001;
}

template <typename TScalar>
bool IsSame(const TScalar &a, const TScalar &b)
{
  return ( vcl_abs(a - b) < 0.0001 );

}

/** \class SetThirdFilterInput
 * A  partial specialization, needed to handle both
 * Binary- and Ternary- Functor Image filters in the same templated
 * test function.
 */
template <class TTernaryFilter,class TImage>
class SetThirdFilterInput
{
public:
  static void Set(typename TTernaryFilter::Pointer &,typename TImage::Pointer &)
    {}
};

template<class TImage>
class SetThirdFilterInput<itk::TernaryAddImageFilter<TImage,TImage,TImage,TImage>,TImage>
{
public:
  typedef typename itk::TernaryAddImageFilter<TImage,TImage,TImage,TImage>::Pointer
  FilterTypePointer;
  static void Set(FilterTypePointer &filter,typename TImage::Pointer &ptr)
    {
      filter->SetInput3(ptr);
    }
};

template <class TImage,class TMathFilter,unsigned VDimension>
int itkMathOpTest(float valA, float valB, float valC, typename TImage::PixelType expectedResult)
{

  // Declare the type of the index to access images
  typedef itk::Index<VDimension>         myIndexType;

  // Declare the type of the size
  typedef itk::Size<VDimension>          mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<VDimension>        myRegionType;

  typedef typename TImage::SpacingType mySpacingType;

  // Declare the pointers to images
  typedef typename TImage::Pointer        TImagePointer;
  typedef typename TMathFilter::Pointer   TMathFilterPointer;

  // Create two images
  TImagePointer inputImageA  = TImage::New();
  TImagePointer inputImageB  = TImage::New();

  // Define their size, and start index
  mySizeType size;
  size[0] = 4;
  size[1] = 4;
  size[2] = 4;

  myIndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  mySpacingType spacing;
  spacing[0] = 1.0;
  spacing[1] = 1.0;
  spacing[2] = 1.0;

  // Initialize Image A
  inputImageA->SetLargestPossibleRegion( region );
  inputImageA->SetBufferedRegion( region );
  inputImageA->SetRequestedRegion( region );
  inputImageA->Allocate();
  inputImageA->SetSpacing(spacing);

  // Initialize Image B, same space, different spacing.
  size[0] = size[0] / 2;
  size[1] = size[1] / 2;
  size[2] = size[2] / 2;

  spacing[0] *= 2.0;
  spacing[1] *= 2.0;
  spacing[2] *= 2.0;

  inputImageB->SetLargestPossibleRegion( region );
  inputImageB->SetBufferedRegion( region );
  inputImageB->SetRequestedRegion( region );
  inputImageB->Allocate();
  inputImageB->SetSpacing(spacing);

  // Declare Iterator types apropriated for each image
  typedef itk::ImageRegionIteratorWithIndex<TImage>  myIteratorType1;
  typedef itk::ImageRegionIteratorWithIndex<TImage>  myIteratorType2;
  typedef itk::ImageRegionIteratorWithIndex<TImage>  myIteratorType3;

  // Create one iterator for Image A (this is a light object)
  myIteratorType1 it1( inputImageA, inputImageA->GetBufferedRegion() );

  // Initialize the content of Image A
  while( !it1.IsAtEnd() )
  {
  typename TImage::PixelType p;
  SetPixelValue(p,valA);
    it1.Set( p );
    ++it1;
  }

  // Create one iterator for Image B (this is a light object)
  myIteratorType2 it2( inputImageB, inputImageB->GetBufferedRegion() );

  // Initialize the content of Image B
  while( !it2.IsAtEnd() )
  {
  typename TImage::PixelType p;
  SetPixelValue(p,valB);
  it2.Set( p );
    ++it2;
  }

  // Create an ADD Filter
  TMathFilterPointer filter = TMathFilter::New();
  filter->SetUsePhysicalSpace(true);

  typedef typename TMathFilter::FunctorType TernaryFunctorType;
  //
  // If it is a ternary filter, concoct a third image
  if(dynamic_cast<itk::TernaryFunctorImageFilter<TImage,TImage,
                                                 TImage,TImage,
                                                 TernaryFunctorType> *>(filter.GetPointer())
     != 0)
    {
    size[0] = (3 * size[0])/2;
    size[1] = (3 * size[1])/2;
    size[2] = (3 * size[2])/2;

    spacing[0] /= 1.5;
    spacing[1] /= 1.5;
    spacing[2] /= 1.5;

    TImagePointer inputImageC  = TImage::New();
    inputImageC->SetLargestPossibleRegion( region );
    inputImageC->SetBufferedRegion( region );
    inputImageC->SetRequestedRegion( region );
    inputImageC->Allocate();
    inputImageC->SetSpacing(spacing);
    // Create one iterator for Image B (this is a light object)
    myIteratorType2 it3( inputImageC, inputImageC->GetBufferedRegion() );

    // Initialize the content of Image B
    while( !it3.IsAtEnd() )
      {
      typename TImage::PixelType p;
      SetPixelValue(p,valC);
      it3.Set( p );
      ++it3;
      }
    // use Partial Template Specialization hack to only call
    // SetInput3 on the filter type that supports it.
    SetThirdFilterInput<TMathFilter,TImage>::Set( filter,inputImageC );
    }

  // Connect the input images
  filter->SetInput1( inputImageA );
  filter->SetInput2( inputImageB );

  filter->SetFunctor(filter->GetFunctor());

  // Get the Smart Pointer to the Filter Output
  TImagePointer outputImage = filter->GetOutput();

  // Execute the filter
  filter->Update();
  filter->SetFunctor(filter->GetFunctor());

  // Create an iterator for going through the image output
  myIteratorType3 it3(outputImage, outputImage->GetBufferedRegion());

  //  Print the content of the result image
  while( !it3.IsAtEnd() )
  {
  if(!IsSame(expectedResult,it3.Get()))
    {
    std::cout << "Result "
              << it3.Get()
              << " didn't match expected result "
              << expectedResult
              << std::endl;
    return EXIT_FAILURE;
    }
    ++it3;
  }

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}

int itkMathInWorldCoordinatesTest(int, char* [] )
{
  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<float, myDimension>                                              myImageType;
  typedef itk::AddImageFilter<myImageType,myImageType,myImageType>                    AddImageFilterType;
  typedef itk::SubtractImageFilter<myImageType,myImageType,myImageType>               SubtractImageFilterType;
  typedef itk::MultiplyImageFilter<myImageType,myImageType,myImageType>               MultiplyImageFilterType;
  typedef itk::DivideImageFilter<myImageType,myImageType,myImageType>                 DivideImageFilterType;
  typedef itk::TernaryAddImageFilter<myImageType,myImageType,myImageType,myImageType> TernaryAddImageFilterType;
  int err(EXIT_SUCCESS);

  if(itkMathOpTest<myImageType,AddImageFilterType,myDimension>(2.0,3.0,0.0,5.0) != EXIT_SUCCESS)
    {
    err = EXIT_FAILURE;
    }
  if(itkMathOpTest<myImageType,SubtractImageFilterType,myDimension>(2.0,3.0,0.0,-1.0) != EXIT_SUCCESS)
    {
    err = EXIT_FAILURE;
    }
  if(itkMathOpTest<myImageType,MultiplyImageFilterType,myDimension>(2.0,3.0,0.0,6.0) != EXIT_SUCCESS)
    {
    err = EXIT_FAILURE;
    }
  if(itkMathOpTest<myImageType,DivideImageFilterType,myDimension>(2.0,3.0,0.0,(2.0/3.0)) != EXIT_SUCCESS)
    {
    err = EXIT_FAILURE;
    }
  if(itkMathOpTest<myImageType,TernaryAddImageFilterType,myDimension>(2.0,3.0,4.0,9.0) != EXIT_SUCCESS)
    {
    err = EXIT_FAILURE;
    }

  typedef itk::Vector<float,3>                    VectorPixelType;
  typedef itk::Image<VectorPixelType,myDimension> myVectorImageType;
  typedef itk::AddImageFilter<myVectorImageType,myVectorImageType,myVectorImageType>
                                                  VectorAddImageFilterType;

  VectorPixelType expectedResult;
  for(unsigned i = 0; i < 3; i++)
    {
    expectedResult.SetNthComponent(i,5.0);
    }
  if(itkMathOpTest<myVectorImageType,VectorAddImageFilterType,myDimension>(2.0,3.0,0.0,expectedResult)
     != EXIT_SUCCESS)
    {
    err = EXIT_FAILURE;
    }
  return err;
}
