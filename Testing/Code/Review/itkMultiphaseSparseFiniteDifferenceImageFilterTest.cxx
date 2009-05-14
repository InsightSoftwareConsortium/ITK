/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiphaseSparseFiniteDifferenceImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkMultiphaseSparseFiniteDifferenceImageFilter.h"
#include "itkScalarChanAndVeseLevelSetFunction.h"
#include "itkScalarChanAndVeseLevelSetFunctionData.h"
#include "itkConstrainedRegionBasedLevelSetFunctionSharedData.h"
#include "itkImage.h"
#include "itkTestingMacros.h"

namespace itk
{

template < class TInputImage, class TOutputImage,
  class TFiniteDifferenceFunction = FiniteDifferenceFunction<TOutputImage>,
  typename TIdCell = unsigned int >
class MultiphaseSparseFiniteDifferenceImageFilterTestHelper
  : public MultiphaseSparseFiniteDifferenceImageFilter<
      TInputImage, TOutputImage, TFiniteDifferenceFunction >
{
public:
  /** Standard class typedefs. */
  typedef MultiphaseSparseFiniteDifferenceImageFilterTestHelper  Self;
  typedef MultiphaseSparseFiniteDifferenceImageFilter<
    TInputImage, TOutputImage,TFiniteDifferenceFunction >        Superclass;
  typedef SmartPointer<Self>                                     Pointer;
  typedef SmartPointer<const Self>                               ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro( MultiphaseSparseFiniteDifferenceImageFilterTestHelper, MultiphaseSparseFiniteDifferenceImageFilter );

  itkNewMacro( Self );

  virtual void AllocateUpdateBuffer() {}

  typedef typename Superclass::TimeStepType   TimeStepType;

  virtual void ApplyUpdate(TimeStepType itkNotUsed(dt) ) {}

  virtual TimeStepType CalculateChange()
    {
    return TimeStepType( 1.0 );
    }

  virtual void CopyInputToOutput() {}

};


}

int itkMultiphaseSparseFiniteDifferenceImageFilterTest( int, char* [] )
{
  const unsigned int Dimension = 3;

  typedef double                                  PixelType;
  typedef itk::Image< PixelType, Dimension >      ImageType;
  typedef itk::Image< float, Dimension >          FeatureImageType;

  typedef itk::ScalarChanAndVeseLevelSetFunctionData<
    ImageType, FeatureImageType >      DataHelperType;
  typedef itk::ConstrainedRegionBasedLevelSetFunctionSharedData<
    ImageType, FeatureImageType, DataHelperType >      SharedDataHelperType;


  typedef itk::ScalarChanAndVeseLevelSetFunction<
    ImageType, FeatureImageType, SharedDataHelperType >      RegionBasedLevelSetFunctionType;

  RegionBasedLevelSetFunctionType::Pointer function = RegionBasedLevelSetFunctionType::New();

  typedef itk::MultiphaseSparseFiniteDifferenceImageFilterTestHelper<
    ImageType, ImageType, RegionBasedLevelSetFunctionType >  FilterType;

  FilterType::Pointer filter = FilterType::New();

  std::cout << "GetNameOfClass() = " << filter->GetNameOfClass() << std::endl;
  filter->Print( std::cout );


  return EXIT_SUCCESS;
}
