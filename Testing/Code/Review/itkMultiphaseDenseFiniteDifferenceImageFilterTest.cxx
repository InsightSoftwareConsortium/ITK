/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiphaseDenseFiniteDifferenceImageFilterTest.cxx
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

#include "itkMultiphaseDenseFiniteDifferenceImageFilter.h"
#include "itkScalarChanAndVeseLevelSetFunction.h"
#include "itkScalarChanAndVeseLevelSetFunctionData.h"
#include "itkConstrainedRegionBasedLevelSetFunctionSharedData.h"
#include "itkImage.h"
#include "itkTestingMacros.h"

namespace itk
{

template < class TInputImage, class TFeatureImage, class TOutputImage,
  class TFiniteDifferenceFunction, typename TIdCell >
class MultiphaseDenseFiniteDifferenceImageFilterTestHelper
  : public MultiphaseDenseFiniteDifferenceImageFilter<
      TInputImage, TFeatureImage, TOutputImage, 
      TFiniteDifferenceFunction, TIdCell >
{
public:
  /** Standard class typedefs. */
  typedef MultiphaseDenseFiniteDifferenceImageFilterTestHelper      Self;
  typedef MultiphaseDenseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
    TOutputImage,TFiniteDifferenceFunction >                        Superclass;
  typedef SmartPointer<Self>                                        Pointer;
  typedef SmartPointer<const Self>                                  ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro( MultiphaseDenseFiniteDifferenceImageFilterTestHelper, MultiphaseDenseFiniteDifferenceImageFilter );

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

int itkMultiphaseDenseFiniteDifferenceImageFilterTest( int, char* [] )
{
  const unsigned int Dimension = 3;

  typedef itk::Image< double, Dimension >         LevelSetImageType;
  typedef itk::Image< float, Dimension >          FeatureImageType;
  typedef itk::Image< unsigned char, Dimension >  OutputImageType;

  typedef itk::ScalarChanAndVeseLevelSetFunctionData< LevelSetImageType, FeatureImageType >
    DataHelperType;
  typedef itk::ConstrainedRegionBasedLevelSetFunctionSharedData< LevelSetImageType,
    FeatureImageType, DataHelperType >            SharedDataHelperType;

  typedef itk::ScalarChanAndVeseLevelSetFunction< LevelSetImageType, FeatureImageType,
    SharedDataHelperType >                        RegionBasedLevelSetFunctionType;

  RegionBasedLevelSetFunctionType::Pointer function = RegionBasedLevelSetFunctionType::New();

  typedef unsigned long IdCellType;

  typedef itk::MultiphaseDenseFiniteDifferenceImageFilterTestHelper< LevelSetImageType,
    FeatureImageType, OutputImageType, RegionBasedLevelSetFunctionType,
    IdCellType >  FilterType;

  FilterType::Pointer filter = FilterType::New();

  std::cout << "GetNameOfClass() = " << filter->GetNameOfClass() << std::endl;
  filter->Print( std::cout );


  return EXIT_SUCCESS;
}
