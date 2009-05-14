/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarChanAndVeseLevelSetFunctionTest2.cxx
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

#include "itkScalarChanAndVeseLevelSetFunction.h"
#include "itkScalarChanAndVeseLevelSetFunctionData.h"
#include "itkConstrainedRegionBasedLevelSetFunctionSharedData.h"
#include "itkVector.h"
#include "itkImage.h"
#include "itkTestingMacros.h"

namespace itk
{

template < class TInput, // LevelSetImageType
  class TFeature, // FeatureImageType
  class TSharedData >
class ScalarChanAndVeseLevelSetFunctionTest2Helper :
 public ScalarChanAndVeseLevelSetFunction< TInput, TFeature, TSharedData >
{
public:
  /** Standard class typedefs. */
  typedef ScalarChanAndVeseLevelSetFunctionTest2Helper                       Self;
  typedef ScalarChanAndVeseLevelSetFunction<TInput,TFeature,TSharedData>    Superclass;
  typedef SmartPointer<Self>                                          Pointer;
  typedef SmartPointer<const Self>                                    ConstPointer;

  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( ScalarChanAndVeseLevelSetFunctionTest2Helper, ScalarChanAndVeseLevelSetFunction );

  typedef typename Superclass::ScalarValueType     ScalarValueType;
  typedef typename Superclass::FeaturePixelType    FeaturePixelType;
  typedef typename Superclass::FeatureIndexType    FeatureIndexType;


  virtual ScalarValueType computeInternalTerm(const FeaturePixelType &,
    const FeatureIndexType &, const unsigned int & )
    {
    return ScalarValueType( 0 );
    }

  virtual ScalarValueType computeExternalTerm(const FeaturePixelType &,
    const FeatureIndexType &, const unsigned int & )
    {
    return ScalarValueType( 0 );
    }

  virtual void computeOverlapParameters( const FeatureIndexType,
    unsigned int &, unsigned int & ) {}

  virtual void ComputeParameters() {}


protected:
  ScalarChanAndVeseLevelSetFunctionTest2Helper() {}
  ~ScalarChanAndVeseLevelSetFunctionTest2Helper() {}

private:
  ScalarChanAndVeseLevelSetFunctionTest2Helper(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

}

int itkScalarChanAndVeseLevelSetFunctionTest2( int, char* [] )
{
  const unsigned int Dimension = 3;

  typedef double                                  PixelType;
  typedef itk::Image< PixelType, Dimension >      ImageType;
  typedef itk::Image< float, Dimension >          FeatureImageType;

  typedef itk::ScalarChanAndVeseLevelSetFunctionData< ImageType, FeatureImageType >  DataHelperType;

  typedef itk::ConstrainedRegionBasedLevelSetFunctionSharedData< ImageType, FeatureImageType, DataHelperType >
    SharedDataHelperType;


  typedef itk::ScalarChanAndVeseLevelSetFunctionTest2Helper<
    ImageType, FeatureImageType, SharedDataHelperType >      ChanAndVeseLevelSetFunctionType;

  ChanAndVeseLevelSetFunctionType::Pointer function = ChanAndVeseLevelSetFunctionType::New();

  std::cout << "GetNameOfClass() = " << function->GetNameOfClass() << std::endl;
  function->Print( std::cout );

  return EXIT_SUCCESS;
}
