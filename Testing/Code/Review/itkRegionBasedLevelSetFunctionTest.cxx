/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionBasedLevelSetFunctionTest.cxx
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

#include "itkRegionBasedLevelSetFunction.h"
#include "itkVector.h"
#include "itkImage.h"
#include "itkTestingMacros.h"

namespace itk
{

template < class TInput, // LevelSetImageType
  class TFeature, // FeatureImageType
  class TSharedData >
class RegionBasedLevelSetFunctionTestHelper :
 public RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
{
public:
  /** Standard class typedefs. */
  typedef RegionBasedLevelSetFunctionTestHelper                       Self;
  typedef RegionBasedLevelSetFunction<TInput,TFeature,TSharedData>    Superclass;
  typedef SmartPointer<Self>                                          Pointer;
  typedef SmartPointer<const Self>                                    ConstPointer;

  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( RegionBasedLevelSetFunctionTestHelper, RegionBasedLevelSetFunction );

  typedef typename Superclass::ScalarValueType     ScalarValueType;
  typedef typename Superclass::FeaturePixelType    FeaturePixelType;
  typedef typename Superclass::FeatureIndexType    FeatureIndexType;


  virtual ScalarValueType ComputeInternalTerm(const FeaturePixelType& ,
    const FeatureIndexType& )
    {
    return ScalarValueType( 0 );
    }

  virtual ScalarValueType ComputeExternalTerm(const FeaturePixelType& ,
    const FeatureIndexType & )
    {
    return ScalarValueType( 0 );
    }

  virtual ScalarValueType ComputeOverlapParameters( const FeatureIndexType&,
    ScalarValueType & )
    {
    return ScalarValueType( 0 );
    }

  virtual void ComputeParameters() {}

  virtual void UpdateSharedDataParameters() {}


protected:
  RegionBasedLevelSetFunctionTestHelper() {}
  ~RegionBasedLevelSetFunctionTestHelper() {}

private:
  RegionBasedLevelSetFunctionTestHelper(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

template <unsigned int NDimension>
class RegionBasedLevelSetFunctionSharedDataHelper : public DataObject
{
public:
  /** Standard class typedefs. */
  typedef RegionBasedLevelSetFunctionSharedDataHelper  Self;
  typedef DataObject                                   Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;

  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( RegionBasedLevelSetFunctionSharedDataHelper, DataObject );

  unsigned long       m_FunctionCount;

  typedef Index< NDimension >                 IndexType;

  struct SingleData
    {
    unsigned int m_WeightedNumberOfPixelsInsideLevelSet;
    IndexType GetFeatureIndex( const IndexType & indx )
      {
      return indx;
      }
    };

  SingleData* m_LevelSetDataPointerVector[19];
};

}

int itkRegionBasedLevelSetFunctionTest( int, char* [] )
{
  const unsigned int Dimension = 3;

  typedef double                                  PixelType;
  typedef itk::Image< PixelType, Dimension >      ImageType;
  typedef itk::Image< float, Dimension >          FeatureImageType;

  typedef itk::RegionBasedLevelSetFunctionSharedDataHelper<Dimension>      DataHelperType;


  typedef itk::RegionBasedLevelSetFunctionTestHelper<
    ImageType, FeatureImageType, DataHelperType >      RegionBasedLevelSetFunctionType;

  RegionBasedLevelSetFunctionType::Pointer function = RegionBasedLevelSetFunctionType::New();

  return EXIT_SUCCESS;
}
