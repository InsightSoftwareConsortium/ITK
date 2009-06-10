/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarRegionBasedLevelSetFunctionTest.cxx
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

#include "itkScalarRegionBasedLevelSetFunction.h"
#include "itkVector.h"
#include "itkImage.h"
#include "itkTestingMacros.h"

namespace itk
{

template < class TInput, // LevelSetImageType
  class TFeature, // FeatureImageType
  class TSharedData >
class ScalarRegionBasedLevelSetFunctionTestHelper :
 public ScalarRegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
{
public:
  /** Standard class typedefs. */
  typedef ScalarRegionBasedLevelSetFunctionTestHelper                       Self;
  typedef ScalarRegionBasedLevelSetFunction<TInput,TFeature,TSharedData>    Superclass;
  typedef SmartPointer<Self>                                                Pointer;
  typedef SmartPointer<const Self>                                          ConstPointer;

  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( ScalarRegionBasedLevelSetFunctionTestHelper, ScalarRegionBasedLevelSetFunction );

  typedef typename Superclass::ScalarValueType     ScalarValueType;
  typedef typename Superclass::FeaturePixelType    FeaturePixelType;
  typedef typename Superclass::FeatureIndexType    FeatureIndexType;


  virtual ScalarValueType ComputeInternalTerm(const FeaturePixelType &,
    const FeatureIndexType & )
    {
    return ScalarValueType( 0 );
    }

  virtual ScalarValueType ComputeExternalTerm(const FeaturePixelType &,
    const FeatureIndexType & )
    {
    return ScalarValueType( 0 );
    }

  virtual ScalarValueType ComputeOverlapParameters( const FeatureIndexType& ,
    ScalarValueType& )
    {
    return ScalarValueType( 0 );
    }

  virtual void ComputeParameters() {}

  virtual void UpdateSharedDataParameters() {}

  virtual void UpdateSharedDataInsideParameters( const unsigned int& ,
    const FeaturePixelType&, const ScalarValueType& ) {}

  virtual void UpdateSharedDataOutsideParameters( const unsigned int& ,
    const FeaturePixelType&, const ScalarValueType& ) {}


protected:
  ScalarRegionBasedLevelSetFunctionTestHelper() {}
  ~ScalarRegionBasedLevelSetFunctionTestHelper() {}

private:
  ScalarRegionBasedLevelSetFunctionTestHelper(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

template <unsigned int NDimension>
class ScalarRegionBasedLevelSetFunctionSharedDataHelper : public DataObject
{
public:
  /** Standard class typedefs. */
  typedef ScalarRegionBasedLevelSetFunctionSharedDataHelper   Self;
  typedef DataObject                                          Superclass;
  typedef SmartPointer<Self>                                  Pointer;
  typedef SmartPointer<const Self>                            ConstPointer;

  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( ScalarRegionBasedLevelSetFunctionSharedDataHelper, DataObject );

  unsigned long       m_FunctionCount;

  typedef Index< NDimension >                 IndexType;
  typedef std::list< unsigned int >           ListPixelType;

  typedef Image< ListPixelType, NDimension > ImageType;
  typename ImageType::Pointer   m_NearestNeighborListImage;

  typedef double                              PixelType;
  typedef Image< PixelType, NDimension >      InputImageType;

  struct SingleData
    {
    typename InputImageType::Pointer m_HeavisideFunctionOfLevelSetImage;
    int m_WeightedNumberOfPixelsInsideLevelSet;
    int m_WeightedSumOfPixelValuesInsideLevelSet;
    int m_ForegroundConstantValues;

    int m_WeightedNumberOfPixelsOutsideLevelSet;
    int m_WeightedSumOfPixelValuesOutsideLevelSet;
    int m_BackgroundConstantValues;

    IndexType GetFeatureIndex( const IndexType & indx )
      {
      return indx;
      }

    IndexType GetIndex( const IndexType & globalIndex )
      {
      return globalIndex;
      }
    };

  SingleData* m_LevelSetDataPointerVector[19];

};

}

int itkScalarRegionBasedLevelSetFunctionTest( int, char* [] )
{
  const unsigned int Dimension = 3;

  typedef double                                  PixelType;
  typedef itk::Image< PixelType, Dimension >      ImageType;
  typedef itk::Image< float, Dimension >          FeatureImageType;

  typedef itk::ScalarRegionBasedLevelSetFunctionSharedDataHelper<Dimension>      DataHelperType;


  typedef itk::ScalarRegionBasedLevelSetFunctionTestHelper<
    ImageType, FeatureImageType, DataHelperType >      RegionBasedLevelSetFunctionType;

  RegionBasedLevelSetFunctionType::Pointer function = RegionBasedLevelSetFunctionType::New();

  return EXIT_SUCCESS;
}
