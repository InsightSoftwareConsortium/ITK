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

#include "itkScalarRegionBasedLevelSetFunction.h"

namespace itk
{

template < typename TInput, // LevelSetImageType
  typename TFeature, // FeatureImageType
  typename TSharedData >
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

  ScalarValueType ComputeInternalTerm(const FeaturePixelType &,
    const FeatureIndexType & ) override
    {
    return ScalarValueType( 0 );
    }

  ScalarValueType ComputeExternalTerm(const FeaturePixelType &,
    const FeatureIndexType & ) override
    {
    return ScalarValueType( 0 );
    }

  ScalarValueType ComputeOverlapParameters( const FeatureIndexType& ,
    ScalarValueType& ) override
    {
    return ScalarValueType( 0 );
    }

  void ComputeParameters() override {}

  void UpdateSharedDataParameters() override {}

  void UpdateSharedDataInsideParameters( const unsigned int& ,
    const FeaturePixelType&, const ScalarValueType& ) override {}

  void UpdateSharedDataOutsideParameters( const unsigned int& ,
    const FeaturePixelType&, const ScalarValueType& ) override {}

protected:
  ScalarRegionBasedLevelSetFunctionTestHelper() {}
  ~ScalarRegionBasedLevelSetFunctionTestHelper() override {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScalarRegionBasedLevelSetFunctionTestHelper);
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
  if( function.IsNull() )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
