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

#include "itkRegionBasedLevelSetFunction.h"

namespace itk
{

template < typename TInput, // LevelSetImageType
  typename TFeature, // FeatureImageType
  typename TSharedData >
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
    const FeatureIndexType& ) ITK_OVERRIDE
    {
    return ScalarValueType( 0 );
    }

  virtual ScalarValueType ComputeExternalTerm(const FeaturePixelType& ,
    const FeatureIndexType & ) ITK_OVERRIDE
    {
    return ScalarValueType( 0 );
    }

  virtual ScalarValueType ComputeOverlapParameters( const FeatureIndexType&,
    ScalarValueType & ) ITK_OVERRIDE
    {
    return ScalarValueType( 0 );
    }

  virtual void ComputeParameters() ITK_OVERRIDE {}

  virtual void UpdateSharedDataParameters() ITK_OVERRIDE {}

protected:
  RegionBasedLevelSetFunctionTestHelper() {}
  ~RegionBasedLevelSetFunctionTestHelper() {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegionBasedLevelSetFunctionTestHelper);
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
  if( function.IsNull() )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
