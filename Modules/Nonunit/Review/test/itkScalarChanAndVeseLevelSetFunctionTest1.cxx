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

#include "itkScalarChanAndVeseLevelSetFunction.h"

namespace itk
{

template < typename TInput, // LevelSetImageType
  typename TFeature, // FeatureImageType
  typename TSharedData >
class ScalarChanAndVeseLevelSetFunctionTestHelper :
 public ScalarChanAndVeseLevelSetFunction< TInput, TFeature, TSharedData >
{
public:
  /** Standard class typedefs. */
  typedef ScalarChanAndVeseLevelSetFunctionTestHelper                       Self;
  typedef ScalarChanAndVeseLevelSetFunction<TInput,TFeature,TSharedData>    Superclass;
  typedef SmartPointer<Self>                                          Pointer;
  typedef SmartPointer<const Self>                                    ConstPointer;

  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( ScalarChanAndVeseLevelSetFunctionTestHelper, ScalarChanAndVeseLevelSetFunction );

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

  virtual void ComputeParameters() ITK_OVERRIDE {}

protected:
  ScalarChanAndVeseLevelSetFunctionTestHelper() {}
  ~ScalarChanAndVeseLevelSetFunctionTestHelper() {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScalarChanAndVeseLevelSetFunctionTestHelper);
};

template <unsigned int NDimension>
class ScalarChanAndVeseLevelSetFunctionSharedDataHelper : public DataObject
{
public:
  /** Standard class typedefs. */
  typedef ScalarChanAndVeseLevelSetFunctionSharedDataHelper   Self;
  typedef DataObject                                          Superclass;
  typedef SmartPointer<Self>                                  Pointer;
  typedef SmartPointer<const Self>                            ConstPointer;

  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( ScalarChanAndVeseLevelSetFunctionSharedDataHelper, DataObject );

  unsigned long       m_FunctionCount;

  typedef Index< NDimension >                 IndexType;
  typedef std::list< unsigned int >           ListPixelType;
  typedef Image< ListPixelType, NDimension >  ImageType;

  typename ImageType::Pointer                 m_NearestNeighborListImage;

  typedef double                              PixelType;
  typedef Image< PixelType, NDimension >      InputImageType;

  struct SingleData
    {
    IndexType GetIndex( const IndexType & globalIndex )
      {
      return globalIndex;
      }

    IndexType GetFeatureIndex( const IndexType & indx )
      {
      return indx;
      }

    typename InputImageType::Pointer m_HeavisideFunctionOfLevelSetImage;

    double m_WeightedNumberOfPixelsInsideLevelSet;
    double m_WeightedSumOfPixelValuesInsideLevelSet;
    double m_ForegroundConstantValues;

    double m_WeightedNumberOfPixelsOutsideLevelSet;
    double m_WeightedSumOfPixelValuesOutsideLevelSet;
    double m_BackgroundConstantValues;
    };

  SingleData* m_LevelSetDataPointerVector[19];
};

}

int itkScalarChanAndVeseLevelSetFunctionTest1( int, char* [] )
{
  const unsigned int Dimension = 3;

  typedef double                                  PixelType;
  typedef itk::Image< PixelType, Dimension >      ImageType;
  typedef itk::Image< float, Dimension >          FeatureImageType;

  typedef itk::ScalarChanAndVeseLevelSetFunctionSharedDataHelper<Dimension>      DataHelperType;


  typedef itk::ScalarChanAndVeseLevelSetFunctionTestHelper<
    ImageType, FeatureImageType, DataHelperType >      ChanAndVeseLevelSetFunctionType;

  ChanAndVeseLevelSetFunctionType::Pointer function = ChanAndVeseLevelSetFunctionType::New();

  std::cout << "GetNameOfClass() = " << function->GetNameOfClass() << std::endl;
  function->Print( std::cout );

  return EXIT_SUCCESS;
}
