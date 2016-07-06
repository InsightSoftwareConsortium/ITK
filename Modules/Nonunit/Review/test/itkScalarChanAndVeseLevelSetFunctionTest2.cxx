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

  virtual void ComputeParameters() ITK_OVERRIDE {}

protected:
  ScalarChanAndVeseLevelSetFunctionTest2Helper() {}
  ~ScalarChanAndVeseLevelSetFunctionTest2Helper() {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScalarChanAndVeseLevelSetFunctionTest2Helper);
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
