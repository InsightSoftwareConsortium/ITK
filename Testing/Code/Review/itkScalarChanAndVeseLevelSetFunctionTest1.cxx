/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarChanAndVeseLevelSetFunctionTest1.cxx
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
#include "itkVector.h"
#include "itkImage.h"
#include "itkTestingMacros.h"

namespace itk
{

template < class TInput, // LevelSetImageType
  class TFeature, // FeatureImageType
  class TSharedData >
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

  virtual void ComputeParameters() {}


protected:
  ScalarChanAndVeseLevelSetFunctionTestHelper() {}
  ~ScalarChanAndVeseLevelSetFunctionTestHelper() {}

private:
  ScalarChanAndVeseLevelSetFunctionTestHelper(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
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

  IndexType GetFeatureIndex( unsigned int itkNotUsed(functionId), const IndexType & indx )
    {
    return indx;
    }

  typedef std::list< unsigned int > ListPixelType;

  typedef Image< ListPixelType, NDimension > ImageType;

  typename ImageType::Pointer   m_NearestNeighborListImage;

  IndexType GetIndex( unsigned int itkNotUsed(functionId), const IndexType & globalIndex )
    {
    return globalIndex;
    }

  typedef double                              PixelType;
  typedef Image< PixelType, NDimension >      InputImageType;

  typename InputImageType::Pointer m_HeavisideFunctionOfLevelSetImage[19];

  int m_NumberOfPixelsInsideLevelSet[19];
  int m_SumOfPixelValuesInsideLevelSet[19];
  int m_ForegroundConstantValues[19];

  int m_NumberOfPixelsOutsideLevelSet[19];
  int m_SumOfPixelValuesOutsideLevelSet[19];
  int m_BackgroundConstantValues[19];

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
 
  return EXIT_SUCCESS;
}
