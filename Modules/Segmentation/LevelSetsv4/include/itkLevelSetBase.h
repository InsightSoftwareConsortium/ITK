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

#ifndef itkLevelSetBase_h
#define itkLevelSetBase_h

#include "itkIntTypes.h"
#include "itkCovariantVector.h"
#include "itkMatrix.h"
#include "itkNumericTraits.h"
#include "itkDataObject.h"

namespace itk
{
/**
 *  \class LevelSetBase
 *  \brief Abstract base class for the representation of a level-set function
 *
 *  \tparam TInput Input type where the level set function will be evaluated
 *  \tparam VDimension Dimension of the input space
 *  \tparam TOutput Returned type when evaluating the level set function
 *  \tparam TDomain Support of the level-set function (e.g. Image or QuadEdgeMesh)
 *  \ingroup ITKLevelSetsv4
 */
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
class ITK_TEMPLATE_EXPORT LevelSetBase : public DataObject
{
public:
  typedef LevelSetBase               Self;
  typedef DataObject                 Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information */
  itkTypeMacro ( LevelSetBase, DataObject );

  itkStaticConstMacro ( Dimension, unsigned int, VDimension );

  typedef TInput                                           InputType;
  typedef TOutput                                          OutputType;
  typedef TDomain                                          DomainType;
  typedef typename NumericTraits< OutputType >::RealType   OutputRealType;
  typedef CovariantVector< OutputRealType, VDimension >    GradientType;
  typedef Matrix< OutputRealType, VDimension, VDimension > HessianType;

  /** Type used to define Regions */
  typedef IdentifierType RegionType;

  /** Returns the value of the level set function at a given location iP */
  virtual OutputType    Evaluate( const InputType& iP ) const = 0;

  /** Returns the gradient of the level set function at a given location iP */
  virtual GradientType  EvaluateGradient( const InputType& iP ) const = 0;

  /** Returns the hessian of the level set function at a given location iP */
  virtual HessianType   EvaluateHessian( const InputType& iP ) const = 0;

  virtual OutputRealType EvaluateLaplacian( const InputType& iP ) const = 0;
  virtual OutputRealType EvaluateGradientNorm( const InputType& iP ) const;
  virtual OutputRealType EvaluateMeanCurvature( const InputType& iP ) const = 0;

  /** \class DataType
   *  \brief Internal class used for one computed characteritic
   *
   *  It holds the name of the characteristics, its value, and a boolean
   *  to keep track if it has already been computed or not.
   *  \ingroup ITKLevelSetsv4
   */
  template< typename T >
  class ITK_TEMPLATE_EXPORT DataType
    {
    public:
      DataType( const std::string& iName ) :
        m_Name( iName ), m_Computed( false )
        {}
      DataType( const DataType& iData ) : m_Name( iData.m_Name ),
        m_Value( iData.m_Value ), m_Computed( iData.m_Computed )
      {}

      ~DataType() {}

      std::string m_Name;
      T           m_Value;
      bool        m_Computed;

      void operator =( const DataType& iData )
        {
        this->m_Name = iData.m_Name;
        this->m_Value = iData.m_Value;
        this->m_Computed = iData.m_Computed;
        }

    private:
      DataType();
    };

  /** \struct LevelSetDataType
   *  \brief Convenient data structure to cache computed characteristics
   *  \ingroup ITKLevelSetsv4
   */
  struct LevelSetDataType
    {
    LevelSetDataType() : Value( "Value" ), Gradient( "Gradient" ),
      Hessian( "Hessian" ), Laplacian( "Laplacian" ),
      GradientNorm( "GradientNorm" ), MeanCurvature( "MeanCurvature" ),
      ForwardGradient( "ForwardGradient" ), BackwardGradient( "BackwardGradient" )
      {
      Value.m_Value = NumericTraits< OutputType >::ZeroValue();
      Gradient.m_Value.Fill( NumericTraits< OutputRealType >::ZeroValue() );
      Hessian.m_Value.Fill( NumericTraits< OutputRealType >::ZeroValue() );
      Laplacian.m_Value = NumericTraits< OutputRealType >::ZeroValue();
      GradientNorm.m_Value = NumericTraits< OutputRealType >::ZeroValue();
      MeanCurvature.m_Value = NumericTraits< OutputRealType >::ZeroValue();
      ForwardGradient.m_Value.Fill( NumericTraits< OutputRealType >::ZeroValue() );
      BackwardGradient.m_Value.Fill( NumericTraits< OutputRealType >::ZeroValue() );
      }

    LevelSetDataType( const LevelSetDataType& iData ) : Value( iData.Value ),
      Gradient( iData.Gradient ), Hessian( iData.Hessian ),
      Laplacian( iData.Laplacian ), GradientNorm( iData.GradientNorm ),
      MeanCurvature( iData.MeanCurvature ), ForwardGradient( iData.ForwardGradient ),
      BackwardGradient( iData.BackwardGradient ) {}

    ~LevelSetDataType() {}

    void operator = ( const LevelSetDataType& iData )
      {
      Value = iData.Value;
      Gradient = iData.Gradient;
      Hessian = iData.Hessian;
      Laplacian = iData.Laplacian;
      GradientNorm = iData.GradientNorm;
      MeanCurvature = iData.MeanCurvature;
      ForwardGradient = iData.ForwardGradient;
      BackwardGradient = iData.BackwardGradient;
      }

    /** the boolean value stores if it has already been computed */
    DataType< OutputType >      Value;
    DataType< GradientType >    Gradient;
    DataType< HessianType >     Hessian;
    DataType< OutputRealType >  Laplacian;
    DataType< OutputRealType >  GradientNorm;
    DataType< OutputRealType >  MeanCurvature;
    DataType< GradientType >    ForwardGradient;
    DataType< GradientType >    BackwardGradient;
    };

  virtual void Evaluate( const InputType& iP, LevelSetDataType& ioData ) const = 0;
  virtual void EvaluateGradient( const InputType& iP, LevelSetDataType& ioData ) const = 0;
  virtual void EvaluateHessian( const InputType& iP, LevelSetDataType& ioData ) const = 0;
  virtual void EvaluateLaplacian( const InputType& iP, LevelSetDataType& ioData ) const = 0;
  virtual void EvaluateGradientNorm( const InputType& iP, LevelSetDataType& ioData ) const;
  virtual void EvaluateMeanCurvature( const InputType& iP, LevelSetDataType& ioData ) const;
  virtual void EvaluateForwardGradient( const InputType& iP, LevelSetDataType& ioData ) const = 0;
  virtual void EvaluateBackwardGradient( const InputType& iP, LevelSetDataType& ioData ) const = 0;

  /** Returns true if iP is inside the level set, i.e. \f$\phi(p) \le 0 \f$ */
  virtual bool IsInside( const InputType& iP ) const;

  /** Get the maximum number of regions that this data can be
   * separated into. */
  itkGetConstMacro(MaximumNumberOfRegions, RegionType);

  /** Initialize the level set function */
  virtual void Initialize() ITK_OVERRIDE;

  /** Methods to manage streaming. */
  virtual void UpdateOutputInformation() ITK_OVERRIDE;

  virtual void SetRequestedRegionToLargestPossibleRegion() ITK_OVERRIDE;

  virtual void CopyInformation(const DataObject *data) ITK_OVERRIDE;

  virtual void Graft(const DataObject *data) ITK_OVERRIDE;

  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() ITK_OVERRIDE;

  virtual bool VerifyRequestedRegion() ITK_OVERRIDE;

  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method
   * implements the API from DataObject. The data object parameter must be
   * castable to a PointSet. */
  virtual void SetRequestedRegion( const DataObject *data) ITK_OVERRIDE;

  /** Set/Get the Requested region */
  virtual void SetRequestedRegion(const RegionType & region);

  itkGetConstMacro(RequestedRegion, RegionType);

  /** Set/Get the Buffered region */
  virtual void SetBufferedRegion(const RegionType & region);

  itkGetConstMacro(BufferedRegion, RegionType);

protected:
  LevelSetBase();
  virtual ~LevelSetBase() ITK_OVERRIDE {}

  // If the RegionType is ITK_UNSTRUCTURED_REGION, then the following
  // variables represent the maximum number of region that the data
  // object can be broken into, which region out of how many is
  // currently in the buffered region, and the number of regions and
  // the specific region requested for the update. Data objects that
  // do not support any division of the data can simply leave the
  // MaximumNumberOfRegions as 1. The RequestedNumberOfRegions and
  // RequestedRegion are used to define the currently requested
  // region. The LargestPossibleRegion is always requested region = 0
  // and number of regions = 1;
  RegionType m_MaximumNumberOfRegions;
  RegionType m_NumberOfRegions;
  RegionType m_RequestedNumberOfRegions;
  RegionType m_BufferedRegion;
  RegionType m_RequestedRegion;

private:
  LevelSetBase( const Self& ); // purposely left unimplemented
  void operator = ( const Self& ); // purposely left unimplemented

};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetBase.hxx"
#endif

#endif // itkLevelSetBase_h
