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
#ifndef itkMixtureModelComponentBase_h
#define itkMixtureModelComponentBase_h

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"

#include "itkArray.h"
#include "itkObject.h"
#include "itkMembershipFunctionBase.h"

namespace itk
{
namespace Statistics
{
/** \class MixtureModelComponentBase
 * \brief base class for distribution modules that supports analytical way
 * to update the distribution parameters
 *
 * This class expects that its subclasses (distribution components) should
 * have analytical expressions for updating its paraters using only
 * the measurement vectors and their associated weights.
 *
 * This class can be considered as a macro class that encapsulates the
 * storage for the weights, model (subclasses of MembershipFunctionBase),
 * and model parameter estimators (implementation of analytical expressions).
 *
 * Subclasses of this class should define their own distribution specific
 * membership function. For example, GaussianMixtureModelComponent class
 * defines and creates a GaussianDensityFunction object for that matter.
 * Subclasses should also cast such membership function object to
 * MembershipFunctionBase object. By doing that, users can get pointers
 * to membership functions from different distributional model
 *
 * \sa ExpectationMaximizationMixtureModelEstimator
 * \ingroup ITKStatistics
 */

template< typename TSample >
class ITK_TEMPLATE_EXPORT MixtureModelComponentBase:
  public Object
{
public:
  /**Standard class typedefs. */
  typedef MixtureModelComponentBase  Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /**Standard Macros */
  itkTypeMacro(MixtureModelComponentBase, Object);

  typedef typename TSample::MeasurementVectorType     MeasurementVectorType;
  typedef typename TSample::MeasurementVectorSizeType MeasurementVectorSizeType;

  /** typedef for the MembershipFunctionBase */
  typedef MembershipFunctionBase< MeasurementVectorType >
  MembershipFunctionType;

  /** typedef of strorage for the weights */
  typedef Array< double > WeightArrayType;

  typedef Array< double > ParametersType;

  /** stores the sample pointer */
  virtual void SetSample(const TSample *sample);

  /** returns the sample pointer */
  const TSample * GetSample() const;

  /** returns the pointer to the membership function object.
   * Subclasses of this class are responsible for creating the
   * actual membership function objects and cast them to
   * MembershipFunctionBase objects */
  MembershipFunctionType * GetMembershipFunction();

  void SetMinimalParametersChange(double change)
  {
    m_MinimalParametersChange = change;
  }

  double GetMinimalParametersChange()
  {
    return m_MinimalParametersChange;
  }

  virtual void SetParameters(const ParametersType & parameters);

  virtual ParametersType GetFullParameters()
  {
    return m_Parameters;
  }

  /** sets the parameters modified tag. if one or more of the membership
   * funtion's parameters are changed, then flag should be true */
  void AreParametersModified(bool flag);

  /** returns the value of parameter modified tag */
  bool AreParametersModified();

  /** sets the index-th weight with the "value" */
  void SetWeight(unsigned int index, double value);

  /** returns the index-th weight */
  double GetWeight(unsigned int index) const;

  /** returns the membership score of the "measurements" vector */
  double Evaluate(MeasurementVectorType & measurements);

  /** returns the pointer to the weights array */
  itkGetConstReferenceMacro(Weights, WeightArrayType);

  virtual void Update();

protected:
  MixtureModelComponentBase();
  virtual ~MixtureModelComponentBase() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** stores the pointer to the membership function.
   * subclasses use this function to store their membership function
   * object after dynamic creation */
  void SetMembershipFunction(MembershipFunctionType *function);

  virtual void GenerateData() = 0;

private:
  /** target sample data pointer */
  const TSample *m_Sample;

  double m_MinimalParametersChange;

  ParametersType m_Parameters;

  /** SmartPointer to the memberhip function - usually density function */
  MembershipFunctionType *m_MembershipFunction;

  /** weights array */
  WeightArrayType m_Weights;

  /** indicative flag of membership function's parameter changes */
  bool m_ParametersModified;
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMixtureModelComponentBase.hxx"
#endif

#endif
