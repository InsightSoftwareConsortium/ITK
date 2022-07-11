/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMinimalStandardRandomVariateGenerator_h
#define itkMinimalStandardRandomVariateGenerator_h

#include "itkIntTypes.h"
#include "itkObjectFactory.h"
#include "itkRandomVariateGeneratorBase.h"
#include "FpfhExport.h"
#include "itkNormalVariateGenerator.h"

namespace itk
{
namespace Statistics
{
/** \class MinimalStandardRandomVariateGenerator
 * \brief Linear congruential random random variate generator.
 *
 * This is a pseudo-random number generator for unsigned integers following
 *
 * \f[
 *   X_{n+1} = (a X_n + c) \mod m
 * \f]
 *
 * where \f$a\f$ is the Multiplier \f$c\f$ is the Increment and \f$m\f$ is
 * the Modulus.
 *
 * https://en.wikipedia.com/wiki/Linear_congruential_generator
 *
 * The random numbers generated have a period \f$m\f$.
 *
 * This class uses \f$a = 48271\f$, \f$c = 0\f$, \f$m = 2^31 -1 =
 * 2147483647\f$, the Minimial Standard configuration recommended by Park,
 * Miller and Stockmeyer in 1993.
 *
 * \ingroup Fpfh
 */
class Fpfh_EXPORT MinimalStandardRandomVariateGenerator : public RandomVariateGeneratorBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MinimalStandardRandomVariateGenerator);

  /** Standard class aliases. */
  using Self = MinimalStandardRandomVariateGenerator;
  using Superclass = RandomVariateGeneratorBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using IntegerType = uint32_t;

  using NormalGeneratorType = itk::Statistics::NormalVariateGenerator;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MinimalStandardRandomVariateGenerator, RandomVariateGeneratorBase);

  /** Method for creation through the object factory.  */
  itkNewMacro(Self);

  /** initialize with a simple IntegerType */
  void
  Initialize(int randomSeed);

  /** Get a variate in the range [0, 1] */
  double
  GetVariate() override;

protected:
  MinimalStandardRandomVariateGenerator();
  ~MinimalStandardRandomVariateGenerator() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  NormalGeneratorType::Pointer m_NormalGenerator;
};

} // end of namespace Statistics
} // end of namespace itk

#endif // itkMinimalStandardRandomVariateGenerator_h
