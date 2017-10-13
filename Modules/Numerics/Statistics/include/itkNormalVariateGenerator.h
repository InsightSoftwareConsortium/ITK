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
#ifndef itkNormalVariateGenerator_h
#define itkNormalVariateGenerator_h

#include "itkObjectFactory.h"
#include "itkRandomVariateGeneratorBase.h"
#include "ITKStatisticsExport.h"

namespace itk
{
namespace Statistics
{
/** \class NormalVariateGenerator
 * \brief Normal random variate generator
 *
 * This generation method was initially developed and implemented by
 * Martin Styner, University of North Carolina at Chapel Hill,
 * and his colleagues.
 *
 * You should run Initialize() function before call GetNormalVariate()
 * function.
 *
 * The followings are original comments.
 *
 *  Revision date 31 May 1996
 *      This is a revised version of the algorithm described in
 *
 *      ACM Transactions on Mathematical Software, Vol 22, No 1
 *      March 1996, pp 119-127.
 *
 * It is somewhat faster, and uses less memory as the vector of variates is
 * updated in-situ. It has passed all the same statistical tests as described
 * in the TOMS article, plus others. Seems OK so far.
 *
 *        Works well with total pool of 1024 variates, and does not need
 *        two vectors of this size, so does less damage to cache.
 *                Has been tested for frequency of tail values which
 *        should occur once in a million. OK. Other usual tests OK.
 *        About 13 % faster than TOMS version.
 *
 *        FAST GENERATOR OF PSEUDO-RANDOM UNIT NORMAL VARIATES
 *
 *                C.S.Wallace, Monash University, 1994
 *
 * To use this code, files needing to call the generator should include:
 * \code
 * #include "FastNorm.h"
 * \endcode
 * and be linked with the maths library (-lm)
 *        FastNorm.h contains declaration of the initialization routine
 * 'initnorm()', definition of a macro 'FastGauss' used to generate variates,
 * and three variables used in the macro.
 *        Read below for calling conventions.
 *
 * THIS CODE ASSUMES TWO'S-COMPLEMENT 32-BIT INTEGER ARITHMATIC.  IT ALSO
 * ASSUMES THE 'C' COMPILER COMPILES THE LEFT-SHIFT OPERATOR "<<" AS A LOGICAL
 * SHIFT, DISCARDING THE SIGN DIGIT AND SHIFTING IN ZEROS ON THE RIGHT, SO
 * " X << 1" IS EQUIVALENT TO " X+X ".   IT ALSO ASSUMES THE RIGHT-SHIFT
 * OPERATOR ">>" IS SIGN-PRESERVING, SO ( -2 >> 1) = -1,  ( -1>>1) = -1.
 *
 *
 *
 *         A fast generator of pseudo-random variates from the unit Normal
 * distribution. It keeps a pool of about 1000 variates, and generates new
 * ones by picking 4 from the pool, rotating the 4-vector with these as its
 * components, and replacing the old variates with the components of the
 * rotated vector.
 *
 *
 *         The program should initialize the generator by calling
 * initnorm(seed)
 * with seed a int integer seed value. Different seed values will give
 * different sequences of Normals.
 *         Then, wherever the program needs a new Normal variate, it should
 * use the macro FastGauss, e.g. in statements like:
 *         x = FastGauss;  (Sets x to a random Normal value)
 *
 *
 * \ingroup Statistics
 * \ingroup ITKStatistics
 */
class ITKStatistics_EXPORT NormalVariateGenerator:
  public RandomVariateGeneratorBase
{
public:
  /** Standard class typedefs. */
  typedef NormalVariateGenerator     Self;
  typedef RandomVariateGeneratorBase Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(NormalVariateGenerator,
               RandomVariateGeneratorBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** generate random number table */
  void Initialize(int randomSeed);

  /** get a variate using FastNorm function */
  virtual double GetVariate() ITK_OVERRIDE;

protected:
  NormalVariateGenerator();
  virtual ~NormalVariateGenerator() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** get a variate */
  double FastNorm();

private:

  static inline int SignedShiftXOR( int irs )
    {
      // shifting of signed integer gives undefined results, explicitly
      // cast to unsigned to get expected ( if two complement
      // representation ) results.
      unsigned int uirs = static_cast<unsigned int>(irs);
      return static_cast<int>(( irs <= 0 ) ? ( (  uirs << 1 ) ^ 333556017 ) : ( uirs << 1 ));
    }

  double m_Scale;
  double m_Rscale;
  double m_Rcons;
  int    m_ELEN;
  int    m_LEN;
  int    m_LMASK;
  int    m_TLEN;

  int  m_Gaussfaze;
  int *m_Gausssave;

  double m_GScale;

  int *  m_Vec1;
  int    m_Nslew;
  int    m_Irs;
  int    m_Lseed;
  double m_Chic1;
  double m_Chic2;
  double m_ActualRSD;
};  // end of class
} // end of namespace Statistics
} // end of namespace itk
#endif
