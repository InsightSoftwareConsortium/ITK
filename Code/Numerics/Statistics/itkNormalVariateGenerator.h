/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalVariateGenerator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNormalVariateGenerator_h
#define __itkNormalVariateGenerator_h

#include "itkObjectFactory.h"
#include "itkRandomVariateGeneratorBase.h"

namespace itk {
namespace Statistics {

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
 *      This is a revised version of the algorithm decribed in
 *
 *      ACM Transactions on Mathematical Software, Vol 22, No 1
 *      March 1996, pp 119-127.
 * 
 * It is somewhat faster, and uses less memory as the vector of variates is
 * updated in-situ. It has passed all the same statistical tests as decribed
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
 * To use this code, files needing to call the generator should #include the
 * file "FastNorm.h" and be linked with the maths library (-lm)
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
 */
class ITK_EXPORT NormalVariateGenerator : 
    public RandomVariateGeneratorBase
{
public:
  /** Standard class typedefs. */
  typedef NormalVariateGenerator Self ;
  typedef RandomVariateGeneratorBase Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(NormalVariateGenerator, 
               RandomVariateGeneratorBase );
 
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
    
  /** generate random number table */
  void Initialize(int randomSeed) ;

  /** get a variate using FastNorm function */
  double GetVariate() ;
    
protected:
  NormalVariateGenerator() ;
  virtual ~NormalVariateGenerator() ; 
  virtual void PrintSelf(std::ostream& os, Indent indent) const ;
    
  /** get a variate */
  double FastNorm (void) ;
    
private:
  double Scale ;
  double Rscale ;
  double Rcons ;
  int ELEN ;
  int LEN ;
  int LMASK ;
  int TLEN ;
    
  int gaussfaze, *gausssave;
  double GScale;
    
  int* vec1 ;
  int nslew;
  int irs, lseed;
  double chic1, chic2, actualRSD;
} ;  // end of class
  
} // end of namespace Statistics
} // end of namespace itk
#endif
