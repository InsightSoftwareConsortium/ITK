/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalVariateGenerator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <vnl/vnl_math.h>

#include "itkNormalVariateGenerator.h"

namespace itk {
namespace Statistics {

NormalVariateGenerator::NormalVariateGenerator()
{
  Scale = ((double) 30000000.0) ;
  Rscale = ((double) (1.0 / Scale)) ;
  Rcons = ((double) (1.0 / (2.0 * 1024.0 * 1024.0 * 1024.0))) ;
  ELEN = 7 ;    /*  LEN must be 2 ** ELEN       */
  LEN = 128 ;
  LMASK = (4 * (LEN-1)) ;
  TLEN  = (8*LEN) ;
  vec1 = new int[TLEN] ;

  this->Initialize(0) ;
}

NormalVariateGenerator::~NormalVariateGenerator()
{
  delete[] vec1 ;
}

void NormalVariateGenerator::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent) ;

  os << indent << "Scale: " << Scale << std::endl ;
  os << indent << "Rscale: " << Rscale << std::endl ;
  os << indent << "Rcons: " << Rcons << std::endl ;
  os << indent << "ELEN: " << ELEN << std::endl ;
  os << indent << "LEN: " << LEN << std::endl ;
  os << indent << "LMASK: " << LMASK << std::endl ;
  os << indent << "TLEN: " << TLEN << std::endl ;
    
  os << indent << "gaussfaze: " << gaussfaze << std::endl ;
  os << indent << "gausssave: " << gausssave << std::endl ;
  os << indent << "GScale: " << GScale << std::endl ;
    
  os << indent << "vec1: " << vec1 << std::endl ;
  os << indent << "nslew: " << nslew << std::endl ;
  os << indent << "irs: " << irs << std::endl ;
  os << indent << "lseed: " << lseed << std::endl ;
  os << indent << "chic1: " << chic1 << std::endl ;
  os << indent << "chic2: " << chic2 << std::endl ;
  os << indent << "actualRSD: " << actualRSD << std::endl ;
}

void NormalVariateGenerator::Initialize(int randomSeed)
{
  // m_Random Seed was originally getpid()

  double fake ;
  lseed = randomSeed ;
  irs = randomSeed ;
  gaussfaze = 1 ;
  nslew = 0 ;
  GScale = Rscale ;
//      At one stage, we need to generate a random variable Z such that
//      (TLEN * Z*Z) has a Chi-squared-TLEN density. Now, a var with
//      an approximate Chi-sq-K distn can be got as
//          0.5 * (C + A*n)**2  where n has unit Normal distn,
//      A = (1 + 1 / (8K)),  C*C = 2K - A*A    (For large K)
//          So we form Z as (sqrt (1 / 2TLEN)) * (C + A*n)
//      or:
//          Z = (sqrt (1/2TLEN)) * A * (B + n)
//      where:
//          B = C / A.
//      We set chic1 = A * sqrt (0.5 / TLEN),  chic2 = B
  
  fake = 1.0 + 0.125 / TLEN;   // This is A 
  chic2 = sqrt (2.0 * TLEN  -  fake*fake) /  fake;
  chic1 = fake * sqrt (0.5 / TLEN);

  actualRSD = 0.0 ;
  return;
}


double NormalVariateGenerator::GetVariate()
{
  if (--gaussfaze)
    return GScale * gausssave[gaussfaze] ;
  else
    return FastNorm() ;
}

/*      -----------------------------------------------------   */


double NormalVariateGenerator::FastNorm(void)
{
  int i;
  int inc    = 0;
  int skew;
  int stride;
  int mask   = 0;
  int p;
  int q;
  int r;
  int s;
  int t;
  int *pa = 0;
  int *pb = 0;
  int *pc = 0;
  int *pd = 0;
  int *pe;
  int *p0 = 0;
  int mtype;
  int stype;
  double ts;
  double tr;
  double tx;
  double ty;
  double tz;

  /*    See if time to make a new set of 'original' deviates  */
  /*    or at least to correct for a drift in sum-of-squares    */
  if (! (nslew & 0xFF)) goto renormalize;

 startpass:
  /*    Count passes    */
  nslew ++;
  /*    Reset index into Saved values   */
  gaussfaze = TLEN - 1; /* We will steal the last one   */
  /*    Update pseudo-random and use to choose type of rotation  */
  lseed = 69069 * lseed + 33331;
  irs = (irs <= 0) ? ((irs << 1) ^ 333556017):(irs << 1);
  t = irs + lseed;
  if (t < 0) t = ~t;
  /*    This gives us 31 random bits in t       */
  /*    We need ELEN to fix initial index into LEN, ELEN-1 to fix an odd
        stride, 2 to fix matrix type and maybe 1 for scantype, making
        2*ELEN + 2 in all, and leaving 29 - 2*ELEN unused
  */
  t = t >> (29 - 2*ELEN);       /*  Discard unwanted digits  */
  skew = (LEN-1) & t;  t = t >> ELEN;
  skew = 4 * skew;      /*  To give a word index to group of 4 */
  stride = (LEN/2 -1 ) & t;     t = t >> (ELEN-1);
  stride = 8 * stride + 4;      /* To give an odd num of 4-groups */
  mtype = t & 3;
  /*    Leaves a bit for stype, but not currently used   */

  /*    Use last bits of nslew to determine scanning pattern   */
  stype = nslew & 3;
  switch (stype)        {
  case 0:               /*   From consecutive in top to scattered in bot  */
    inc = 1;
    mask = LMASK;
    pa = vec1;  pb = pa + LEN;  pc = pb + LEN;  pd = pc + LEN;
    p0 = vec1 + 4 * LEN;
    goto scanset;
  case 1:               /*   From consec in bot to scatt in top  */
    inc = 1;
    mask = LMASK;
    pa = vec1 + 4 * LEN;  pb = pa + LEN;  pc = pb + LEN;  pd = pc + LEN;
    p0 = vec1;
    goto scanset;
  case 2:               /*   From consec in even to scatt in odd  */
    inc = 2;
    mask = 2*LMASK;   skew *= 2;   stride *= 2;
    pa = vec1 + 1;  pb = pa + 2*LEN;  pc = pb + 2*LEN;  pd = pc + 2*LEN;
    p0 = vec1;
    goto scanset;
  case 3:               /*  From consec in odd to scatt in even  */
    inc = 2;
    mask = 2*LMASK;   skew *= 2;   stride *= 2;
    pa = vec1;  pb = pa + 2*LEN;  pc = pb + 2*LEN;  pd = pc + 2*LEN;
    p0 = vec1 + 1;
    goto scanset;
  }     /*   End of scan pattern cases */

 scanset:
  gausssave = vec1;
  /*    Set loop count  */
  i = LEN;

  /*    Use mtype to select matrix   */
  switch (mtype)        {
  case 0:               goto matrix0;
  case 1:               goto matrix1;
  case 2:               goto matrix2;
  case 3:               goto matrix3;
  }

 matrix0:
  pa += (inc * (LEN-1));
 mpass0:
  skew = (skew + stride) & mask;
  pe = p0 + skew;
  p = -*pa;  q = -*pb;  r =  *pc;  s =  *pd;
  t = (p + q + r + s) >> 1;
  p = t - p;  q = t - q;  r = t - r;  s = t - s;
  /*    Have new values in p,q,r,s.  Place and save replaced vals  */
  t = -*pe;  *pe = p;   pe += inc;
  p = *pe;  *pe = q;   pe += inc;
  q = -*pe;  *pe = r;   pe += inc;
  r = *pe;  *pe = s;
  /*    Have vals in p,q,r,t    */
  s = (p + q + r + t) >> 1;
  *pa = s - p;   pa -= inc;
  *pb = s - q;   pb += inc;
  *pc = s - r;   pc += inc;
  *pd = s - t;   pd += inc;
  if (--i) goto mpass0;
  goto endpass;

 matrix1:
  pb += (inc * (LEN-1));
 mpass1:
  skew = (skew + stride) & mask;
  pe = p0 + skew;
  p = -*pa;  q = *pb;  r = *pc;  s = -*pd;
  t = (p + q + r + s) >> 1;
  p = t - p;  q = t - q;  r = t - r;  s = t - s;
  /*    Have new values in p,q,r,s.  Place and save replaced vals  */
  t = *pe;  *pe = p;   pe += inc;
  p = -*pe;  *pe = q;   pe += inc;
  q = -*pe;  *pe = r;   pe += inc;
  r = *pe;  *pe = s;
  /*    Have vals in p,q,r,t    */
  s = (p + q + r + t) >> 1;
  *pa = s - p;   pa += inc;
  *pb = s - t;   pb -= inc;
  *pc = s - q;   pc += inc;
  *pd = s - r;   pd += inc;
  if (--i) goto mpass1;
  goto endpass;

 matrix2:
  pc += (inc * (LEN-1));
 mpass2:
  skew = (skew + stride) & mask;
  pe = p0 + skew;
  p = *pa;  q = -*pb;  r = *pc;  s = -*pd;
  t = (p + q + r + s) >> 1;
  p = t - p;  q = t - q;  r = t - r;  s = t - s;
  /*    Have new values in p,q,r,s.  Place and save replaced vals  */
  t = *pe;  *pe = p;   pe += inc;
  p = *pe;  *pe = q;   pe += inc;
  q = -*pe;  *pe = r;   pe += inc;
  r = -*pe;  *pe = s;
  /*    Have vals in p,q,r,t    */
  s = (p + q + r + t) >> 1;
  *pa = s - r;   pa += inc;
  *pb = s - p;   pb += inc;
  *pc = s - q;   pc -= inc;
  *pd = s - t;   pd += inc;
  if (--i) goto mpass2;
  goto endpass;

 matrix3:
  pd += (inc * (LEN-1));
 mpass3:
  skew = (skew + stride) & mask;
  pe = p0 + skew;
  p = *pa;  q = *pb;  r = -*pc;  s = -*pd;
  t = (p + q + r + s) >> 1;
  p = t - p;  q = t - q;  r = t - r;  s = t - s;
  /*    Have new values in p,q,r,s.  Place and save replaced vals  */
  t = -*pe;  *pe = p;   pe += inc;
  p =  *pe;  *pe = q;   pe += inc;
  q =  *pe;  *pe = r;   pe += inc;
  r = -*pe;  *pe = s;
  /*    Have vals in p,q,r,t    */
  s = (p + q + r + t) >> 1;
  *pa = s - q;   pa += inc;
  *pb = s - r;   pb += inc;
  *pc = s - t;   pc += inc;
  *pd = s - p;   pd -= inc;
  if (--i) goto mpass3;
  goto endpass;

 endpass:
  /*    Choose a value for GScale which will make the sum-of-squares have
        the variance of Chi-Sq (TLEN), i.e., 2*TLEN.  Choose a value from
        Chi-Sq (TLEN) using the method descibed in initnorm.
        The Normal variate is obtained from gausssave[TLEN-1], which is
        not used by the caller.
  */
  ts = chic1 * (chic2 + GScale * vec1 [TLEN-1]);
  /*    TLEN * ts * ts  has ChiSq (TLEN) distribution   */
  GScale = Rscale * ts * actualRSD;
  return (GScale * vec1[0]);

 renormalize:
  if (nslew & 0xFFFF) goto recalcsumsq;
  /*    Here, replace the whole pool with conventional Normal variates  */
  ts = 0.0;
  p = 0;
 nextpair:
  lseed = 69069 * lseed + 33331;
  irs = (irs <= 0) ? ((irs << 1) ^ 333556017):(irs << 1);
  r = irs + lseed;
  tx = Rcons * r;
  lseed = 69069 * lseed + 33331;
  irs = (irs <= 0) ? ((irs << 1) ^ 333556017):(irs << 1);
  r = irs + lseed;
  ty = Rcons * r;
  tr = tx * tx + ty * ty;
  if ((tr > 1.0) || (tr < 0.1)) goto nextpair;
  lseed = 69069 * lseed + 33331;
  irs = (irs <= 0) ? ((irs << 1) ^ 333556017):(irs << 1);
  r = irs + lseed;
  if (r < 0) r = ~r;
  tz = -2.0 * log ((r + 0.5) * Rcons);   /* Sum of squares */
  ts += tz;
  tz = sqrt ( tz / tr );
  vec1 [p++] = (int) (Scale *  tx * tz) ;
  vec1 [p++] = (int) (Scale *  ty * tz) ;
  if (p < TLEN) goto nextpair;
  /*    Horrid, but good enough */
  /*    Calc correction factor to make sum of squares = TLEN    */
  ts = TLEN / ts;  /* Should be close to 1.0  */
  tr = sqrt (ts);
  for (p = 0; p < TLEN; p++)    {
  tx = vec1 [p] * tr;
  vec1 [p] = (int) ((tx < 0.0) ? (tx - 0.5) : (tx + 0.5)) ;
  }

 recalcsumsq:
  /*    Calculate actual sum of squares for correction   */
  ts = 0.0;
  for (p = 0; p < TLEN; p++)    {       
  tx = vec1[p];
  ts += (tx * tx);
  }
  /*    Now ts should be Scale*Scale*TLEN or thereabouts   */
  ts = sqrt (ts / (Scale * Scale * TLEN));
  actualRSD = 1.0 / ts;   /* Reciprocal of actual Standard Devtn */
  goto startpass;

}

} // end of name space Statistics
} // end of name space itk

