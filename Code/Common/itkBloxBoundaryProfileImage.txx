#ifndef __itkBloxBoundaryProfileImage_txx
#define __itkBloxBoundaryProfileImage_txx

#include "itkBloxBoundaryProfileImage.h"
#include "itkMultipleValuedCostFunction.h"

#include "itkSymmetricEllipsoidInteriorExteriorSpatialFunction.h"

#include "itkLevenbergMarquardtOptimizer.h"

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_math.h>
#include <itkArray.h>
#include <itkArray2D.h>

typedef vnl_matrix<double> MatrixType;
typedef vnl_vector<double> VectorType;

namespace itk
{

template <unsigned int VImageDimension>
BloxBoundaryProfileImage<VImageDimension>
::BloxBoundaryProfileImage()
{
 // m_NumBoundaryProfiles = 0;  
//  m_UniqueAxis = 0;
//  m_SymmetricAxes = 0;

//  m_Accumulator = new double[m_NumberOfBins];
//  m_Normalizer = new double[m_NumberOfBins];
//  m_NormalizedAccumulator = new double[m_NumberOfBins];
//  m_FinalParameters = new double[m_SpaceDimension];
}

template <unsigned int VImageDimension>
BloxBoundaryProfileImage<VImageDimension>
::~BloxBoundaryProfileImage()
{

}


template <unsigned int VImageDimension>
void
BloxBoundaryProfileImage<VImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

/*
  unsigned int i;
  os << indent << "Boundary point image origin: [";
  for (i=0; i < NDimensions - 1; i++)
    {
    os << m_BPImageOrigin[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Boundary point image spacing: [";
  for (i=0; i < NDimensions - 1; i++)
    {
    os << m_BPImageSpacing[i] << ", ";
    }
  os << "]" << std::endl;
*/
}





} // end namespace itk

#endif
