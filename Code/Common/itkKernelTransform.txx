#include "itkKernelTransform.h"

namespace itk
{

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::IMatrixType
KernelTransform<TScalarType, NDimensions>::m_I;

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::KernelTransform() : 
Transformation<TScalarType, NDimensions>()
{
  static bool IMatrixInitialized = false;

  if (!IMatrixInitialized)
  {
    m_I.set_identity();
    IMatrixInitialized = true;
  }
  m_p = new PointListType;
  m_q = new PointListType;
  m_d = new VectorListType;
  m_LMatrix = NULL;
  m_KMatrix = NULL;
  m_PMatrix = NULL;
  m_YMatrix = NULL;
  m_WMatrix = NULL;
  m_WMatrixComputed = false;
}

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::~KernelTransform()
{
  if (m_p != NULL)
  {
    delete m_p;
  }
  if (m_q != NULL)
  {
    delete m_q;
  }
  if (m_d != NULL)
  {
    delete m_d;
  }
  if (m_LMatrix != NULL)
  {
    delete m_LMatrix;
  }
  if (m_KMatrix != NULL)
  {
    delete m_KMatrix;
  }
  if (m_PMatrix != NULL)
  {
    delete m_PMatrix;
  }
  if (m_YMatrix != NULL)
  {
    delete m_YMatrix;
  }
  if (m_WMatrix != NULL)
  {
    delete m_WMatrix;
  }
}

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::PointListType*
KernelTransform<TScalarType, NDimensions>::Getp()
{
	return m_p;
}

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::PointListType*
KernelTransform<TScalarType, NDimensions>::Getq()
{
	return m_q;
}

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::VectorListType*
KernelTransform<TScalarType, NDimensions>::Getd()
{
	return m_d;
}

/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::ComputeD()
{
  int numLandmarks = m_p->size();
  int i;
  PointType* q;
  PointType* p;
  VectorType* d;

  m_d->clear();
  for (i = 0; i < numLandmarks; i++) {
    q = (*m_q)[i];
    p = (*m_p)[i];
    d = new VectorType(*q - *p);
    m_d->push_back(d);
  }
}

/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::ComputeW()
{
  int numLandmarks = m_p->size();
  vnl_svd<TScalarType>* svd;

  ComputeL();
  ComputeY();
  if (m_WMatrix != NULL)
  {
    delete m_WMatrix;
  }
  m_WMatrix = new WMatrixType(NDimensions*(numLandmarks+NDimensions+1), 1);
  svd = new vnl_svd<TScalarType>(*m_LMatrix, 1e-8);
  *m_WMatrix = svd->solve(*m_YMatrix);
	//std::cout << *m_WMatrix << std::endl;
}

/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::ComputeL()
{
  int numLandmarks = m_p->size();
  vnl_matrix<TScalarType> O2(NDimensions*(NDimensions+1),
                             NDimensions*(NDimensions+1), 0);

  ComputeP();
  ComputeK();
  if (m_LMatrix != NULL)
  {
    delete m_LMatrix;
  }
  m_LMatrix = new LMatrixType(NDimensions*(numLandmarks+NDimensions+1),
                              NDimensions*(numLandmarks+NDimensions+1));
  m_LMatrix->update(*m_KMatrix, 0, 0);
  m_LMatrix->update(*m_PMatrix, 0, m_KMatrix->columns());
  m_LMatrix->update(m_PMatrix->transpose(), m_KMatrix->rows(), 0);
  m_LMatrix->update(O2, m_KMatrix->rows(), m_KMatrix->columns());
}

/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::ComputeK()
{
  int numLandmarks = m_p->size();
  GMatrixType G;
  VectorType s;
  int i, j;

  ComputeD();
  if (m_KMatrix != NULL)
  {
    delete m_KMatrix;
  }
  m_KMatrix = new KMatrixType(NDimensions*numLandmarks,
                              NDimensions*numLandmarks);
  for (i = 0; i < numLandmarks; i++)
  {
    for (j = 0; j < numLandmarks; j++)
    {
      s = *((*m_p)[i]) - *((*m_p)[j]);
      G = ComputeG(s);
      m_KMatrix->update(G, i*NDimensions, j*NDimensions);
    }
  }
}

/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::ComputeP()
{
  int numLandmarks = m_p->size();
  IMatrixType I;
  IMatrixType temp;
  int i, j;

  I.set_identity();
  if (m_PMatrix != NULL)
  {
    delete m_PMatrix;
  }
  m_PMatrix = new PMatrixType(NDimensions*numLandmarks,
                              NDimensions*(NDimensions+1));
  for (i = 0; i < numLandmarks; i++)
  {
    for (j = 0; j < NDimensions; j++)
    {
      temp = I * (*m_p)[i]->Get_vnl_vector()[j];
      m_PMatrix->update(temp, i*NDimensions, j*NDimensions);
    }
    m_PMatrix->update(I, i*NDimensions, j*NDimensions);
  }
}

/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::ComputeY()
{
  int i, j;
  int numLandmarks = m_p->size();

  if (m_YMatrix != NULL)
  {
    delete m_YMatrix;
  }

  m_YMatrix = new YMatrixType(NDimensions*(numLandmarks+NDimensions+1), 1);
  for (i = 0; i < numLandmarks; i++)
  {
    for (j = 0; j < NDimensions; j++)
    {
      m_YMatrix->put(i*NDimensions+j, 0, (*m_d)[i]->Get_vnl_vector()[j]);
    }
  }
  for (i = 0; i < NDimensions*(NDimensions+1); i++) {
    m_YMatrix->put(numLandmarks*NDimensions+i, 0, 0);
  }
}

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::PointType
KernelTransform<TScalarType, NDimensions>::Transform(const PointType& thisPoint) const
{
  int numLandmarks = m_p->size();
  int i, j;
  ColumnMatrixType b, c, d, Ax;
  vnl_matrix_fixed<TScalarType, NDimensions, NDimensions> A;
  PointType result;
  VectorType argumentG;

  d = d*0;
  for (i = 0; i < numLandmarks; i++)
  {
    c.update(m_WMatrix->extract(NDimensions, 1, i*NDimensions, 0), 0, 0);
    argumentG = thisPoint - *((*m_p)[i]);
    d = d + ComputeG(argumentG) * c;
  }
  for (i = 0; i < NDimensions; i++)
  {
    A.update(m_WMatrix->extract(NDimensions, 1,
                                (numLandmarks+i)*NDimensions, 0), 0, i);
  }
  b.update(m_WMatrix->extract(NDimensions, 1,
                              (numLandmarks+NDimensions)*NDimensions, 0),
                              0, 0);
  for (j = 0; j < NDimensions; j++)
  {
    Ax.put(j, 0, thisPoint[j]);
  }
  Ax = A*Ax;
  d = d + Ax + b;
  for (j = 0; j < NDimensions; j++)
  {
    result[j] = thisPoint[j] + d.get(j, 0);
  }
  return result;
}

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::VectorType
KernelTransform<TScalarType, NDimensions>::Transform(const VectorType& thisVector) const
{
  int numLandmarks = m_p->size();
  int i, j;
  ColumnMatrixType c, d, Ax;
  VectorType result;
  VectorType GArgument;
  vnl_matrix_fixed<TScalarType, NDimensions, NDimensions> A;
  VectorType thisVectorCopy = thisVector;

  d = d*0;
  for (i = 0; i < numLandmarks; i++)
  {
    c.update(m_WMatrix->extract(NDimensions, 1, i*NDimensions, 0), 0, 0);
    for (j = 0; j < NDimensions; j++)
    {
      GArgument[j] = thisVectorCopy.Get_vnl_vector()[j] -
                     (*m_p)[i]->Get_vnl_vector()[j];
    }
    d = d + ComputeG(GArgument) * c;
  }
  A.update(m_WMatrix->extract(NDimensions, 1,
                              numLandmarks*NDimensions, 0), 0, 0);
  A.update(m_WMatrix->extract(NDimensions, 1,
                              (numLandmarks+1)*NDimensions, 0), 0, 1);
  A.update(m_WMatrix->extract(NDimensions, 1,
                              (numLandmarks+2)*NDimensions, 0), 0, 2);
  for (j = 0; j < NDimensions; j++)
  {
    Ax.put(j, 0, thisVector[j]);
  }
  Ax = A*Ax;
  d = d + Ax;
  for (i = 0; i < NDimensions; i++)
  {
    result[i] = thisVector[i] + d.get(i, 0);
  }

  return result;
}

/**
 *
 */
template<class TScalarType, int NDimensions>
inline
std::ostream&
operator<< (std::ostream &s,
            KernelTransform<TScalarType, NDimensions>& transform)
{
    return transform.PrintSelf(s);
}

} // namespace itk
