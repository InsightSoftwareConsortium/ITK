/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnaryMedialNodeMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkUnaryMedialNodeMetric_h
#define __itkUnaryMedialNodeMetric_h

#include "itkLightObject.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkBloxCoreAtomPixel.h"

namespace itk
{
  
/** \class UnaryMedialNodeMetric
 *  \brief Compares the scale and dimensionality of two medial nodes.
 * 
 * The class is templated over image dimension. The metric 
 * measures the similarity of two medial nodes based on 
 * their eigenvalues and scales.  The unary metric is
 * calculated by u = sum(L^2 * S), where L is the difference in 
 * eigenvalues and S is the ratio of the difference in
 * medial node scales and sum of the medial node scales.
 * The metric is normalized such that a value of 0
 * means that the nodes are perfectly similar and that 
 * a value of 1 means that the nodes are not similar.
 *
 * Reference: Tamburo, Cois, Shelton, Stetten. Medial Node
 * Correspondences Towards Automated Registration, Lecture
 * Notes in Computer Science (in press), 2003.
 *
 * \ingroup ImageFeatureSimilarityMetrics 
*/

template<int VDimensions = 3>
class UnaryMedialNodeMetric : public LightObject 
{
public:

  /** Standard class typedefs. */
  typedef UnaryMedialNodeMetric           Self;
  typedef LightObject                     Superclass; 
  typedef SmartPointer<Self>              Pointer;
  typedef SmartPointer<const Self>        ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(UnaryMedialNodeMetric, LightObject);

  /** Pixel typedef. */
  typedef BloxCoreAtomPixel<VDimensions> MedialNodeType;
  typedef typename BloxCoreAtomPixel<VDimensions>::Pointer MedialNodePointerType;
  typedef typename MedialNodeType::EigenvalueType EigenvalueType;

  /** Initialize and compute the Unary Metric. */
  void Initialize(void);

  /** Set the two medial nodes to compute a unary metric for. */
  void SetMedialNodes(MedialNodeType * medialNodeA, MedialNodeType * medialNodeB);

  /** Return the resulting unary metric value for a 
   *  given two medial nodes. */
  double GetResult(){return m_MetricResult;}

protected:

  /** Default Constructor. */
  UnaryMedialNodeMetric();

  /** Default Destructor. */
  ~UnaryMedialNodeMetric(){}

  void operator=(const Self&); //purposely not implemented

  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  
  /** The two medial nodes to compute the unary metric for. */
  MedialNodeType * m_MedialNodeA;
  MedialNodeType * m_MedialNodeB;

  /** Resulting metric value. */
  double m_MetricResult;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkUnaryMedialNodeMetric.txx"
#endif

#endif



