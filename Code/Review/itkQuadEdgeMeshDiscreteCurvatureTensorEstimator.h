/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshDiscreteCurvatureTensorEstimator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshDiscreteCurvatureTensorEstimator_h
#define __itkQuadEdgeMeshDiscreteCurvatureTensorEstimator_h

namespace itk
{
/**
 * \class QuadEdgeMeshDiscreteCurvatureTensorEstimator
 *
 * \brief FIXME Add documentation here
 *
 */
template< class TInputMesh, class TOutputMesh >
class QuadEdgeMeshDiscreteCurvatureTensorEstimator:
  public QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
{
public:
  typedef QuadEdgeMeshDiscreteCurvatureTensorEstimator Self;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter             Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(QuadEdgeMeshDiscreteCurvatureTensorEstimator, QuadEdgeMeshToQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);
protected:
  QuadEdgeMeshDiscreteCurvatureTensorEstimator() {}
  ~QuadEdgeMeshDiscreteCurvatureTensorEstimator() {}

  ///TODO to be implemented
  virtual void GenerateData()
  {}

private:
  QuadEdgeMeshDiscreteCurvatureTensorEstimator(const Self &); // purposely not
                                                              // implemented
  void operator=(const Self &);                               // purposely not
                                                              // implemented
};
}

#endif
