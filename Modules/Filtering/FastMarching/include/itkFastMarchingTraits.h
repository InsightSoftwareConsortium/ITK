/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkFastMarchingTraits_h
#define itkFastMarchingTraits_h

#include "itkIntTypes.h"
#include "itkVectorContainer.h"
#include "itkConceptChecking.h"
#include "itkImage.h"
#include "itkQuadEdgeMesh.h"
#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"
#include "itkImageToImageFilter.h"
#include "itkNodePair.h"

namespace itk
{
/**  \class FastMarchingTraits
  \brief Base class traits to be used by the FastMarchingBase

  \tparam TInputDomain Input Domain type (e.g. itk::Image or itk::QuadEdgeMesh)
  \tparam TNode Node type where the front propagates
  \tparam TOutputDomain Output Domain type (similar to TInputDomain)
  \tparam TSuperclass Superclass. When dealing with itk::Image, the Superclass
  will be itk::ImageToImageFilter; and itk::QuadEdgeMeshToQuadEdgeMeshFilter
  when dealing with itk::QuadEdgeMesh

  \ingroup ITKFastMarching
  */
template <typename TInputDomain, typename TNode, typename TOutputDomain, typename TSuperclass>
class FastMarchingTraitsBase
{
public:
  /** Input Domain Type */
  using InputDomainType = TInputDomain;
  using InputDomainPointer = typename InputDomainType::Pointer;
  using InputPixelType = typename InputDomainType::PixelType;

  /** Node type */
  using NodeType = TNode;

  /** Output Domain Type */
  using OutputDomainType = TOutputDomain;
  using OutputDomainPointer = typename OutputDomainType::Pointer;
  using OutputPixelType = typename OutputDomainType::PixelType;

  using NodePairType = NodePair<NodeType, OutputPixelType>;
  using NodePairContainerType = VectorContainer<IdentifierType, NodePairType>;
  using NodePairContainerPointer = typename NodePairContainerType::Pointer;
  using NodePairContainerIterator = typename NodePairContainerType::Iterator;
  using NodePairContainerConstIterator = typename NodePairContainerType::ConstIterator;

  /*
  using NodeContainerType = VectorContainer< IdentifierType, NodeType >;
  using NodeContainerPointer = typename NodeContainerType::Pointer;
  using NodeContainerIterator = typename NodeContainerType::Iterator;
  using NodeContainerConstIterator = typename NodeContainerType::ConstIterator;
  */

  using SuperclassType = TSuperclass;

  /** \enum LabelEnum Fast Marching algorithm nodes types. */
  enum LabelType
  {
    /** \c Far represent far away nodes*/
    Far = 0,
    /** \c Alive represent nodes which have already been processed*/
    Alive,
    /** \c Trial represent nodes within a narrowband of the propagating front */
    Trial,
    /** \c InitialTrial represent nodes from where the propagation is initiated */
    InitialTrial,
    /** \c Forbidden represent nodes where the front can not propagate */
    Forbidden,
    /** \c Topology represent trial nodes but their inclusion would have
    violated topology checks. */
    Topology
  };

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro(DoubleConvertibleOutputCheck, (Concept::Convertible<double, OutputPixelType>));

  itkConceptMacro(OutputOStreamWritableCheck, (Concept::OStreamWritable<OutputPixelType>));
#endif
};


template <typename TInput, typename TOutput>
class FastMarchingTraits
{};

template <unsigned int VDimension,
          typename TInputPixel,
          typename TOutputPixel> // = TInputPixel >
class FastMarchingTraits<Image<TInputPixel, VDimension>, Image<TOutputPixel, VDimension>>
  : public FastMarchingTraitsBase<Image<TInputPixel, VDimension>,
                                  Index<VDimension>,
                                  Image<TOutputPixel, VDimension>,
                                  ImageToImageFilter<Image<TInputPixel, VDimension>, Image<TOutputPixel, VDimension>>>
{
public:
  static constexpr unsigned int ImageDimension = VDimension;
};


template <unsigned int VDimension,
          typename TInputPixel,
          typename TInputMeshTraits, //= QuadEdgeMeshTraits< TInputPixel, VDimension, bool, bool >,
          typename TOutputPixel,     //= TInputPixel,
          class TOutputMeshTraits    //= QuadEdgeMeshTraits< TOutputPixel, VDimension, bool, bool >
          >
class FastMarchingTraits<QuadEdgeMesh<TInputPixel, VDimension, TInputMeshTraits>,
                         QuadEdgeMesh<TOutputPixel, VDimension, TOutputMeshTraits>>
  : public FastMarchingTraitsBase<
      QuadEdgeMesh<TInputPixel, VDimension, TInputMeshTraits>,
      typename TInputMeshTraits::PointIdentifier,
      QuadEdgeMesh<TOutputPixel, VDimension, TOutputMeshTraits>,
      QuadEdgeMeshToQuadEdgeMeshFilter<QuadEdgeMesh<TInputPixel, VDimension, TInputMeshTraits>,
                                       QuadEdgeMesh<TOutputPixel, VDimension, TOutputMeshTraits>>>
{
public:
  static constexpr unsigned int PointDimension = VDimension;
};

} // namespace itk
#endif // itkFastMarchingTraits_h
