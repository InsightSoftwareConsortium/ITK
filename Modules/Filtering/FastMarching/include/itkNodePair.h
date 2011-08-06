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

#ifndef __itkNodePair_h
#define __itkNodePair_h

#include <utility>

namespace itk
{
/**
\class NodePair
\brief Represents a Node and its associated value (front value)

\ingroup ITKFastMarching
*/
template<class NodeType, class OutputPixelType>
class NodePair : public std::pair< NodeType, OutputPixelType >
  {
public:
  typedef NodePair                               Self;
  typedef std::pair< NodeType, OutputPixelType > Superclass;

  NodePair() : Superclass() {}
  NodePair( const NodeType& iNode, const OutputPixelType& iValue ) :
    Superclass( iNode, iValue ) {}
  NodePair( const Self& iPair ) : Superclass( iPair ) {}

  void operator = ( const Self& iPair )
    {
    this->first = iPair.first;
    this->second = iPair.second;
    }

  void SetValue( const OutputPixelType& iValue )
    {
    this->second = iValue;
    }
  OutputPixelType GetValue() const
    {
    return this->second;
    }
  void SetNode( const NodeType& iNode )
    {
    this->first = iNode;
    }
  NodeType GetNode() const
    {
    return this->first;
    }

  bool operator < ( const Self& iRight ) const
    {
    return this->second < iRight.second;
    }

  bool operator > ( const Self& iRight ) const
    {
    return this->second > iRight.second;
    }

  bool operator <= ( const Self& iRight ) const
    {
    return this->second <= iRight.second;
    }

  bool operator >= ( const Self& iRight ) const
    {
    return this->second >= iRight.second;
    }
  };

}
#endif // __itkFastMarchingTraits_h
