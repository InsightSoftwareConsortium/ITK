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

#include "itkFastMarchingStoppingCriterionBase.h"

namespace itk
{
template< typename TInput, typename TOutput >
class FastMarchingStoppingCriterionBaseHelperTest :
public FastMarchingStoppingCriterionBase< TInput, TOutput >
{
public:
  typedef FastMarchingStoppingCriterionBaseHelperTest           Self;
  typedef FastMarchingStoppingCriterionBase< TInput, TOutput >  Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  typedef typename Superclass::NodeType NodeType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingStoppingCriterionBaseHelperTest,
                FastMarchingStoppingCriterionBase );

  bool IsSatisfied() const ITK_OVERRIDE { return true; }
  std::string GetDescription() const ITK_OVERRIDE { return "Description"; }

protected:
  FastMarchingStoppingCriterionBaseHelperTest() : Superclass() {}
  ~FastMarchingStoppingCriterionBaseHelperTest() ITK_OVERRIDE {}

  void SetCurrentNode( const NodeType& ) ITK_OVERRIDE {}

  void Reset() ITK_OVERRIDE {}

private:
  FastMarchingStoppingCriterionBaseHelperTest( const Self& );
  void operator = ( const Self& );
};
}

int itkFastMarchingStoppingCriterionBaseTest( int , char *[] )
{
  typedef itk::Image< float, 2> ImageType;

  typedef itk::FastMarchingStoppingCriterionBaseHelperTest< ImageType, ImageType >
    ImageStoppingCriterionType;

  ImageStoppingCriterionType::Pointer image_criterion = ImageStoppingCriterionType::New();
  if( image_criterion.IsNull() )
    {
    return EXIT_FAILURE;
    }

  typedef itk::QuadEdgeMesh< float, 3 > MeshType;

  typedef itk::FastMarchingStoppingCriterionBaseHelperTest< MeshType, MeshType >
      MeshStoppingCriterionType;

  MeshStoppingCriterionType::Pointer mesh_criterion = MeshStoppingCriterionType::New();
  if( mesh_criterion.IsNull() )
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
