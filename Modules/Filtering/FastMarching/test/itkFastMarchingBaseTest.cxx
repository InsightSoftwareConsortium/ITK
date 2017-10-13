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

#include "itkFastMarchingBase.h"

namespace itk
{
template< typename TInput, typename TOutput >
class FastMarchingBaseTestHelper :
    public FastMarchingBase< TInput, TOutput >
{
public:
  typedef FastMarchingBaseTestHelper          Self;
  typedef FastMarchingBase< TInput, TOutput > Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingBaseTestHelper, FastMarchingBase);

  typedef typename Superclass::Traits           Traits;
  typedef typename Superclass::OutputDomainType OutputDomainType;

//  typedef typename Superclass::NodeContainerType  NodeContainerType;
  typedef typename Superclass::NodeType         NodeType;

  typedef typename Superclass::OutputPixelType  OutputPixelType;
  typedef typename Superclass::LabelType        LabelType;

protected:
  FastMarchingBaseTestHelper() {}
  ~FastMarchingBaseTestHelper() ITK_OVERRIDE {}

  IdentifierType GetTotalNumberOfNodes() const ITK_OVERRIDE
    { return 1; }

  void SetOutputValue( OutputDomainType*,
                      const NodeType&,
                      const OutputPixelType& ) ITK_OVERRIDE
    {
    }

  const OutputPixelType GetOutputValue( OutputDomainType* ,
                                  const NodeType& ) const ITK_OVERRIDE
    {
    return NumericTraits< OutputPixelType >::ZeroValue();
    }

  unsigned char GetLabelValueForGivenNode( const NodeType& ) const ITK_OVERRIDE
    {
    return Traits::Far;
    }

  void SetLabelValueForGivenNode( const NodeType& ,
                                 const LabelType& ) ITK_OVERRIDE
    {}

  void UpdateNeighbors( OutputDomainType* , const NodeType& ) ITK_OVERRIDE
    {}

  void UpdateValue( OutputDomainType* , const NodeType& ) ITK_OVERRIDE
    {}

  bool CheckTopology( OutputDomainType* , const NodeType&  ) ITK_OVERRIDE
    { return true; }

  void InitializeOutput( OutputDomainType* ) ITK_OVERRIDE {}

private:
  FastMarchingBaseTestHelper( const Self& );
  void operator = ( const Self& );
};
}

// -----------------------------------------------------------------------------

int itkFastMarchingBaseTest( int argc, char* argv[] )
{
  if( argc != 2 )
    {
    return EXIT_FAILURE;
    }

  typedef float PixelType;

  bool exception_caught = false;

  if( atoi( argv[1] ) == 0 )
    {
    const unsigned Dimension = 3;
    typedef itk::Image<PixelType, Dimension> ImageType;

    ImageType::Pointer input = ImageType::New();

    typedef itk::FastMarchingBaseTestHelper< ImageType, ImageType >
        ImageFastMarching;
    ImageFastMarching::Pointer fmm = ImageFastMarching::New();
    fmm->SetInput( input );

    try
      {
      fmm->Update();
      }
    catch( itk::ExceptionObject & excep )
      {
      std::cerr << "Exception caught !" << std::endl;
      std::cerr << excep << std::endl;
      exception_caught = true;
      }

    typedef ImageFastMarching::OutputDomainType OutputImageType;
    OutputImageType::Pointer output = fmm->GetOutput();

    (void) output;
    }
  else
    {
    if( atoi( argv[1] ) == 1 )
      {
      typedef itk::QuadEdgeMesh<PixelType, 3, itk::QuadEdgeMeshTraits< PixelType, 3, bool, bool > > MeshType;

      MeshType::Pointer input = MeshType::New();

      typedef itk::FastMarchingBaseTestHelper< MeshType, MeshType >
          MeshFastMarching;
      MeshFastMarching::Pointer fmm = MeshFastMarching::New();
      fmm->SetInput( input );

      try
        {
        fmm->Update();
        }
      catch( itk::ExceptionObject & excep )
        {
        std::cerr << "Exception caught !" << std::endl;
        std::cerr << excep << std::endl;
        exception_caught = true;
        }

      typedef MeshFastMarching::OutputDomainType OutputMeshType;
      OutputMeshType::Pointer output = fmm->GetOutput();

      (void) output;
      }
    }

  if( exception_caught )
    {
    return EXIT_SUCCESS;
    }
  else
    {
    return EXIT_FAILURE;
    }
}
