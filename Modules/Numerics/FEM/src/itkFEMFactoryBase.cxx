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
#include "itkFEMFactory.h"
#include "itkVersion.h"
#include "itkFEMElement2DC0LinearLineStress.h"
#include "itkFEMElement2DC0LinearQuadrilateralMembrane.h"
#include "itkFEMElement2DC0LinearQuadrilateralStrain.h"
#include "itkFEMElement2DC0LinearQuadrilateralStress.h"
#include "itkFEMElement2DC0LinearTriangularMembrane.h"
#include "itkFEMElement2DC0LinearTriangularStrain.h"
#include "itkFEMElement2DC0LinearTriangularStress.h"
#include "itkFEMElement2DC0QuadraticTriangularStrain.h"
#include "itkFEMElement2DC0QuadraticTriangularStress.h"
#include "itkFEMElement2DC1Beam.h"
#include "itkFEMElement3DC0LinearHexahedronMembrane.h"
#include "itkFEMElement3DC0LinearHexahedronStrain.h"
#include "itkFEMElement3DC0LinearTetrahedronMembrane.h"
#include "itkFEMElement3DC0LinearTetrahedronStrain.h"
#include "itkFEMElement3DC0LinearTriangularLaplaceBeltrami.h"
#include "itkFEMElement3DC0LinearTriangularMembrane.h"

#include "itkFEMLoadPoint.h"


#include "itkMetaFEMObjectConverter.h"

namespace itk
{
FEMFactoryBase * FEMFactoryBase::m_Factory = ITK_NULLPTR;
SimpleFastMutexLock FEMFactoryBase::m_CreationLock;

FEMFactoryBase::FEMFactoryBase()
{
}

FEMFactoryBase::~FEMFactoryBase()
{
}

void FEMFactoryBase::RegisterDefaultTypes()
{
  //if( m_Factory == 0 )
    {
    FEMFactory<itk::fem::Element::Node>::RegisterType();
    FEMFactory<itk::fem::Element2DC0LinearLineStress>::RegisterType();
    FEMFactory<itk::fem::Element2DC0LinearQuadrilateralMembrane>::RegisterType();
    FEMFactory<itk::fem::Element2DC0LinearQuadrilateralStrain>::RegisterType();
    FEMFactory<itk::fem::Element2DC0LinearQuadrilateralStress>::RegisterType();
    FEMFactory<itk::fem::Element2DC0LinearTriangularMembrane>::RegisterType();
    FEMFactory<itk::fem::Element2DC0LinearTriangularStrain>::RegisterType();
    FEMFactory<itk::fem::Element2DC0LinearTriangularStress>::RegisterType();
    FEMFactory<itk::fem::Element2DC0QuadraticTriangularStrain>::RegisterType();
    FEMFactory<itk::fem::Element2DC0QuadraticTriangularStress>::RegisterType();
    FEMFactory<itk::fem::Element2DC1Beam>::RegisterType();
    FEMFactory<itk::fem::Element3DC0LinearHexahedronMembrane>::RegisterType();
    FEMFactory<itk::fem::Element3DC0LinearHexahedronStrain>::RegisterType();
    FEMFactory<itk::fem::Element3DC0LinearTetrahedronMembrane>::RegisterType();
    FEMFactory<itk::fem::Element3DC0LinearTetrahedronStrain>::RegisterType();
    FEMFactory<itk::fem::Element3DC0LinearTriangularLaplaceBeltrami>::RegisterType();
    FEMFactory<itk::fem::Element3DC0LinearTriangularMembrane>::RegisterType();
    FEMFactory<itk::fem::LoadBC>::RegisterType();
    FEMFactory<itk::fem::LoadBCMFC>::RegisterType();
    FEMFactory<itk::fem::LoadEdge>::RegisterType();
    FEMFactory<itk::fem::LoadLandmark>::RegisterType();
    FEMFactory<itk::fem::LoadNode>::RegisterType();
    FEMFactory<itk::fem::LoadPoint>::RegisterType();
    FEMFactory<itk::fem::LoadGravConst>::RegisterType();
    FEMFactory<itk::fem::LoadElement>::RegisterType();
    FEMFactory<itk::fem::MaterialLinearElasticity>::RegisterType();
    }
}

const char *
FEMFactoryBase::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
FEMFactoryBase::GetDescription() const
{
  return "FEM Factory Base";
}

} // end namespace itk
