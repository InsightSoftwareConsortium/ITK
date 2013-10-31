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

/**
 *  Example on the use of the CurvatureAnisotropicDiffusionImageFilter
 *
 */

import InsightToolkit.*;


public class CurvatureAnisotropicDiffusionImageFilter
{
  public static void main( String argv[] )
  {
    itkImageFileReaderUS2_Pointer reader = itkImageFileReaderUS2.itkImageFileReaderUS2_New();
    itkImageFileWriterUS2_Pointer writer = itkImageFileWriterUS2.itkImageFileWriterUS2_New();

    itkCastImageFilterUS2F2_Pointer inputCast = itkCastImageFilterUS2F2.itkCastImageFilterUS2F2_New();

    itkCurvatureAnisotropicDiffusionImageFilterF2F2_Pointer filter = itkCurvatureAnisotropicDiffusionImageFilterF2F2.itkCurvatureAnisotropicDiffusionImageFilterF2F2_New();

    itkRescaleIntensityImageFilterF2US2_Pointer outputCast = itkRescaleIntensityImageFilterF2US2.itkRescaleIntensityImageFilterF2US2_New();

    inputCast.SetInput( reader.GetOutput() );
    filter.SetInput( inputCast.GetOutput() );
    outputCast.SetInput( filter.GetOutput() );
    writer.SetInput( outputCast.GetOutput() );

    outputCast.SetOutputMinimum(  0  );
    outputCast.SetOutputMaximum( 255 );

    filter.SetNumberOfIterations(   Integer.parseInt( argv[2] ) );
    filter.SetTimeStep(             Float.parseFloat( argv[3] ) );
    filter.SetConductanceParameter( Float.parseFloat( argv[4] ) );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    writer.Update();
  }

}


