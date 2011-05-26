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
 *  Example on the use of the GradientAnisotropicDiffusionImageFilter
 *
 */

import org.itk.io.*;
import org.itk.denoising.*;
import org.itk.intensityfilters.*;
import org.itk.simplefilters.*;


public class GradientAnisotropicDiffusionImageFilter
{
  public static void main( String argv[] )
  {
    itkImageFileReaderIUS2 reader = new itkImageFileReaderIUS2();
    itkImageFileWriterIUS2 writer = new itkImageFileWriterIUS2();

    itkCastImageFilterIUS2IF2 inputCast = new itkCastImageFilterIUS2IF2();

    itkGradientAnisotropicDiffusionImageFilterIF2IF2 filter = new itkGradientAnisotropicDiffusionImageFilterIF2IF2();

    itkRescaleIntensityImageFilterIF2IUS2 outputCast = new itkRescaleIntensityImageFilterIF2IUS2();

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
