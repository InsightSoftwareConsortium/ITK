/**
 *  Example on the use of the BinaryDilateImageFilter
 *
 */

import InsightToolkit.*;

public class BinaryDilateImageFilter
{
  public static void main( String argv[] )
  {
    System.out.println("BinaryDilateImageFilter Example");

    itkImageFileReaderUC2_Pointer reader = itkImageFileReaderUC2.itkImageFileReaderUC2_New();
    itkImageFileWriterUC2_Pointer writer = itkImageFileWriterUC2.itkImageFileWriterUC2_New();

    itkBinaryDilateImageFilterUC2UC2_Pointer filter = itkBinaryDilateImageFilterUC2UC2.itkBinaryDilateImageFilterUC2UC2_New();

    filter.SetInput( reader.GetOutput() );
    writer.SetInput( filter.GetOutput() );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );


    itkBinaryBallStructuringElementUC2 element = new itkBinaryBallStructuringElementUC2();

    element.SetRadius( 1 );
    element.CreateStructuringElement();

    filter.SetKernel( element );

    short value = 255;

    filter.SetDilateValue( value );

    writer.Update();
  }

}


