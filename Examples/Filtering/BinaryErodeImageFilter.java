/**
 *  Example on the use of the BinaryErodeImageFilter
 *
 */

import InsightToolkit.*;

public class BinaryErodeImageFilter
{
  public static void main( String argv[] )
  {
    System.out.println("BinaryErodeImageFilter Example");

    itkImageFileReaderUC2_Pointer reader = itkImageFileReaderUC2.itkImageFileReaderUC2_New();
    itkImageFileWriterUC2_Pointer writer = itkImageFileWriterUC2.itkImageFileWriterUC2_New();

    itkBinaryErodeImageFilterUC2UC2_Pointer filter = itkBinaryErodeImageFilterUC2UC2.itkBinaryErodeImageFilterUC2UC2_New();

    filter.SetInput( reader.GetOutput() );
    writer.SetInput( filter.GetOutput() );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );


    itkBinaryBallStructuringElementUC2 element = new itkBinaryBallStructuringElementUC2();

    element.SetRadius( 1 );
    element.CreateStructuringElement();

    filter.SetKernel( element );

    short value = 255;

    filter.SetErodeValue( value );

    writer.Update();
  }

}


