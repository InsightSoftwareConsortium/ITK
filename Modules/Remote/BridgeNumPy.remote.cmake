# Contact: Matt McCormick <matt.mccormick@kitware.com>
itk_fetch_module(BridgeNumPy
  "This module contains wrapper code to convert ITK Image's to
  NumPy array's and back.  For example:

    import itk
    ImageType = itk.Image[itk.F, 2]
    image = ImageType.New()
    array = itk.PyBuffer[ImageType].GetArrayFromImage(image)
    image = itk.PyBuffer[ImageType].GetImageFromArray(array)

  See http://insight-journal.org/browse/publication/85
      http://hdl.handle.net/1926/188"
  GIT_REPOSITORY https://github.com/InsightSoftwareConsortium/ITKBridgeNumPy.git
  GIT_TAG f645d226d3f2980c48fd15a9240ae0ce23deb046
  )
