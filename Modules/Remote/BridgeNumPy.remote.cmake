# Contact: Matt McCormick <matt.mccormick@kitware.com>
itk_fetch_module(BridgeNumPy
  "This module contains wrapper code to convert ITK Image's to
  NumPy array's and back.  For example:

    import itk
    ImageType = itk.Image[itk.F, 2]
    image = ImageType.New()
    array = itk.PyBuffer[ImageType].GetArrayFromImage(image)
    image = itk.PyBuffer[ImageType].GetImageFromArray(array)

  It also enables conversion of a VNL vector or matrix into a NumPy array and
  back. For example:

    import itk
    v = itk.vnl_vector[itk.F]()
    v.set_size(2)
    arr = itk.PyVnl[itk.F].GetArrayFromVnlVector(v)
    v = itk.PyVnl[itk.F].GetVnlVectorFromArray(arr)

  See http://insight-journal.org/browse/publication/85
      https://hdl.handle.net/1926/188"
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKBridgeNumPy.git
  GIT_TAG 5ffa0941ecf9c605583e32aede55a644b6b65df2
  )
