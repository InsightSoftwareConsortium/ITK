itk_fetch_module(MultipleImageIterator
"Several applications such as multi-atlas segmentation
require frequent iteration over multiple image volumes at the same time.
Doing so with the regular ITK iterators is tedious and error prone
as it requires updating each iterator at end of each iteration.
Failing to do so results in hard to debug errors and crashes.
The MultipleImageIterator is a simple wrapper class that tries to make this more convenient.

A more detailed description can be found in the Insight Journal article::
Schaerer J. \"A MultipleImageIterator for iterating over multiple images simultaneously\".
  http://hdl.handle.net/10380/3455
  http://www.insight-journal.org/browse/publication/915
  December, 2014.
"
  GIT_REPOSITORY ${git_protocol}://github.com/KitwareMedical/MultipleImageIterator.git
  GIT_TAG 1105469ce1c2cc289008d9cb6bebde487a39ae3e
  )
