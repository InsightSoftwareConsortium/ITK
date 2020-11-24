from sys import stderr as _system_error_stream

# The following line defines an ascii string used for dynamically refreshing
# the import and progress callbacks on the same terminal line.
# See http://www.termsys.demon.co.uk/vtansi.htm
# \033 is the C-style octal code for an escape character
# [2000D moves the cursor back 2000 columns, this is a brute force way of
# getting to the start of the line.
# [K erases the end of the line
clrLine = "\033[2000D\033[K"


def auto_not_in_place(v=True):
    """Force it to not run in place"""
    import itkConfig

    itkConfig.NotInPlace = v


def auto_progress(progress_type=1):
    """Set up auto progress report

    progress_type:
        1 or True -> auto progress be used in a terminal
        2 -> simple auto progress (without special characters)
        0 or False -> disable auto progress
    """
    import itkConfig

    if progress_type is True or progress_type == 1:
        itkConfig.ImportCallback = terminal_import_callback
        itkConfig.ProgressCallback = terminal_progress_callback

    elif progress_type == 2:
        itkConfig.ImportCallback = simple_import_callback
        itkConfig.ProgressCallback = simple_progress_callback

    elif progress_type is False or progress_type == 0:
        itkConfig.ImportCallback = None
        itkConfig.ProgressCallback = None

    else:
        raise ValueError("Invalid auto progress type: " + repr(progress_type))


def terminal_progress_callback(name, p):
    """Display the progress of an object and clean the display once complete

    This function can be used with itkConfig.ProgressCallback
    """
    print(clrLine + f"{name}: {p:f}", file=_system_error_stream, end="")
    if p == 1:
        print(clrLine, file=_system_error_stream, end="")


def terminal_import_callback(name, p):
    """Display the loading of a module and clean the display once complete

    This function can be used with itkConfig.ImportCallback
    """
    print(clrLine + f"Loading {name}... ", file=_system_error_stream, end="")
    if p == 1:
        print(clrLine, file=_system_error_stream, end="")


def simple_import_callback(name, p):
    """Print a message when a module is loading

    This function can be used with itkConfig.ImportCallback
    """
    if p == 0:
        print(f"Loading {name}... ", file=_system_error_stream, end="")
    elif p == 1:
        print("done", file=_system_error_stream)


def simple_progress_callback(name, p):
    """Print a message when an object is running

    This function can be used with itkConfig.ProgressCallback
    """
    if p == 0:
        print(f"Running {name}... ", file=_system_error_stream, end="")
    elif p == 1:
        print("done", file=_system_error_stream)


def force_load():
    """force itk to load all the submodules"""
    import itk

    for k in dir(itk):
        getattr(itk, k)
