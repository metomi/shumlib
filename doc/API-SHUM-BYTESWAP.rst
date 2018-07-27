API Reference: shum_byteswap
----------------------------

When performing reads from disk there are two different orders in which the 
individual bytes making up a word/record can be read; these forms are known as 
"big endian" or "little endian" byte ordering, taken from the "end" of the word
where the read is started from (and the fact that bytes correspond to larger 
values at one end of the word).

If the machine's default byte ordering is opposite to the one for a given file,
the data read in (or written) will not convert to the intended values correctly.  
This library allows the ordering of data to be swapped before writing or after
reading to solve this issue.  The UM uses exclusively big-endian input/output
files and so often needs to perform swapping when run on little-endian machines.

Note that this library is intended as an *alternative* to any compiler provided 
non-standard swapping functionality, and as such they should not be used 
together.

Fortran Functions/Subroutines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

``get_shum_byteswap_version``
'''''''''''''''''''''''''''''

All Shumlib libraries expose a module and function named in this format; it 
allows access to the Shumlib version number used when compiling the library.

    **Available via module**
        ``f_shum_byteswap_version_mod``

    **Syntax**
        ``version = get_shum_byteswap_version()``

    **Returns**
        ``version (INTEGER)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

``f_shum_byteswap``
'''''''''''''''''''

This function performs an in-place byteswap on a provided array.  

    **Available via module**
        ``f_shum_byteswap_mod``

    **Syntax**
        ``status = f_shum_byteswap(bytes, swap_words, word_len, message)``

    **Inputs**
        ``swap_words (INTEGER)``
            The number of words (of size ``word_len`` which should be swapped).
        ``word_len (INTEGER)``
            The length (in bytes) of the words to be swapped.  Supported lengths
            are 2, 4 or 8 byte words.

    **Input & Output**
        ``bytes (various kinds/ranks supported - see description)``
            The array which should be swapped (in place) - this may be a 1d
            or 2d array of type ``REAL`` or ``INTEGER`` and may be either
            32-bit or 64-bit.
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

    **Notes**
        The ``INTEGER`` arguments here (except for the ``bytes`` argument)
        must be either *all* 32-bit or *all* 64-bit (but *not* a mixture of the
        two).

        It is not necessary for the ``word_len`` to match the size of the
        elements of ``bytes`` (and it may sometimes be required to swap an array
        with a different word size in memory to its representation on disk).
        However as a result of this some care must be taken to ensure the total
        size for the swap requested does not exceed the extent of the passed
        array (if it does the call will fail with a non-zero error code).

``f_shum_get_machine_endianism``
''''''''''''''''''''''''''''''''

This function should be used to test whether or not a swap is required.

    **Available via module**
        ``f_shum_byteswap_mod``

    **Syntax**
        ``endianism = f_shum_get_machine_endianism()``

    **Return Value**
        ``endianism (ENUM defined in module)``
            The native endianism of the machine. This can take one of two
            values which are also available in ``f_shum_byteswap_mod``;
            ``f_shum_bigendian`` and ``f_shum_littleendian``.  Tests on
            the return value of this function should always be performed
            against one of these variables.

C Functions
%%%%%%%%%%%

``get_shum_byteswap_version``
'''''''''''''''''''''''''''''

All Shumlib libraries expose a function named in this format; it allows access
to the Shumlib version number used when compiling the library.

    **Required header/s**
        ``c_shum_byteswap_version.h``

    **Syntax**
        ``version = get_shum_byteswap_version()``

    **Returns**
        ``version (int)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

``c_shum_byteswap``
'''''''''''''''''''

This function performs an in-place byteswap of an array.

    **Required header/s**
        ``c_shum_byteswap.h``
        ``c_shum_byteswap_opt.h``

    **Syntax**
        ``c_shum_byteswap(array, len, word_len, message, message_len)``

    **Arguments**
        ``array (void*)``
            Pointer to the start of the array to swap.
        ``len (int64_t*)``
            The number of words (of size ``word_len`` which should be swapped).
        ``word_len (int64_t*)``
            The length (in bytes) of the words to be swapped.  Supported lengths
            are 2, 4 or 8 byte words.
        ``message (char*)``
            Error message buffer.
        ``message_len (int64_t*)``
            Length of error message buffer.

    **Return Value**
        ``(int64_t)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

    **Notes**
        It is not necessary for the ``word_len`` to match the size of the
        elements of ``array`` (and it may sometimes be required to swap an array
        with a different word size in memory to its representation on disk).
        However as a result of this some care must be taken to ensure the total
        size for the swap requested does not exceed the extent of the passed
        array.

``c_shum_get_machine_endianism``
''''''''''''''''''''''''''''''''

This function should be used to test whether or not a swap is required.

    **Required header/s**
        ``c_shum_byteswap.h``
        ``c_shum_byteswap_opt.h``

    **Syntax**
        ``c_shum_get_machine_endianism()``

    **Return Value**
        ``endianness (enum)``
            The native endianism of the machine. An enum taking 2 possible
            values defined in ``c_shum_byteswap.h``; ``bigEndian`` and 
            ``littleEndian``. 

