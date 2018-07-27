API Reference: shum_latlon_eq_grids
-----------------------------------

Fortran Functions/Subroutines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

``get_shum_latlon_eq_grids_version``
''''''''''''''''''''''''''''''''''''

All Shumlib libraries expose a module and function named in this format; it 
allows access to the Shumlib version number used when compiling the library.

    **Available via module**
        ``f_shum_latlon_eq_grids_version_mod``

    **Syntax**
        ``version = get_shum_latlon_eq_grids_version()``

    **Returns**
        ``version (INTEGER)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).


``f_shum_latlon_to_eq``
'''''''''''''''''''''''

This function calculates latitude & longitude on equatorial latitude-longitude 
(eq) grid defined by a translated and/or rotated pole.
It is used in regional models from input arrays of latitude & longitude on
standard grid. Both input and output latitudes & longitudes are in degrees.

    **Available via module**
        ``f_shum_latlon_eq_grids_mod``

    **Syntax**
        ``status = f_shum_latlon_to_eq(phi, lambda, phi_eq, lambda_eq, phi_pole, lambda_pole, message)``

    **Inputs**
        ``phi, lambda (REAL, 64- or 32-bit)``
            Standard latitude & longitude (degrees) (1D array or scalar). 
            All arrays are dimensioned using the SIZE of the first input array.
        ``phi_pole, lambda_pole (REAL, 64- or 32-bit)``
            Latitude & longitude of rotated pole (degrees).
    
    **Outputs**
        ``phi_eq (REAL, 64- or 32-bit)``
            Latitude on equatorial (rotated) grid (degrees) (1D array or scalar).
        ``lambda_eq (REAL, 64- or 32-bit)``
            Longitude on equatorial (rotated) grid (degrees) (1D array or scalar).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred, in which case the ``message`` argument will contain 
            information about the problem.

    **Notes**
        The arguments here must be either *all* 32-bit or *all* 64-bit 
        (but *not* a mixture of the two).


``f_shum_eq_to_latlon``
'''''''''''''''''''''''

This function calculates latitude & longitude on standard grid from values of
latitude & longitude on equatorial (eq) grid defined by a translated and/or 
rotated pole as used in regional models.
Both input and output latitudes & longitudes are in degrees.

    **Available via module**
        ``f_shum_latlon_eq_grids_mod``

    **Syntax**
        ``status = f_shum_eq_to_latlon(phi_eq, lambda_eq, phi, lambda, phi_pole, lambda_pole, message)``

    **Inputs**
        ``phi_eq, lambda_eq (REAL, 64- or 32-bit)``
            Latitude & longitude on equatorial (rotated) grid (1D array or scalar). 
            All arrays are dimensioned using the SIZE of the first input array.
        ``phi_pole, lambda_pole (REAL, 64- or 32-bit)``
            Latitude & longitude of rotated pole (degrees).

    **Outputs**
        ``phi (REAL, 64- or 32-bit)``
            Latitude on standard grid (degrees) (1D array or scalar).
        ``lambda (REAL, 64- or 32-bit)``
            Longitude on standard grid (degrees) (1D array or scalar).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred, in which case the ``message`` argument will contain
            information about the problem.

    **Notes**
        The arguments here must be either *all* 32-bit or *all* 64-bit
        (but *not* a mixture of the two).


``f_shum_latlon_eq_vector_coeff``
'''''''''''''''''''''''''''''''''

This function calculates pairs of coefficients needed to translate u and v 
vector components of wind between equatorial (eq) latitude-longitude grid and 
standard (ll) latitude-longitude grid (or vice versa).
Input latitudes & longitudes are in degrees.
It can in principle be applied to any pair of vector components.

    **Available via module**
        ``f_shum_latlon_eq_grids_mod``

    **Syntax**
        ``status = f_shum_latlon_eq_vector_coeff(coeff1, coeff2, lambda, lambda_eq, phi_pole, lambda_pole, message)``

    **Inputs**
        ``lambda (REAL, 64 or 32-bit)``
            Longitudes on standard lat-lon grid (1D array). 
            All arrays are dimensioned using the SIZE of the first input array.
        ``lambda_eq (REAL, 64 or 32-bit)`` 
            Longitudes on equatorial lat-lon grid (1D array).
        ``phi_pole (REAL, 64 or 32-bit)``
            Latitude of pole of equatorial (rotated) grid (1 value).
        ``lambda_pole (REAL, 64 or 32-bit)``
            Longitude of pole of equatorial (rotated) grid (1 value).

    **Outputs**
        ``coeff1, coeff2 (REAL), 64 or 32-bit``
            Pairs of rotation coefficients (1D arrays).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred, in which case the ``message`` argument will contain 
            information about the problem.

    **Notes**
        The arguments here may be either *all* 32-bit or *all* 64-bit 
        (but *not* a mixture of the two).
        These coeff1, coeff2 must be calculated for this rotation set 
        (lambda, lambda_eq, phi_pole, lambda_pole) before the vector rotation
        functions ``f_shum_latlon_to_eq_vector`` 
        or ``f_shum_eq_to_latlon_vector`` are used.


``f_shum_eq_to_latlon_vector``
''''''''''''''''''''''''''''''

This function calculates u & v vector components of wind on standard 
latitude-longitude (ll) grid by rotating wind components on equatorial 
latitude-longitude (eq) grid.
It can in principle be applied to any pair of vector components.

    **Available via module**
        ``f_shum_latlon_eq_grids_mod``

    **Syntax**
        ``status = f_shum_eq_to_latlon_vector(coeff1, coeff2, u_eq, v_eq, u, v, message, mdi)``

    **Inputs**
        ``coeff1, coeff2 (REAL, 64- or 32-bit)``
            Pairs of rotation coefficients (1D arrays). 
            All arrays are dimensioned using the SIZE of the first input array. 
            N.B. These must have been calculated or saved for THIS rotation set
            (lambda, lambda_eq, phi_pole, lambda_pole) prior to using this function.
        ``u_eq (REAL, 64- or 32-bit)``
            u vector component on equatorial lat-lon grid (1D array).
        ``v_eq (REAL, 64- or 32-bit)``
            v vector component on equatorial lat-lon grid (1D array).
        ``mdi (REAL, OPTIONAL, 64- or 32-bit)``
            Missing data indicator value; any values in either input field 
            which have this value will be output unrotated as missing data.

    **Outputs**
        ``u (REAL, 64- or 32-bit)``
            u vector component rotated to standard lat-lon grid (1D array).
        ``v (REAL, 64- or 32-bit)``
            v vector component rotated to standard lat-lon grid (1D array).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred, in which case the ``message`` argument will contain 
            information about the problem.

    **Notes**
        The arguments here may be either *all* 32-bit or *all* 64-bit 
        (but *not* a mixture of the two). 
        Reminder: The rotation coefficients must have been calculated or saved
        for THIS rotation set (lambda, lambda_eq, phi_pole, lambda_pole) 
        prior to using this function.


``f_shum_latlon_to_eq_vector``
''''''''''''''''''''''''''''''

This function calculates u & v vector components of wind on equatorial (rotated)
latitude-longitude (eq) grid by rotating wind components on standard 
latitude-longitude (ll) grid.
It can in principle be applied to any pair of vector components.

    **Available via module**
        ``f_shum_latlon_eq_grids_mod``

    **Syntax**
        ``status = f_shum_latlon_to_eq_vector(coeff1, coeff2, u, v, u_eq, v_eq, message, mdi)``

    **Inputs**
        ``coeff1, coeff2 (REAL, 64- or 32-bit)``
            Pairs of rotation coefficients (1D arrays). 
            All arrays are dimensioned using the SIZE of the first input array. 
            N.B. These must have been calculated or saved for THIS rotation set
            (lambda, lambda_eq, phi_pole, lambda_pole) prior to using this function.
        ``u (REAL, 64- or 32-bit)``
            u vector component on standard lat-lon grid (1D array).
        ``v (REAL, 32- or 64-bit)``
            v vector component on standard lat-lon grid (1D array).
        ``mdi (REAL, OPTIONAL, 64- or 32-bit)``
            Missing data indicator value; any values in either input field 
            which have this value will be output unrotated as missing data.

    **Outputs**
        ``u_eq (REAL, 64- or 32-bit)``
            u vector component rotated to equatorial lat-lon grid (1D array).
        ``v_eq (REAL, 64- or 32-bit)``
            v vector component rotated to equatorial lat-lon grid (1D array).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred, in which case the ``message`` argument will contain
            information about the problem.

    **Notes**
        The arguments here may be either *all* 32-bit or *all* 64-bit
        (but *not* a mixture of the two).
        Reminder: The rotation coefficients must have been calculated or saved
        for THIS rotation set (lambda, lambda_eq, phi_pole, lambda_pole) 
        prior to using this function.


C Functions
%%%%%%%%%%%

These grid transformations are currently only available as FORTRAN functions. 

Unified Model Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

These 5 shumlib functions replace the 5 control/grids lat-lon to equatorial 
grid transformation subroutines ``lltoeq, eqtoll, w_coeff, w_eqtoll & 
w_lltoeq`` respectively. 

In the Unified Model the shumlib functions are invoked through a new set of 
subroutine wrappers in module ``control/grids/latlon_eq_rotation_mod.F90`` 
named ``rotate_latlon_to_eq, rotate_eq_to_latlon, eq_latlon_vector_coeffs, 
vector_eq_to_latlon & vector_latlon_to_eq``. 

E.g. 
``USE latlon_eq_rotation_mod, ONLY: rotate_eq_to_latlon, rotate_latlon_to_eq``
while the wrapper accesses shumlib via 
``USE f_shum_latlon_eq_grids_mod, ONLY: f_shum_latlon_to_eq`` etc.

