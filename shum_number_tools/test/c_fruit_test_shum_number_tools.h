/* *********************************COPYRIGHT**********************************/
/* (C) Crown copyright Met Office. All rights reserved.                       */
/* For further details please refer to the file LICENCE.txt                   */
/* which you should have received as part of this distribution.               */
/* *********************************COPYRIGHT**********************************/
/*                                                                            */
/* This file is part of the UM Shared Library project.                        */
/*                                                                            */
/* The UM Shared Library is free software: you can redistribute it            */
/* and/or modify it under the terms of the Modified BSD License, as           */
/* published by the Open Source Initiative.                                   */
/*                                                                            */
/* The UM Shared Library is distributed in the hope that it will be           */
/* useful, but WITHOUT ANY WARRANTY; without even the implied warranty        */
/* of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           */
/* Modified BSD License for more details.                                     */
/*                                                                            */
/* You should have received a copy of the Modified BSD License                */
/* along with the UM Shared Library.                                          */
/* If not, see <http://opensource.org/licenses/BSD-3-Clause>.                 */
/******************************************************************************/

#if !defined(C_FRUIT_TEST_SHUM_NUMBER_TOOLS_H)

#define C_FRUIT_TEST_SHUM_NUMBER_TOOLS_H

/******************************************************************************/
/* Prototypes                                                                 */
/******************************************************************************/

extern float  c_test_generate_finf       (void);
extern double c_test_generate_dinf       (void);
extern float  c_test_generate_fnan       (void);
extern double c_test_generate_dnan       (void);
extern void   c_test_generate_fdenormal  (float *);
extern void   c_test_generate_ddenormal  (double *);

#endif
