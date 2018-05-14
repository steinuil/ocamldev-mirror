/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id: getdomainname.c,v 1.1 2001/05/21 07:39:47 lefessan Exp $ */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#if defined (_WIN32)
#include <winsock.h>
#elif !macintosh
#include <sys/param.h>
#endif

#include "unixsupport.h"

value unix_getdomainname(value unit)         /* ML */
{
  char name[MAXHOSTNAMELEN];
  getdomainname(name, MAXHOSTNAMELEN);
  name[MAXHOSTNAMELEN-1] = 0;
  return copy_string(name);
}
