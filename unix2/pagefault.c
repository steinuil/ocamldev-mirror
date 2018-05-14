////////////////////////////////////////////////////////////////
// Copyright (C) 1998 PerDiS Consortium
//
// This file is part of the PerDiS system.
//
// PerDiS is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library  General Public
// License as published by the Free Software Foundation; either
// version 2 of the License, or (at your option) any later
// version.
//
// PerDiS is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE. See
// the GNU Library General Public License for more details.
//
// You should have received a copy of the GNU Library General
// Public  License along with PerDiS; if not, write to
// the Free Software Foundation, Inc., 59 Temple Place -
// Suite 330, Boston, MA 02111-1307, USA.
//
// You can contact the authors by email at 
// <info@perdis.esprit.ec.org>.
//


//
// Page Fault Handler Module
// Provides services to handle page faults
// XB 20-Mar-98
//


#include <stdio.h>
#include <errno.h>
#include <signal.h>
//#include <mmgr/mmgr.h>
//#include <mmgr/pfh.h>
//#include <dirty/dirty.h>

//#include <sys/mman.h>


//
// locks are performed through the explicit interface
//extern "C" {
//    #include <pds_api.h>
//	   }





// Yo, I love HP/UX!!!
#ifdef __hpux
#define cma_sigaction sigaction
#endif

// FIXME: another ugly hack (macro defined twice),
// but how can I define this???
// setting the last error
#define set_error(r,e) _last_error = e


//
// handle to the memory manager
//
//extern mmgr memory_manager ;
//extern dirty dirty_module ;


//
// signal handler code
//

#ifdef __linux
#include <asm/sigcontext.h>
void pagefault_handler (int sig, struct sigcontext_struct s_context)
#endif

#if defined (__sun) || defined (__hpux) || defined (__alpha)
void pagefault_handler (int,
			siginfo_t *info)
#endif

#ifdef _AIX
void pagefault_handler (int, int,
			struct sigcontext *ctx)
#endif

{
    char *base_addr ;
    long page ;
    char *flt_addr ; // to be readable from gdb
//    intent_type it ;
    char *start; int size ; // of the range


#ifdef __linux
    flt_addr = (char*) s_context.cr2 ; 
#endif

#if defined (sun) || defined (__hpux) || defined (__alpha)
    if (info == NULL) {
      cout << "*** Fatal error. Aborting\n"
	   << "       Undocumented fault type\n" ;
      exit (-1) ;
    }
    flt_addr = info->si_addr ;
#endif

    // OF: 23 Mar 98 - AIX support

#ifdef _AIX
    flt_addr = (char*)ctx->sc_jmpbuf.jmp_context.o_vaddr;
#endif

    fprintf(stderr, "PAGE FAULT AT %lx\n", flt_addr);
} // pagefault_handler ()




//
// enabling/disabling page fault handling
//

static int pfh_enabled = 0 ; 
static struct sigaction old_action ;

int enable_pagefault_handling () {

    struct sigaction action ;

    if (pfh_enabled)
	// already enabled
	// return an error
	return -1 ;
    
#ifdef __sun
    if ( sigaction (SIGSEGV,
		    NULL,
		    &old_action) < 0 )
    {
	cout << "*** Fatal error---aborting\n" ;
	perror ("sigaction") ;
	abort () ;
    }

    action = old_action ;
    action.sa_handler = (void (*)(int))pagefault_handler ;
    action.sa_flags |= SA_SIGINFO ;

    if ( sigaction (SIGSEGV,
		    &action,
		    NULL) < 0 )
    {
	cout << "*** Fatal error---aborting\n" ;
	perror ("sigaction") ;
	abort () ;
    }
#endif

#ifdef __hpux
    if ( sigaction (SIGBUS,
		    NULL,
		    &old_action) < 0 )
    {
	cout << "*** Fatal error---aborting\n" ;
	perror ("sigaction") ;
	abort () ;
    }

    action = old_action ;
    action.sa_handler = 
      (void(*)(int))pagefault_handler ;
    action.sa_flags |= SA_SIGINFO ;

    if ( sigaction (SIGBUS,
		    &action,
		    NULL) < 0 )
    {
	cout << "*** Fatal error---aborting\n" ;
	perror ("sigaction") ;
	abort () ;
    }
#endif

    // OF: 23 Mar 98 - AIX support
#if defined(__linux) || defined(_AIX)
    if ( sigaction (SIGSEGV,
		    NULL,
		    &old_action) < 0 )
    {
	printf( "*** Fatal error---aborting\n") ;
	perror ("sigaction (get old)") ;
	exit (2) ;
    }

    action = old_action ;
    action.sa_handler = (void (*)(int))pagefault_handler ;

    if ( sigaction (SIGSEGV,
		    &action,
		    NULL) < 0 )
    {
	printf( "*** Fatal error---aborting\n") ;
	perror ("sigaction (set new)") ;
	exit(2) ;
    }
#endif

    pfh_enabled = 1 ;
    
    return 1 ;
}    


int disable_pagefault_handling () {

    if (!pfh_enabled)
	// not enabled
	// return an error
	return -1 ;

    if ( sigaction (SIGSEGV,
		    &old_action,
		    NULL) < 0 )
    {
	printf( "*** Fatal error---aborting\n") ;
	perror ("sigaction") ;
	exit(2) ;
    }

    pfh_enabled = 0 ;

    return 1 ;
}


int main()
{
  char *s = 0x4468744;

   enable_pagefault_handling();

   s[0] = 12;
  return 0;
}
