/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#if !defined(LWT_ON_WINDOWS)

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <dirent.h>
#include <sys/types.h>

#include "lwt_unix.h"

CAMLprim value lwt_unix_invalidate_dir(value dir)
{
    CAMLparam1(dir);
    DIR_Val(dir) = NULL;
    CAMLreturn(Val_unit);
}
#endif
