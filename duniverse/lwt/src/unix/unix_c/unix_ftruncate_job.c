/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



/* Informations:

   - this is the expected prototype of the C function [ftruncate]:

       int ftruncate(int fd, off_t offset)

   - these are the expected ocaml externals for this job:

       external ftruncate_job : Unix.file_descr -> int -> unit Lwt_unix.job = "lwt_unix_ftruncate_job"
       external ftruncate_sync : Unix.file_descr -> int -> unit = "lwt_unix_ftruncate_sync"
*/

/* Caml headers. */
#include <lwt_unix.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>

#if !defined(LWT_ON_WINDOWS)

/* Specific headers. */
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

/* +-----------------------------------------------------------------+
   | Asynchronous job                                                |
   +-----------------------------------------------------------------+ */

/* Structure holding informations for calling [ftruncate]. */
struct job_ftruncate {
  /* Informations used by lwt. It must be the first field of the structure. */
  struct lwt_unix_job job;
  /* This field store the result of the call. */
  int result;
  /* This field store the value of [errno] after the call. */
  int errno_copy;
  /* in parameter. */
  int fd;
  /* in parameter. */
  off_t offset;
};

/* The function calling [ftruncate]. */
static void worker_ftruncate(struct job_ftruncate* job)
{
  /* Perform the blocking call. */
  job->result = ftruncate(job->fd, job->offset);
  /* Save the value of errno. */
  job->errno_copy = errno;
}

/* The function building the caml result. */
static value result_ftruncate(struct job_ftruncate* job)
{
  /* Check for errors. */
  if (job->result < 0) {
    /* Save the value of errno so we can use it once the job has been freed. */
    int error = job->errno_copy;
    /* Free the job structure. */
    lwt_unix_free_job(&job->job);
    /* Raise the error. */
    unix_error(error, "ftruncate", Nothing);
  }
  /* Free the job structure. */
  lwt_unix_free_job(&job->job);
  /* Return the result. */
  return Val_unit;
}

/* The stub creating the job structure. */
CAMLprim value lwt_unix_ftruncate_job(value fd, value offset)
{
  /* Allocate a new job. */
  struct job_ftruncate* job = lwt_unix_new(struct job_ftruncate);
  /* Initializes function fields. */
  job->job.worker = (lwt_unix_job_worker)worker_ftruncate;
  job->job.result = (lwt_unix_job_result)result_ftruncate;
  /* Copy the fd parameter. */
  job->fd = Int_val(fd);
  /* Copy the offset parameter. */
  job->offset = Long_val(offset);
  /* Wrap the structure into a caml value. */
  return lwt_unix_alloc_job(&job->job);
}

/* The stub creating the job structure. */
CAMLprim value lwt_unix_ftruncate_64_job(value fd, value offset)
{
  /* Allocate a new job. */
  struct job_ftruncate* job = lwt_unix_new(struct job_ftruncate);
  /* Initializes function fields. */
  job->job.worker = (lwt_unix_job_worker)worker_ftruncate;
  job->job.result = (lwt_unix_job_result)result_ftruncate;
  /* Copy the fd parameter. */
  job->fd = Int_val(fd);
  /* Copy the offset parameter. */
  job->offset = Int64_val(offset);
  /* Wrap the structure into a caml value. */
  return lwt_unix_alloc_job(&job->job);
}

#else /* !defined(LWT_ON_WINDOWS) */

CAMLprim value lwt_unix_ftruncate_job(value Unit)
{
  lwt_unix_not_available("ftruncate");
  return Val_unit;
}

CAMLprim value lwt_unix_ftruncate_64_job(value Unit)
{
  lwt_unix_not_available("ftruncate");
  return Val_unit;
}

#endif /* !defined(LWT_ON_WINDOWS) */
